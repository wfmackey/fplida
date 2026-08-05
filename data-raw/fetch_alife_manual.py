#!/usr/bin/env python3
"""Fetch ALife variable metadata and manual Markdown.

The ALife research app is an Angular app backed by static assets. The variable
index is at ``/assets/data/variables.json`` and each manual entry is at
``/assets/manual/{source}/{name}.md``. Missing manual files return the Angular
HTML shell with HTTP 200, so this script detects that fallback by content.
"""

from __future__ import annotations

import argparse
import csv
import json
import re
import sys
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass
from pathlib import Path
from typing import Any
from urllib.error import HTTPError, URLError
from urllib.parse import quote, urljoin
from urllib.request import Request, urlopen


BASE_URL = "https://alife-research.app/"
SECTION_RE = re.compile(r"<br>\s*\*\*(?P<label>.+?)\*\*:\s*", re.DOTALL)
TITLE_RE = re.compile(r"^###\s+(?P<title>.+?)\s*$", re.MULTILINE)
USER_AGENT = "fplida-alife-manual-fetcher/1.0"


@dataclass(frozen=True)
class ManualFetch:
    variable_id: str
    source: str
    name: str
    url: str
    found: bool
    text: str
    error: str = ""


def fetch_text(url: str, timeout: int) -> str:
    request = Request(url, headers={"User-Agent": USER_AGENT})
    with urlopen(request, timeout=timeout) as response:
        data = response.read()
    return data.decode("utf-8-sig")


def fetch_text_with_retries(url: str, timeout: int, retries: int) -> str:
    last_error: Exception | None = None
    for attempt in range(retries + 1):
        try:
            return fetch_text(url, timeout)
        except (HTTPError, URLError, TimeoutError, OSError) as exc:
            last_error = exc
            if attempt < retries:
                time.sleep(min(2**attempt, 8))
    if last_error is None:
        raise RuntimeError(f"Could not fetch {url}")
    raise last_error


def looks_like_html_shell(text: str) -> bool:
    prefix = text.lstrip()[:200].lower()
    return prefix.startswith("<!doctype html") or prefix.startswith("<html")


def clean_text(value: str) -> str:
    value = value.replace("\r\n", "\n").replace("\r", "\n")
    value = value.replace("\xa0", " ")
    value = value.replace("\\_", "_")
    value = re.sub(r"[ \t]+\n", "\n", value)
    value = re.sub(r"\n{3,}", "\n\n", value)
    return value.strip()


def clean_label(value: str) -> str:
    value = clean_text(value)
    return re.sub(r"\s+", " ", value)


def parse_manual(text: str) -> tuple[str, dict[str, str]]:
    title_match = TITLE_RE.search(text)
    title = clean_text(title_match.group("title")) if title_match else ""

    sections: dict[str, str] = {}
    matches = list(SECTION_RE.finditer(text))
    for index, match in enumerate(matches):
        label = clean_label(match.group("label"))
        body_start = match.end()
        body_end = matches[index + 1].start() if index + 1 < len(matches) else len(text)
        sections[label] = clean_text(text[body_start:body_end])
    return title, sections


def manual_url(base_url: str, variable: dict[str, Any]) -> str:
    path = "assets/manual/{}/{}.md".format(
        quote(variable["Source"], safe=""),
        quote(variable["Name"], safe=""),
    )
    return urljoin(base_url, path)


def fetch_manual(
    base_url: str,
    variable: dict[str, Any],
    manual_dir: Path,
    timeout: int,
    force: bool,
    retries: int,
) -> ManualFetch:
    source = variable["Source"]
    name = variable["Name"]
    variable_id = variable["Id"]
    url = manual_url(base_url, variable)
    out_path = manual_dir / source / f"{name}.md"

    if out_path.exists() and not force:
        text = out_path.read_text(encoding="utf-8")
        return ManualFetch(variable_id, source, name, url, True, text)

    last_error = ""
    for attempt in range(retries + 1):
        try:
            text = fetch_text(url, timeout)
            if looks_like_html_shell(text):
                return ManualFetch(variable_id, source, name, url, False, "", "missing")

            out_path.parent.mkdir(parents=True, exist_ok=True)
            out_path.write_text(text, encoding="utf-8")
            return ManualFetch(variable_id, source, name, url, True, text)
        except (HTTPError, URLError, TimeoutError, OSError) as exc:
            last_error = str(exc)
            if attempt < retries:
                time.sleep(min(2**attempt, 8))

    return ManualFetch(variable_id, source, name, url, False, "", last_error)


def years_available(variable: dict[str, Any]) -> str:
    years = [
        str(item["Year"])
        for item in variable.get("YearIndex", [])
        if item.get("Exists")
    ]
    return ",".join(years)


def variable_base_row(variable: dict[str, Any], manual: ManualFetch) -> dict[str, str]:
    return {
        "id": variable.get("Id", ""),
        "name": variable.get("Name", ""),
        "source": variable.get("Source", ""),
        "var_type": variable.get("VarType", ""),
        "description": variable.get("Description") or "",
        "notes": variable.get("Notes") or "",
        "translation": variable.get("Translation") or "",
        "tags": ",".join(variable.get("Tags", []) or []),
        "related_vars": ",".join(variable.get("RelatedVars", []) or []),
        "available_years": years_available(variable),
        "form_pages_json": json.dumps(variable.get("FormPages", []), ensure_ascii=False),
        "year_index_json": json.dumps(variable.get("YearIndex", []), ensure_ascii=False),
        "manual_url": manual.url,
        "manual_found": str(manual.found).lower(),
        "manual_error": manual.error,
    }


def write_csv(
    variables: list[dict[str, Any]],
    manuals: dict[str, ManualFetch],
    out_csv: Path,
) -> tuple[int, int, list[str]]:
    parsed: dict[str, tuple[str, dict[str, str]]] = {}
    section_names: list[str] = []
    seen_sections: set[str] = set()

    for variable in variables:
        manual = manuals[variable["Id"]]
        if manual.found:
            title, sections = parse_manual(manual.text)
            parsed[variable["Id"]] = (title, sections)
            for section in sections:
                if section not in seen_sections:
                    section_names.append(section)
                    seen_sections.add(section)
        else:
            parsed[variable["Id"]] = ("", {})

    fieldnames = [
        "id",
        "name",
        "source",
        "var_type",
        "description",
        "notes",
        "translation",
        "tags",
        "related_vars",
        "available_years",
        "form_pages_json",
        "year_index_json",
        "manual_url",
        "manual_found",
        "manual_error",
        "manual_title",
        "manual_markdown",
        *section_names,
    ]

    out_csv.parent.mkdir(parents=True, exist_ok=True)
    with out_csv.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames, lineterminator="\n")
        writer.writeheader()
        for variable in variables:
            manual = manuals[variable["Id"]]
            title, sections = parsed[variable["Id"]]
            row = variable_base_row(variable, manual)
            row["manual_title"] = title
            row["manual_markdown"] = clean_text(manual.text) if manual.found else ""
            row.update(sections)
            writer.writerow(row)

    found = sum(1 for manual in manuals.values() if manual.found)
    missing = len(manuals) - found
    return found, missing, section_names


def write_manual_status(
    manuals: dict[str, ManualFetch],
    out_csv: Path,
    include_error: str | None = None,
) -> None:
    out_csv.parent.mkdir(parents=True, exist_ok=True)
    with out_csv.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(
            handle,
            fieldnames=["id", "source", "name", "manual_url", "manual_error"],
            lineterminator="\n",
        )
        writer.writeheader()
        for manual in manuals.values():
            if not manual.found and (
                include_error is None or manual.error == include_error
            ):
                writer.writerow(
                    {
                        "id": manual.variable_id,
                        "source": manual.source,
                        "name": manual.name,
                        "manual_url": manual.url,
                        "manual_error": manual.error,
                    }
                )


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--base-url", default=BASE_URL)
    parser.add_argument("--raw-dir", default="data-raw/alife_manual")
    parser.add_argument(
        "--out-csv",
        default="inst/plida_metadata/alife_variable_manual.csv",
        help="Parsed variable manual output CSV.",
    )
    parser.add_argument(
        "--missing-csv",
        default="data-raw/alife_manual/missing_manuals.csv",
        help="CSV of variables where ALife returned the HTML fallback.",
    )
    parser.add_argument(
        "--unresolved-csv",
        default="data-raw/alife_manual/unresolved_manuals.csv",
        help="CSV of variables whose manual Markdown was not fetched.",
    )
    parser.add_argument("--workers", type=int, default=8)
    parser.add_argument("--timeout", type=int, default=30)
    parser.add_argument("--retries", type=int, default=3)
    parser.add_argument("--limit", type=int, help="Fetch only the first N variables.")
    parser.add_argument(
        "--force",
        action="store_true",
        help="Refetch manual Markdown even when a cached file exists.",
    )
    args = parser.parse_args()

    base_url = args.base_url.rstrip("/") + "/"
    raw_dir = Path(args.raw_dir)
    manual_dir = raw_dir / "manual"
    variables_path = raw_dir / "variables.json"

    raw_dir.mkdir(parents=True, exist_ok=True)
    variables_url = urljoin(base_url, "assets/data/variables.json")
    try:
        variables_text = fetch_text_with_retries(variables_url, args.timeout, args.retries)
    except (HTTPError, URLError, TimeoutError, OSError) as exc:
        if not variables_path.exists():
            raise
        print(
            f"Could not refresh {variables_url}; using cached {variables_path}: {exc}",
            file=sys.stderr,
        )
        variables_text = variables_path.read_text(encoding="utf-8-sig")

    all_variables = json.loads(variables_text)
    variables_path.write_text(
        json.dumps(all_variables, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )
    variables = all_variables[: args.limit] if args.limit else all_variables

    manuals: dict[str, ManualFetch] = {}
    with ThreadPoolExecutor(max_workers=args.workers) as executor:
        futures = {
            executor.submit(
                fetch_manual,
                base_url,
                variable,
                manual_dir,
                args.timeout,
                args.force,
                args.retries,
            ): variable
            for variable in variables
        }
        for index, future in enumerate(as_completed(futures), start=1):
            manual = future.result()
            manuals[manual.variable_id] = manual
            if index % 50 == 0 or index == len(futures):
                print(f"Fetched {index}/{len(futures)} manuals", file=sys.stderr)

    found, unresolved, section_names = write_csv(variables, manuals, Path(args.out_csv))
    write_manual_status(manuals, Path(args.unresolved_csv))
    write_manual_status(manuals, Path(args.missing_csv), include_error="missing")

    print(
        "ALife manual extraction complete: "
        f"{len(variables)} variables, {found} manuals found, "
        f"{unresolved} unresolved, {len(section_names)} parsed section columns."
    )
    print(f"Wrote {args.out_csv}")
    print(f"Wrote {args.unresolved_csv}")
    print(f"Wrote {args.missing_csv}")
    print(f"Wrote raw variable index to {variables_path}")
    print(f"Wrote raw manuals to {manual_dir}")


if __name__ == "__main__":
    main()
