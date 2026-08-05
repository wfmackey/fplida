#!/usr/bin/env python3
"""Update packaged PLIDA metadata from the current DIL workbook.

The workbook keeps six rows of ABS/PLIDA title text before the real table
headers. This script reads the canonical DIL sheets, normalises whitespace,
and rewrites ``inst/plida_metadata/*.csv`` for package use.
"""

from __future__ import annotations

import argparse
from pathlib import Path

import pandas as pd


SHEET_OUTPUTS = {
    "Agencies": "agencies.csv",
    "Datasets": "datasets.csv",
    "Modules": "modules.csv",
    "Products": "products.csv",
    "Variables": "variables.csv",
}


def clean_frame(frame: pd.DataFrame) -> pd.DataFrame:
    """Drop blank rows and trim string cells without changing column names."""
    frame = frame.dropna(how="all").copy()
    for col in frame.columns:
        if frame[col].dtype == object:
            frame[col] = frame[col].map(
                lambda value: value.strip() if isinstance(value, str) else value
            )
    return frame


def read_dil(path: Path) -> dict[str, pd.DataFrame]:
    return {
        sheet: clean_frame(pd.read_excel(path, sheet_name=sheet, header=6))
        for sheet in SHEET_OUTPUTS
    }


def write_metadata(frames: dict[str, pd.DataFrame], out_dir: Path) -> None:
    out_dir.mkdir(parents=True, exist_ok=True)
    for sheet, filename in SHEET_OUTPUTS.items():
        frames[sheet].to_csv(out_dir / filename, index=False)


def write_audit(frames: dict[str, pd.DataFrame], out_path: Path) -> None:
    products = frames["Products"]
    variables = frames["Variables"]
    datasets = frames["Datasets"].rename(columns={"Dataset Acronym": "Dataset"})

    product_counts = (
        products.groupby("Dataset", dropna=False)
        .agg(
            n_products=("Product Name", "nunique"),
            n_modules=("Module Name", "nunique"),
        )
        .reset_index()
    )
    variable_counts = (
        variables.groupby("Dataset", dropna=False)
        .agg(
            n_variable_rows=("Variable Name", "size"),
            n_variables=("Variable Name", "nunique"),
            n_tables=("Table Name", "nunique"),
        )
        .reset_index()
    )
    dataset_info = datasets[
        [
            "Dataset",
            "Supplier",
            "Custodian",
            "Dataset Name",
            "Reference Period",
            "Update Frequency",
        ]
    ].drop_duplicates()

    audit = (
        dataset_info.merge(product_counts, on="Dataset", how="outer")
        .merge(variable_counts, on="Dataset", how="outer")
        .sort_values(["Dataset", "Dataset Name"], na_position="last")
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    audit.to_csv(out_path, index=False)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--input",
        default="data-raw/PLIDA data item list - 19MAR2026.xlsx",
        help="Path to PLIDA DIL workbook.",
    )
    parser.add_argument(
        "--metadata-dir",
        default="inst/plida_metadata",
        help="Package metadata output directory.",
    )
    parser.add_argument(
        "--audit",
        default="data-raw/dil_coverage_2026-03-19.csv",
        help="Dataset-level DIL audit CSV path.",
    )
    args = parser.parse_args()

    frames = read_dil(Path(args.input))
    write_metadata(frames, Path(args.metadata_dir))
    write_audit(frames, Path(args.audit))

    print(
        "Updated PLIDA metadata: "
        f"{len(frames['Datasets'])} dataset rows, "
        f"{len(frames['Products'])} products, "
        f"{len(frames['Variables'])} variable rows."
    )


if __name__ == "__main__":
    main()
