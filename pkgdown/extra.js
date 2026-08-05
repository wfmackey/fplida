// Variable browser for the fplida pkgdown site.
//
// Each dataset article embeds one JSON payload:
//   { domains: [[["code","label"], ...], ...],   // distinct value lists, shared
//     vars:    [{n, d, t, k, p, s, dom, def, src, url, lim, where}, ...] }
// Keys are short because the largest payload holds several thousand variables.
//
// Rows render on demand. Only the visible page is in the DOM, so a dataset with
// 4,884 variables stays responsive.

(function () {
  "use strict";

  var PAGE_SIZE = 50;

  function el(tag, cls, text) {
    var e = document.createElement(tag);
    if (cls) e.className = cls;
    if (text != null) e.textContent = text;
    return e;
  }

  function supportLabel(s) {
    if (s === "supported") return ["Supported", "ok"];
    if (s === "unsupported") return ["Unsupported", "warn"];
    return ["Not assessed", "na"];
  }

  function buildDetail(v, domains) {
    var wrap = el("div", "fp-detail");

    function row(label, value, isHtml) {
      if (value == null || value === "") return;
      var r = el("div", "fp-detail-row");
      r.appendChild(el("div", "fp-detail-key", label));
      var val = el("div", "fp-detail-val");
      if (isHtml) { val.innerHTML = value; } else { val.textContent = value; }
      r.appendChild(val);
      wrap.appendChild(r);
    }

    row("Official description", v.od);
    row("Type", v.t);
    row("Value domain", v.k);
    row("Value definition", v.def);
    row("Reference period", v.p);

    if (v.dom != null && domains[v.dom] && domains[v.dom].length) {
      var list = domains[v.dom];
      var r = el("div", "fp-detail-row");
      r.appendChild(el("div", "fp-detail-key", "Valid values"));
      var val = el("div", "fp-detail-val");
      var dl = el("dl", "fp-values");
      list.forEach(function (pair) {
        dl.appendChild(el("dt", null, pair[0]));
        dl.appendChild(el("dd", null, pair[1]));
      });
      val.appendChild(dl);
      r.appendChild(val);
      wrap.appendChild(r);
    }

    if (v.src) {
      row("Value source", v.url
        ? '<a href="' + v.url + '" rel="nofollow noopener">' + v.src + "</a>"
        : v.src, !!v.url);
    }
    row("Limitation", v.lim);
    if (v.where && v.where.length) {
      // Tolerate a single entry arriving as a bare string.
      row("Appears in", [].concat(v.where).join("; "));
    }
    return wrap;
  }

  function initTable(root) {
    var payload;
    var script = root.querySelector('script[type="application/json"]');
    try {
      payload = JSON.parse(script.textContent);
    } catch (e) {
      root.appendChild(el("p", "fp-error", "Could not load the variable list."));
      return;
    }
    var domains = payload.domains || [];
    var all = payload.vars || [];
    var shown = all;
    var page = 0;

    var controls = el("div", "fp-controls");
    var search = el("input", "fp-search");
    search.type = "search";
    search.placeholder = "Filter " + all.length + " variables by name or description";
    search.setAttribute("aria-label", "Filter variables");
    controls.appendChild(search);

    var filter = el("select", "fp-filter");
    filter.setAttribute("aria-label", "Filter by value support");
    [["", "All value support"], ["supported", "Supported"],
     ["unsupported", "Unsupported"], ["not_applicable", "Not assessed"]]
      .forEach(function (o) {
        var opt = el("option", null, o[1]);
        opt.value = o[0];
        filter.appendChild(opt);
      });
    controls.appendChild(filter);

    var count = el("span", "fp-count");
    controls.appendChild(count);
    root.appendChild(controls);

    var table = el("table", "fp-table");
    var thead = el("thead");
    var hrow = el("tr");
    ["Variable", "Description", "Type", "Value support"].forEach(function (h) {
      hrow.appendChild(el("th", null, h));
    });
    thead.appendChild(hrow);
    table.appendChild(thead);
    var tbody = el("tbody");
    table.appendChild(tbody);
    root.appendChild(table);

    var more = el("button", "fp-more", "Show more");
    more.type = "button";
    root.appendChild(more);

    function render(reset) {
      if (reset) { tbody.innerHTML = ""; page = 0; }
      var start = page * PAGE_SIZE;
      var slice = shown.slice(start, start + PAGE_SIZE);
      slice.forEach(function (v) {
        var tr = el("tr", "fp-row");
        tr.tabIndex = 0;
        tr.setAttribute("role", "button");
        tr.setAttribute("aria-expanded", "false");

        var name = el("td");
        name.appendChild(el("code", "fp-name", v.n));
        tr.appendChild(name);

        tr.appendChild(el("td", "fp-desc", v.d || ""));
        tr.appendChild(el("td", "fp-type", v.t || "—"));

        var sup = supportLabel(v.s);
        var st = el("td");
        st.appendChild(el("span", "fp-badge fp-" + sup[1], sup[0]));
        tr.appendChild(st);

        var drow = el("tr", "fp-detail-tr");
        var dcell = el("td");
        dcell.colSpan = 4;
        try {
          dcell.appendChild(buildDetail(v, domains));
        } catch (err) {
          // One malformed record must not blank the whole table.
          dcell.appendChild(el("p", "fp-error",
            "Detail unavailable for this variable."));
        }
        drow.appendChild(dcell);
        drow.hidden = true;

        function toggle() {
          drow.hidden = !drow.hidden;
          tr.setAttribute("aria-expanded", String(!drow.hidden));
          tr.classList.toggle("fp-open", !drow.hidden);
        }
        tr.addEventListener("click", toggle);
        tr.addEventListener("keydown", function (ev) {
          if (ev.key === "Enter" || ev.key === " ") { ev.preventDefault(); toggle(); }
        });

        tbody.appendChild(tr);
        tbody.appendChild(drow);
      });
      page += 1;
      var done = page * PAGE_SIZE >= shown.length;
      more.hidden = done;
      more.textContent = done ? "" :
        "Show " + Math.min(PAGE_SIZE, shown.length - page * PAGE_SIZE) + " more";
      count.textContent = shown.length === all.length
        ? all.length + " variables"
        : shown.length + " of " + all.length + " variables";
    }

    function apply() {
      var q = search.value.trim().toLowerCase();
      var f = filter.value;
      shown = all.filter(function (v) {
        if (f && v.s !== f) return false;
        if (!q) return true;
        return (v.n && v.n.toLowerCase().indexOf(q) !== -1) ||
               (v.d && v.d.toLowerCase().indexOf(q) !== -1);
      });
      render(true);
    }

    var debounce;
    search.addEventListener("input", function () {
      clearTimeout(debounce);
      debounce = setTimeout(apply, 120);
    });
    filter.addEventListener("change", apply);
    more.addEventListener("click", function () { render(false); });

    render(true);
  }

  document.addEventListener("DOMContentLoaded", function () {
    var roots = document.querySelectorAll(".fplida-vartable");
    Array.prototype.forEach.call(roots, initTable);
  });
})();
