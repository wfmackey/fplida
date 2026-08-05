# Explicit source adapters for canonical DIL tables.
#
# The canonical materialiser must not guess between generated outputs. These
# adapters list the datasets where an exact product output represents every
# table in that product, and the small number of generators whose native
# output name differs from the DIL product or table name.

.dil_multi_table_product_sources <- c(
  "A&T", "AIR", "APSED", "BIRTHS", "COMBINED", "DEATHS", "JK", "MCD",
  "MT_DEMOGS", "NACDC", "NDIS", "SDB", "TVA", "VISA"
)

.dil_has_multi_table_product_source <- function(dataset) {
  dataset %in% .dil_multi_table_product_sources
}

.dil_structure_source_aliases <- function(dataset, product_name, table_name) {
  aliases <- character()

  if (identical(dataset, "CGT") && grepl(
    "^pmp-cgt-[0-9]{4}-[0-9]{2}$", product_name
  )) {
    aliases <- c(
      aliases,
      sub(
        "^pmp-cgt-[0-9]{2}([0-9]{2})-([0-9]{2})$",
        "pmp-cgt-\\1\\2",
        product_name
      )
    )
  }

  if (identical(dataset, "DEATHS") && grepl(
    "-[0-9]{4}$", product_name
  )) {
    year <- sub("^.*-([0-9]{4})$", "\\1", product_name)
    aliases <- c(
      aliases,
      paste0("madipge-death-d-cause-of-death-", year)
    )
  }

  if (identical(dataset, "HE")) {
    table_type <- sub("^hes_madip_student_", "", table_name)
    if (table_type %in% c(
      "enrol", "course", "load", "completions", "help"
    )) {
      aliases <- c(aliases, paste0("madipge-hied-student-", table_type))
    }
  }

  if (identical(dataset, "DOMINO")) {
    aliases <- c(
      aliases,
      paste0(product_name, "--", gsub("_", "-", table_name, fixed = TRUE))
    )
  }

  if (identical(dataset, "CORE") && grepl(
    "^plidage-core-(?:demog-cb-c21|locat-cb|relat-cb-c21)-",
    product_name, perl = TRUE
  )) {
    # The compact CORE generator supplies these three products. They are not
    # generic sources for other CORE products or Census vintages.
    aliases <- c(aliases, product_name)
  }

  unique(aliases[nzchar(aliases)])
}

.dil_canonical_structure_outputs <- function(structures) {
  if (!nrow(structures)) return(character())
  vapply(seq_len(nrow(structures)), function(i) {
    .dil_structure_stem(
      structures[["Product Name"]][[i]],
      structures[["Table Name"]][[i]]
    )
  }, character(1))
}
