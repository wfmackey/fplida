# Match native and canonical asset names to the dataset's registered agency.
dil_asset_agencies <- function(structures = fplida:::.dil_structure_inventory()$structures) {
  entries <- purrr::map_dfr(seq_len(nrow(structures)), function(i) {
    dataset <- structures$Dataset[[i]]
    product <- structures[["Product Name"]][[i]]
    table <- structures[["Table Name"]][[i]]
    agency <- fplida:::dataset_to_agency(dataset)
    if (is.null(agency)) stop("No agency registered for DIL dataset: ", dataset)
    aliases <- fplida:::.dil_structure_source_aliases(dataset, product, table)
    tibble::tibble(asset = unique(c(product, table, paste0(product, "--", table), aliases)),
                   dataset = dataset, agency = tolower(agency))
  }) |> dplyr::distinct()
  identities <- entries |> dplyr::distinct(asset, agency)
  conflicts <- identities |> dplyr::count(asset) |> dplyr::filter(n > 1L)
  if (nrow(conflicts)) stop("Ambiguous DIL agency mapping: ", paste(conflicts$asset, collapse = ", "))
  registry <- stats::setNames(identities$agency, identities$asset)
  attr(registry, "datasets") <- split(entries$dataset, entries$asset)
  registry
}

asset_dataset <- function(asset, family, registry) {
  dataset <- unique(attr(registry, "datasets")[[asset]])
  if (length(dataset) == 1L) return(dataset)
  if (identical(family, "census")) return("CENSUS")
  NA_character_
}

asset_agency <- function(asset, family, registry) {
  agency <- unname(registry[asset])
  if (length(agency) == 1L && !is.na(agency)) return(agency)
  # Rich native outputs can span years that have separate DIL product names.
  native <- c(census = "abs", pit_ps = "ato", pit_itr = "ato",
              stp = "ato", busown = "ato", domino = "dss", mbs = "dhda",
              pbs = "dhda", he = "de", tva = "ncver", visa = "ha",
              ndis = "ndia", deaths = "rbdm", lfs = "abs")
  agency <- unname(native[family])
  if (length(agency) == 1L) agency else NA_character_
}

dil_structure_coverage <- function(inventory,
                                   structures = fplida:::.dil_structure_inventory()$structures) {
  expected <- unique(paste0(structures[["Product Name"]], "--", structures[["Table Name"]]))
  missing <- setdiff(expected, inventory$asset)
  list(passed = length(expected) > 0L && !length(missing),
       expected_structures = length(expected),
       found_structures = sum(expected %in% inventory$asset), missing = missing)
}
