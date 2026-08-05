# Small administrative products with one unresolved dataset-specific field.

.dil_ers_source_value <- function(name, description, source_frame,
                                  spine_rows, seed, period) {
  if (toupper(name) != "REQUEST_CATEGORY") return(NULL)
  # The bundled ERS product models the temporary 2020 COVID-19 early-release
  # program. No public PLIDA codeframe was found for this internal category,
  # so use an explicit local label instead of an invented numeric code.
  rep("COVID-19 early release", nrow(spine_rows))
}
