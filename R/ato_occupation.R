# ATO salary and wage occupation codes.
#
# The occupation field on an individual income tax return is not ANZSCO.
# Taxpayers choose a six-digit code from the ATO's own published list at
# question 1. The two classifications overlap heavily but not completely: of the
# 1,167 ATO codes for 2025-26, 951 are also valid ANZSCO 2019 codes and 216 are
# not.
#
# fplida gives each person an ANZSCO occupation on the shared spine, so the ATO
# products map that to a real ATO code through the bundled crosswalk. The
# person's occupation stays consistent across products; only the code system
# differs, which is what happens in the real data.
#
# Regenerate the bundled files with data-raw/update_ato_occupation_codes.R.


#' Cached ANZSCO -> ATO occupation code crosswalk
#'
#' @return Named integer vector: names are ANZSCO codes as character, values
#'   are ATO occupation codes.
#' @keywords internal
#' @noRd
.ato_occupation_crosswalk <- local({
  cache <- NULL

  function() {
    if (!is.null(cache)) {
      return(cache)
    }
    path <- system.file("extdata", "codeframes",
                        "anzsco-to-ato-occupation.tsv", package = "fplida")
    if (!nzchar(path)) {
      path <- file.path("inst", "extdata", "codeframes",
                        "anzsco-to-ato-occupation.tsv")
    }
    if (!file.exists(path)) {
      stop("The bundled ANZSCO-to-ATO occupation crosswalk is missing. ",
           "Reinstall fplida.", call. = FALSE)
    }
    x <- utils::read.delim(path, stringsAsFactors = FALSE)
    cache <<- stats::setNames(as.integer(x$ato_code),
                              as.character(x$anzsco_code))
    cache
  }
})


#' Map ANZSCO occupation codes to ATO salary and wage occupation codes
#'
#' An ANZSCO code with no ATO counterpart resolves up the code hierarchy — unit
#' group, then minor group, then major group. Every ANZSCO 2019 code resolves at
#' or above the major-group level, so the `999000` "Occupation not listed" code
#' is only a defensive fallback for input that is not valid ANZSCO at all.
#'
#' @param anzsco Integer vector of ANZSCO occupation codes.
#' @return Integer vector of ATO occupation codes, same length.
#' @keywords internal
#' @noRd
.ato_occupation_code <- function(anzsco) {
  anzsco <- as.integer(anzsco)
  xwalk <- .ato_occupation_crosswalk()
  out <- unname(xwalk[as.character(anzsco)])
  # Not valid ANZSCO, or missing: "Occupation not listed".
  out[is.na(out)] <- 999000L
  # Preserve a genuinely absent occupation rather than inventing one.
  out[is.na(anzsco)] <- NA_integer_
  out
}
