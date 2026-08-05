#' Set the fplida data output directory
#'
#' Sets the directory where fplida stores generated data (spine, dataset CSVs,
#' agency spine files). The path is persisted across R sessions by writing to
#' \code{~/.Renviron}.
#'
#' @param path Character. Directory path (will be created if it doesn't exist).
#' @return The normalised path (invisibly).
#' @export
set_data_path <- function(path) {
  path <- normalizePath(path, mustWork = FALSE)
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  options(fplida.data_path = path)

  # Persist to ~/.Renviron
  renviron <- file.path(Sys.getenv("HOME"), ".Renviron")
  lines <- if (file.exists(renviron)) readLines(renviron, warn = FALSE) else character(0)
  lines <- lines[!grepl("^FPLIDA_DATA_PATH=", lines)]
  lines <- c(lines, paste0("FPLIDA_DATA_PATH=", path))
  writeLines(lines, renviron)

  message("Data path set to: ", path)
  message("Persisted to ", renviron)
  invisible(path)
}


#' Get the fplida data output directory
#'
#' Returns the current data path, checking (in order):
#' \enumerate{
#'   \item \code{getOption("fplida.data_path")}
#'   \item \code{Sys.getenv("FPLIDA_DATA_PATH")}
#' }
#' Returns NULL if neither is set.
#'
#' @return Character path or NULL.
#' @export
get_data_path <- function() {
  path <- getOption("fplida.data_path")
  if (!is.null(path)) return(path)

  env <- Sys.getenv("FPLIDA_DATA_PATH", "")
  if (nzchar(env)) return(env)

  NULL
}
