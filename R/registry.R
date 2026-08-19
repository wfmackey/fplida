# The registry -- what PLIDA and BLADE contain -- lives in fplida.info.
#
# Everything the registry knows is useful on its own, to a far wider group
# than the people generating synthetic microdata: someone planning a project
# wants to know whether PLIDA carries the column they need, and does not want
# to install a Rust toolchain to find out. fplida imports the data package so
# there is one copy of the registry and one build of it, and re-exports its
# readers so existing code keeps working unchanged.

#' @importFrom fplida.info dataset_info variable_info variable_values
#'   get_values get_mbs_item_numbers get_pbs_item_codes get_mesh_blocks
#'   get_sa1_codes get_sa2_codes get_country_codes
NULL

#' @export
fplida.info::dataset_info

#' @export
fplida.info::variable_info

#' @export
fplida.info::variable_values

#' @export
fplida.info::get_values

#' @export
fplida.info::get_mbs_item_numbers

#' @export
fplida.info::get_pbs_item_codes

#' @export
fplida.info::get_mesh_blocks

#' @export
fplida.info::get_sa1_codes

#' @export
fplida.info::get_sa2_codes

#' @export
fplida.info::get_country_codes


#' Path to a file shipped with the registry
#'
#' Code frames and the internal documentation moved to `fplida.info` with the
#' rest of the registry. Generators still read them, so this resolves a path
#' in the data package, falling back to this package's own `inst/` when it is
#' being run from a source tree that has not split yet.
#'
#' @param ... Path components below `inst/`.
#' @return Character path, or `""` when the file is not found.
#' @keywords internal
registry_file <- function(...) {
  path <- system.file(..., package = "fplida.info")
  if (nzchar(path)) return(path)
  path <- system.file(..., package = "fplida")
  if (nzchar(path)) return(path)
  local <- file.path("inst", ...)
  if (file.exists(local)) return(local)
  local <- file.path("fplida.info", "inst", ...)
  if (file.exists(local)) return(local)
  ""
}
