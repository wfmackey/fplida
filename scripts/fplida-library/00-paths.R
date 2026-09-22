# Final data and build records use separate roots.
library_paths <- function() {
  offline <- path.expand(Sys.getenv("FPLIDA_OFFLINE_DIR", "~/offline"))
  list(
    final = file.path(offline, "fplida-data"),
    staging = file.path(offline, ".fplida-library-staging"),
    records = file.path(offline, "fplida-library-build-records"),
    backup = file.path(offline, "fplida-library-backups"),
    source = normalizePath(Sys.getenv("FPLIDA_SOURCE_REPO", getwd()),
                           mustWork = TRUE)
  )
}

library_run_paths <- function(paths, profile, run_id) {
  list(
    work = file.path(paths$staging, run_id, "work"),
    slices = file.path(paths$staging, run_id, "slices"),
    records = file.path(paths$records, run_id),
    final = file.path(paths$final, profile),
    backup = file.path(paths$backup, run_id, profile)
  )
}
