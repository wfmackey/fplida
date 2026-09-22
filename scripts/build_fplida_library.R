#!/usr/bin/env Rscript
# Plan is the default so a mistyped command cannot start a large build.
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_file <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
library_script_root <- file.path(dirname(script_file), "fplida-library")
source(file.path(library_script_root, "00-setup.R"))

arguments <- commandArgs(TRUE)
if (any(!grepl("^--[a-z-]+=", arguments))) stop("Use --name=value arguments")
argument_names <- sub("^--([^=]+)=.*$", "\\1", arguments)
argument_values <- sub("^--[^=]+=", "", arguments)
args <- stats::setNames(as.list(argument_values), argument_names)
unknown <- setdiff(names(args), c("profile", "action", "run-id", "format"))
if (length(unknown)) stop("Unknown arguments: ", paste(unknown, collapse = ", "))
profile_name <- if (is.null(args$profile)) "100k" else args$profile
action <- if (is.null(args$action)) "plan" else args$action
profiles <- library_profiles()
if (!profile_name %in% names(profiles)) stop("Profile must be 1k, 100k, 1m or 30m")
config <- profiles[[profile_name]]
if (!is.null(args$format)) {
  if (!args$format %in% c("csv", "parquet")) stop("Format must be csv or parquet")
  config$format <- args$format
}
paths <- library_paths()
run_id <- args[["run-id"]]
if (is.null(run_id)) {
  if (action %in% c("verify", "publish", "tidy")) stop("This action requires --run-id")
  run_id <- paste0(format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC"), "-", profile_name)
}
if (!grepl("^[A-Za-z0-9_-]+$", run_id)) stop("Invalid run ID")
switch(action,
  plan = cat(jsonlite::toJSON(list(profile = config, paths = paths, run_id = run_id),
                             pretty = TRUE, auto_unbox = TRUE), "\n"),
  build = build_library_profile(config, paths, run_id),
  verify = verify_library_profile(paths, run_id),
  publish = publish_library_profile(paths, run_id),
  tidy = archive_obsolete_library_entries(paths, run_id),
  stop("Action must be plan, build, verify, publish or tidy")
)
