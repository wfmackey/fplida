#!/usr/bin/env Rscript

# Build the same DataLab-style subset as build_datalab_1m.R, but with
# 10,000,000 persons and ~/offline/datalab10m as the default target.

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[[1L]])))
} else {
  getwd()
}

if (!nzchar(Sys.getenv("FPLIDA_DATALAB_TARGET"))) {
  Sys.setenv(FPLIDA_DATALAB_TARGET = "~/offline/datalab10m")
}
if (!nzchar(Sys.getenv("FPLIDA_DATALAB_WORK_ROOT"))) {
  Sys.setenv(FPLIDA_DATALAB_WORK_ROOT = "~/offline/.datalab10m-build")
}
if (!nzchar(Sys.getenv("FPLIDA_DATALAB_N"))) {
  Sys.setenv(FPLIDA_DATALAB_N = "10000000")
}

source(file.path(script_dir, "build_datalab_1m.R"), chdir = TRUE)
