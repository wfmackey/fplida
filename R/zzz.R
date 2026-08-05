.onLoad <- function(libname, pkgname) {
  env_path <- Sys.getenv("FPLIDA_DATA_PATH", "")
  if (nzchar(env_path)) {
    options(fplida.data_path = env_path)
  }
}
