# Slice worker entry point.
#
# A slice worker is a fresh R process (spawned by build_fplida_sliced())
# that runs the full product pipeline against a pre-materialised slice
# spine. The orchestrator passes it: slice_run_dir (containing
# _system/base-spine.parquet with just this slice's rows), slice_id
# (for seed offset), products to build, years, and format.
#
# The worker reuses the existing per-product generators unchanged —
# each generator auto-discovers its run_dir via fplida.run_dir option.


#' Run the fplida product pipeline inside a single slice worker
#'
#' Entry point called inside a PSOCK worker R process. Sets the
#' run-directory option so existing generators find the slice's spine,
#' then invokes each requested product generator in order. Skips
#' \code{spine}, \code{core}, and \code{blade} — these are handled
#' centrally by the orchestrator. Returns per-product timing and
#' metadata.
#'
#' @param slice_run_dir Character. Path to slice run directory (already
#'   contains \code{_system/base-spine.parquet}).
#' @param slice_id Integer. 0-based slice id.
#' @param slice_seed Integer. Seed for this slice (orchestrator
#'   pre-computes as \code{base_seed + slice_id * 100000L}).
#' @param years Integer vector.
#' @param products Character vector. Products to build (should already
#'   exclude \code{spine} and \code{core}).
#' @param export_format Character. "parquet" or "csv".
#' @return List with \code{slice_id}, \code{elapsed_seconds},
#'   \code{product_results}.
#' @param mbs_pbs_chunk Integer. MBS/PBS chunk_size override passed
#'   to the worker. The orchestrator sizes this to keep the
#'   \eqn{K \times \text{chunk peak}} total memory comfortably under
#'   machine RAM. Default 300000.
#' @export
build_fplida_slice_worker <- function(slice_run_dir,
                                      slice_id,
                                      slice_seed,
                                      years,
                                      products,
                                      export_format = "parquet",
                                      mbs_pbs_chunk = 300000L) {
  slice_id    <- as.integer(slice_id)
  slice_seed  <- as.integer(slice_seed)
  years       <- as.integer(years)
  mbs_pbs_chunk <- as.integer(mbs_pbs_chunk)
  export_format <- match.arg(export_format, c("parquet", "csv"))

  stopifnot(dir.exists(slice_run_dir))
  spine_path <- file.path(slice_run_dir, "_system", "base-spine.parquet")
  if (!file.exists(spine_path)) {
    stop("Slice worker: slice spine not found at ", spine_path,
         call. = FALSE)
  }

  # Point downstream generators at this slice's run directory.
  options(fplida.run_dir = slice_run_dir)

  # The output_dir argument to generators resolves to the parent of
  # the run directory; we pass that so resolve_run_dir's base scan
  # points at the right place if the option is lost.
  output_dir <- dirname(slice_run_dir)

  # Products to skip — handled centrally by the orchestrator.
  skip_products <- c("spine", "core", "blade")
  to_build <- setdiff(products, skip_products)

  total_start <- proc.time()
  results <- list()

  for (product in to_build) {
    t0 <- proc.time()
    res <- tryCatch(
      .dispatch_slice_product(product, slice_seed, years,
                              output_dir, export_format,
                              mbs_pbs_chunk),
      error = function(e) {
        list(error = conditionMessage(e))
      }
    )
    elapsed <- (proc.time() - t0)[["elapsed"]]
    results[[product]] <- list(
      elapsed_seconds = elapsed,
      metadata = res
    )
  }

  total_elapsed <- (proc.time() - total_start)[["elapsed"]]

  list(
    slice_id = slice_id,
    slice_run_dir = slice_run_dir,
    elapsed_seconds = total_elapsed,
    product_results = results
  )
}


#' Dispatch a single product to its generator with slice-appropriate args
#'
#' Mirrors the switch() in build_fplida() but uses the slice seed and
#' slice run directory context. Not exported — only called from
#' \code{build_fplida_slice_worker()}.
#'
#' @noRd
.dispatch_slice_product <- function(product, seed, years,
                                    output_dir, export_format,
                                    mbs_pbs_chunk = 300000L) {
  # Chunk size is passed in by the orchestrator, which scales it to
  # keep K * per-worker peak memory under machine RAM. Defaults to
  # 300k (safe for K=10 on 32GB at ≤10M N) but the orchestrator drops
  # it for larger N.
  SLICED_MBS_PBS_CHUNK <- mbs_pbs_chunk

  switch(product,
    census = generate_census(seed = seed, output_dir = output_dir,
                             format = export_format, return_data = FALSE),
    pit_ps = {
      ps_years <- sort(unique(as.integer(c(2010L:(min(years) - 1L), years))))
      generate_pit_ps(seed = seed, years = ps_years, output_dir = output_dir,
                      format = export_format, return_data = FALSE)
    },
    pit_itr = {
      itr_years <- sort(unique(as.integer(c(2010L:(min(years) - 1L), years))))
      generate_pit_itr(seed = seed, years = itr_years, output_dir = output_dir,
                       format = export_format, return_data = FALSE)
    },
    he        = generate_he(seed = seed, years = years,
                            output_dir = output_dir,
                            format = export_format, return_data = FALSE),
    domino    = generate_domino(seed = seed, years = years,
                                output_dir = output_dir,
                                format = export_format, return_data = FALSE),
    mbs       = generate_mbs(seed = seed, years = years,
                             output_dir = output_dir,
                             format = export_format, return_data = FALSE,
                             chunk_size = SLICED_MBS_PBS_CHUNK),
    pbs       = generate_pbs(seed = seed, years = years,
                             output_dir = output_dir,
                             format = export_format, return_data = FALSE,
                             chunk_size = SLICED_MBS_PBS_CHUNK),
    tva       = generate_tva(seed = seed, years = years,
                             output_dir = output_dir,
                             format = export_format, return_data = FALSE),
    combined  = generate_combined(seed = seed, output_dir = output_dir,
                                  format = export_format, return_data = FALSE),
    births    = generate_births(seed = seed, output_dir = output_dir,
                                format = export_format, return_data = FALSE),
    deaths    = generate_deaths(seed = seed, years = years,
                                output_dir = output_dir,
                                format = export_format, return_data = FALSE),
    mcd       = generate_mcd(seed = seed, output_dir = output_dir,
                             format = export_format, return_data = FALSE),
    ato_cr    = generate_ato_cr(seed = seed, output_dir = output_dir,
                                format = export_format, return_data = FALSE),
    visa      = generate_visa(seed = seed, output_dir = output_dir,
                              format = export_format, return_data = FALSE),
    mt_demogs = generate_mt_demogs(seed = seed, output_dir = output_dir,
                                   format = export_format, return_data = FALSE),
    sdb       = generate_sdb(seed = seed, output_dir = output_dir,
                             format = export_format, return_data = FALSE),
    travellers = generate_travellers(seed = seed, years = years,
                                     output_dir = output_dir,
                                     format = export_format, return_data = FALSE),
    pit_ie    = generate_pit_ie(seed = seed, output_dir = output_dir,
                                format = export_format, return_data = FALSE),
    busown    = generate_busown(seed = seed, output_dir = output_dir,
                                format = export_format, return_data = FALSE),
    sae       = generate_sae(seed = seed, output_dir = output_dir,
                             format = export_format, return_data = FALSE),
    cgt       = generate_cgt(seed = seed, output_dir = output_dir,
                             format = export_format, return_data = FALSE),
    rps       = generate_rps(seed = seed, output_dir = output_dir,
                             format = export_format, return_data = FALSE),
    stp       = {
      stp_years <- sort(unique(as.integer(c(2020L:max(years)))))
      generate_stp(seed = seed, years = stp_years,
                   output_dir = output_dir,
                   format = export_format, return_data = FALSE)
    },
    ndis      = generate_ndis(seed = seed, output_dir = output_dir,
                              format = export_format, return_data = FALSE),
    apprentice = generate_apprentice(seed = seed, output_dir = output_dir,
                                     format = export_format, return_data = FALSE),
    dex       = generate_dex(seed = seed, output_dir = output_dir,
                             format = export_format, return_data = FALSE),
    air       = generate_air(seed = seed, output_dir = output_dir,
                             format = export_format, return_data = FALSE),
    amep      = generate_amep(seed = seed, output_dir = output_dir,
                              format = export_format, return_data = FALSE),
    nacdc     = generate_nacdc(seed = seed, output_dir = output_dir,
                               format = export_format, return_data = FALSE),
    aedc      = generate_aedc(seed = seed, output_dir = output_dir,
                              format = export_format, return_data = FALSE),
    acld      = generate_acld(seed = seed, output_dir = output_dir,
                              format = export_format, return_data = FALSE),
    sdac      = generate_sdac(seed = seed, output_dir = output_dir,
                              format = export_format, return_data = FALSE),
    ers       = generate_ers(seed = seed, output_dir = output_dir,
                             format = export_format, return_data = FALSE),
    jk        = generate_jk(seed = seed, output_dir = output_dir,
                            format = export_format, return_data = FALSE),
    jm        = generate_jm(seed = seed, output_dir = output_dir,
                            format = export_format, return_data = FALSE),
    nhs       = generate_nhs(seed = seed, output_dir = output_dir,
                             format = export_format, return_data = FALSE),
    nsmhw     = generate_nsmhw(seed = seed, output_dir = output_dir,
                               format = export_format, return_data = FALSE),
    pex       = generate_pex(seed = seed, output_dir = output_dir,
                             format = export_format, return_data = FALSE),
    apsed     = generate_apsed(seed = seed, output_dir = output_dir,
                               format = export_format, return_data = FALSE),
    ato_mcs   = generate_ato_mcs(seed = seed, output_dir = output_dir,
                                 format = export_format, return_data = FALSE),
    lfs       = generate_lfs(seed = seed, output_dir = output_dir,
                             format = export_format, return_data = FALSE),
    smsf      = generate_smsf(seed = seed, output_dir = output_dir,
                              format = export_format, return_data = FALSE),
    stop("Unknown product in slice worker: ", product, call. = FALSE)
  )
}
