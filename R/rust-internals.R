#' Internal Rust wrapper functions
#'
#' Low-level bindings to Rust-compiled generation functions. These are
#' called by the user-facing \code{generate_*} functions and should not
#' normally be used directly.
#'
#' @name rust-internals
#' @keywords internal
#' @param n,seed,min_year,max_year,year,years Scalar integers.
#' @param birth_year,sex,state,education,archetype,indigenous Integer vectors.
#' @param aeuid,id,spine_id,aeuid_ato,aeuid_ncver,country_of_birth Character vectors.
#' @param baseline_employed,baseline_income,baseline_hours,anzsco_major,industry,valid_anzsco_codes Numeric/integer vectors.
#' @param first_year,last_year Integer vectors (per-person year bounds).
#' @param item_num,item_category,item_group,item_sub_heading,item_benefit_type Integer vectors (MBS item table).
#' @param item_schedule_fee,item_benefit_75,item_benefit_85,item_benefit_100,item_weight Numeric vectors (MBS item table).
#' @param provider_pool,ref_pool,prac_pool Character vectors (MBS provider pools).
#' @param pbs_code,atc_level1,benefit_type Character vectors (PBS item table).
#' @param claimed_price,pack_size,number_of_repeats Numeric vectors (PBS item table).
#' @param prescriber_pool,pharmacy_pool Character vectors (PBS provider pools).
#' @param primary_ben Integer vector (DOMINO benefit type).
#' @param spell_aeuid,spell_foe,spell_inst_code,spell_country_of_birth,spell_attend_mode,spell_course_code Character vectors (HE spell data).
#' @param spell_commence_year,spell_actual_duration,spell_completed,spell_is_ft,spell_qual_idx,spell_inst_state Integer vectors (HE spell data).
#' @param anzsco_code Character or integer vector.
NULL
