# Residential mobility and address noise.
#
# The spine gives a household one dwelling, and every product now reads it, so
# every product agrees on every address. A dwelling model where every agency
# agrees perfectly is as misleading as one where nobody does, in the opposite
# direction: it lets a pipeline that would break on real PLIDA run clean here.
#
# Three things are modelled, each with a published figure behind it: people
# with no address at all, agencies disagreeing with one another, and addresses
# that lag the move. The lag needs something to lag behind, so a move history
# comes first.


# Share of people who changed address in the year before the 2021 Census, and
# over the five years before it. Source: ABS, Population movement in Australia.
# https://www.abs.gov.au/articles/population-movement-australia
.MOBILITY_MOVED_1YR <- 0.150
.MOBILITY_MOVED_5YR <- 0.407

# Share of movers who stayed inside their own state. Same source.
.MOBILITY_SAME_STATE <- 0.870

# How far a move goes, as cumulative shares. Of records that disagree at SA1,
# the published rates say 80% also disagree at SA2 (18.07/22.48), 42% at SA4
# (9.50/22.48) and 9.7% at state (2.18/22.48). Reading that as distance: 20%
# of moves stay inside the SA2, a further 38% inside the SA4, a further 32%
# inside the state, and the remaining 9.7% cross a border. The interstate
# share is close to the 13% the mobility article reports for movers leaving
# their state, which is the two sources agreeing.
.MOBILITY_MOVE_WITHIN_SA2 <- 0.200
.MOBILITY_MOVE_WITHIN_SA4 <- 0.580
.MOBILITY_MOVE_WITHIN_STATE <- 0.903

# Share of people the ABS could tie to a dwelling on its 2021 administrative
# population snapshot was 91%: the remaining 9%, 2.3 million of 25.7 million,
# could be coded to an area but not to an address. Source: ABS,
# Administrative data snapshot of population and housing methodology, 30 June
# 2021.
# https://www.abs.gov.au/methodologies/administrative-data-snapshot-population-and-housing-experimental-housing-
.MOBILITY_NO_ADDRESS <- 0.090

# Children are harder to place: 4.3% to 11.2% of children with an address
# history between 2006 and 2021 were missing an ARID. Source: ABS, Creating a
# child mobility indicator using the Life Course Dataset.
# https://www.abs.gov.au/statistics/detailed-methodology-information/information-papers/creating-child-mobility-
.MOBILITY_NO_ADDRESS_CHILD <- 0.112

# On 2016 Census night PLIDA disagreed with the Census for 22.48% of records
# at SA1, 18.07% at SA2, 9.50% at SA4 and 2.18% at state level. Source:
# Bernard, Wu, Wilson, Argent, Zajac and Kimpton (2024), Demographic Research
# 51(22), Tables 5 and 6, pp. 700-701.
# https://www.demographic-research.org/volumes/vol51/22/51-22.pdf
.MOBILITY_DISAGREE_SA1 <- 0.2248

# Mismatch is not random. It is highest for children, young adults, recent
# migrants and First Nations Australians, and lowest for homeowners and people
# with dependent children. Same source, Table 6.
.MOBILITY_DISAGREE_MULTIPLIER <- c(child = 1.6, young_adult = 1.5,
                                   recent_migrant = 1.7, indigenous = 1.4,
                                   settled = 0.6)


#' A deterministic uniform draw
#'
#' Keyed on whatever identifier is passed and a named purpose, so a draw is
#' stable across runs, across products and across build slices, and
#' independent between purposes. The reduction is done in two steps because
#' the product of a nine-digit key and the multiplier exceeds the range a
#' double represents exactly.
#'
#' @param key Numeric vector. The identifier the draw belongs to.
#' @param seed Integer. Random seed.
#' @param purpose Character. What the draw is for.
#' @return Numeric vector in [0, 1).
#' @keywords internal
.mobility_draw_for <- function(key, seed, purpose) {
  n <- length(key)
  if (!n) return(numeric(0))
  modulus <- 1000003
  salt <- .stable_name_seed(purpose)
  # The salt chooses the multiplier rather than being added to the result.
  # Adding it only rotates the sequence, which leaves two purposes perfectly
  # rank-correlated: conditioning on one then skews the other, and a move
  # distance drawn that way is not independent of whether the household moved
  # at all. A different multiplier is a different permutation of the key
  # space, and the modulus is prime so every multiplier is coprime with it.
  multiplier <- 2 + (salt %% (modulus - 3))
  (((as.numeric(key) %% modulus) * multiplier +
      salt * 7919 + as.numeric(seed) * 9176) %% modulus) / modulus
}

#' A deterministic uniform draw for a person
#'
#' @param spine_rows data.frame. Spine rows.
#' @param seed Integer. Random seed.
#' @param purpose Character. What the draw is for.
#' @return Numeric vector in [0, 1).
#' @keywords internal
.mobility_draw <- function(spine_rows, seed, purpose) {
  n <- nrow(spine_rows)
  if (!n) return(numeric(0))
  person <- suppressWarnings(as.numeric(gsub(
    "[^0-9]", "", as.character(spine_rows$spine_id))))
  unusable <- !is.finite(person)
  person[unusable] <- seq_len(n)[unusable]
  .mobility_draw_for(person, seed, purpose)
}

#' A deterministic uniform draw for a dwelling
#'
#' A household moves as one, so anything that follows from a move is drawn
#' for the dwelling rather than for each resident. Drawing it per person puts
#' co-residents at different addresses, which is the record the dwelling
#' model exists to rule out.
#'
#' @param spine_rows data.frame. Spine rows.
#' @param seed Integer. Random seed.
#' @param purpose Character. What the draw is for.
#' @return Numeric vector in [0, 1), equal within a dwelling.
#' @keywords internal
.mobility_dwelling_draw <- function(spine_rows, seed, purpose) {
  .mobility_draw_for(.dil_dwelling_key(spine_rows), seed, purpose)
}

#' Whether a person is in a group that reports its address less reliably
#'
#' @param spine_rows data.frame. Spine rows.
#' @param reference_year Integer. Year the address is reported for.
#' @return Numeric vector of multipliers on the base disagreement rate.
#' @keywords internal
.mobility_disagreement_multiplier <- function(spine_rows,
                                              reference_year = 2021L) {
  n <- nrow(spine_rows)
  if (!n) return(numeric(0))
  out <- rep(1, n)

  age <- suppressWarnings(
    as.integer(reference_year) - as.integer(spine_rows$birth_year))
  age[is.na(age)] <- 40L

  if ("indigenous" %in% names(spine_rows)) {
    indigenous <- as.integer(spine_rows$indigenous) %in% 2:4
    out[indigenous] <- .MOBILITY_DISAGREE_MULTIPLIER[["indigenous"]]
  }
  if ("year_of_arrival" %in% names(spine_rows)) {
    arrival <- suppressWarnings(as.integer(spine_rows$year_of_arrival))
    recent <- !is.na(arrival) & arrival > reference_year - 5L
    out[recent] <- .MOBILITY_DISAGREE_MULTIPLIER[["recent_migrant"]]
  }
  # Age dominates: a young adult moves more than anyone, and a child's
  # address is reported by somebody else.
  out[age >= 18L & age < 30L] <- .MOBILITY_DISAGREE_MULTIPLIER[["young_adult"]]
  out[age < 18L] <- .MOBILITY_DISAGREE_MULTIPLIER[["child"]]
  # Settled: middle-aged, which stands in for the homeowner the source names.
  out[age >= 45L & age < 70L] <- .MOBILITY_DISAGREE_MULTIPLIER[["settled"]]
  out
}


#' A household's residential move history
#'
#' The spine gives a household one dwelling for the whole window, so a move
#' cannot be read from it. This derives one deterministically: whether the
#' household moved in the last year and in the last five, and where it lived
#' before, so an address can lag a move and PUR1P and PUR5P can carry
#' something other than the person's current SA2.
#'
#' A household moves as one, so the draw is keyed on the dwelling. Keying it
#' per person would move half a household and leave the rest, which is the
#' record the dwelling model exists to rule out.
#'
#' @param spine_rows data.frame. Spine rows, needing `state` and `sa2_code`.
#' @param seed Integer. Random seed.
#' @return A list with `moved_1yr`, `moved_5yr` and `previous_sa2`.
#' @keywords internal
.spine_move_history <- function(spine_rows, seed) {
  n <- nrow(spine_rows)
  if (!n) {
    return(list(moved_1yr = logical(0), moved_5yr = logical(0),
                previous_sa2 = character(0)))
  }

  move_draw <- .mobility_dwelling_draw(spine_rows, seed, "residential move")
  moved_5yr <- move_draw < .MOBILITY_MOVED_5YR
  # A household that moved in the last year is one that moved in the last
  # five, so the two rates nest rather than being drawn apart.
  moved_1yr <- move_draw < .MOBILITY_MOVED_1YR

  current_sa2 <- as.character(spine_rows$sa2_code)
  previous_sa2 <- current_sa2
  if (!any(moved_5yr)) {
    return(list(moved_1yr = moved_1yr, moved_5yr = moved_5yr,
                previous_sa2 = previous_sa2))
  }

  lookup <- .load_mb_lookup()
  state <- as.integer(spine_rows$state)
  state[is.na(state) | !state %in% 1:8] <- 1L
  sa2_int <- suppressWarnings(as.integer(current_sa2))
  sa4_of_sa2 <- lookup$sa4_code[match(sa2_int, lookup$sa2_code)]

  distance <- .mobility_dwelling_draw(spine_rows, seed, "move distance")
  pick <- .mobility_dwelling_draw(spine_rows, seed, "previous address")

  # How far a move goes decides which geographies disagree, and the published
  # mismatch rates give the ladder directly: of records that differ at SA1,
  # 80% also differ at SA2, 42% at SA4 and 9.7% at state.
  band <- cut(distance, breaks = c(-Inf, .MOBILITY_MOVE_WITHIN_SA2,
                                   .MOBILITY_MOVE_WITHIN_SA4,
                                   .MOBILITY_MOVE_WITHIN_STATE, Inf),
              labels = c("sa2", "sa4", "state", "interstate"))

  # Draw from a pool, stepping past the row's own SA2 rather than removing it
  # from the pool: removing it would take out every group member's SA2 and
  # could empty the pool entirely.
  from_pool <- function(rows, pool, own) {
    if (!length(rows) || !length(pool)) return(NULL)
    idx <- 1L + as.integer(pick[rows] * length(pool)) %% length(pool)
    value <- pool[idx]
    if (length(pool) > 1L) {
      collide <- which(value == own)
      if (length(collide)) {
        idx[collide] <- 1L + (idx[collide] %% length(pool))
        value <- pool[idx]
      }
    }
    as.character(value)
  }

  # A move inside the SA2 changes the SA1 and nothing above it, so the SA2 is
  # left alone and the address key below it does the work.
  # A move inside the SA4 changes the SA2.
  sa4_rows <- moved_5yr & band == "sa4" & !is.na(sa4_of_sa2)
  for (sa4 in unique(sa4_of_sa2[sa4_rows])) {
    rows <- which(sa4_rows & sa4_of_sa2 == sa4)
    pool <- unique(lookup$sa2_code[lookup$sa4_code == sa4])
    value <- from_pool(rows, pool, sa2_int[rows])
    if (!is.null(value)) previous_sa2[rows] <- value
  }
  # A move inside the state changes the SA4, so the pool is every other SA4
  # in the state. Grouping by the row's own SA4 keeps that filter exact.
  state_rows <- moved_5yr & band == "state" & !is.na(sa4_of_sa2)
  for (sa4 in unique(sa4_of_sa2[state_rows])) {
    rows <- which(state_rows & sa4_of_sa2 == sa4)
    st <- state[rows[1L]]
    pool <- unique(lookup$sa2_code[lookup$state == st &
                                     lookup$sa4_code != sa4])
    value <- from_pool(rows, pool, sa2_int[rows])
    if (!is.null(value)) previous_sa2[rows] <- value
  }
  # And an interstate move changes the state.
  inter_rows <- moved_5yr & band == "interstate"
  for (st in unique(state[inter_rows])) {
    rows <- which(inter_rows & state == st)
    pool <- unique(lookup$sa2_code[lookup$state != st])
    value <- from_pool(rows, pool, sa2_int[rows])
    if (!is.null(value)) previous_sa2[rows] <- value
  }

  previous_sa2[!moved_5yr] <- current_sa2[!moved_5yr]
  list(moved_1yr = moved_1yr, moved_5yr = moved_5yr,
       previous_sa2 = previous_sa2)
}


#' Which people an agency holds a stale address for
#'
#' The ABS assumes three months between a person moving and their Medicare
#' address being updated, and the Productivity Commission puts it more
#' bluntly: reported addresses in some datasets may be significantly out of
#' date. An agency that has not caught up reports where the person used to
#' live, which is what makes two agencies disagree.
#'
#' Sources: ABS, Regional internal migration estimates, provisional
#' methodology,
#' https://www.abs.gov.au/methodologies/regional-internal-migration-estimates-provisional-methodology/mar-2021;
#' Productivity Commission (2024), A-PLIDA-nalysis, p. 26,
#' https://assets.pc.gov.au/research/completed/plida/plida.pdf
#'
#' @param spine_rows data.frame. Spine rows.
#' @param seed Integer. Random seed.
#' @param agency Character. Which agency's address this is.
#' @param moved_5yr Logical. Whether the person has moved at all.
#' @param reference_year Integer. Year the address is reported for.
#' @return Logical vector: TRUE where the agency reports the old address.
#' @keywords internal
.mobility_stale_address <- function(spine_rows, seed, agency, moved_5yr,
                                    reference_year = 2021L) {
  n <- nrow(spine_rows)
  if (!n) return(logical(0))
  # Only a household that moved can have a stale address, so the rate is
  # conditional on having moved: 22.48% of all records disagreeing at SA1,
  # over the 40.7% who moved, is the share of movers an agency has not caught
  # up with. Two agencies disagree when one has caught up and the other has
  # not, so the between-agency rate this produces is close to, not equal to,
  # the published against-Census rate.
  rate <- .MOBILITY_DISAGREE_SA1 / .MOBILITY_MOVED_5YR
  # Keyed on the dwelling: a household's address is one address, so an agency
  # holding a stale copy holds it for the whole household.
  draw <- .mobility_dwelling_draw(spine_rows, seed,
                                  paste("stale address", agency))
  moved_5yr & draw < min(rate, 1)
}


#' Which people have no address at all
#'
#' 9% of the 25.7 million people on the ABS 2021 administrative population
#' snapshot could be coded to an area but not to an address. Children are
#' harder to place than adults.
#'
#' @param spine_rows data.frame. Spine rows.
#' @param seed Integer. Random seed.
#' @param reference_year Integer. Year the address is reported for.
#' @return Logical vector: TRUE where the person has no address.
#' @keywords internal
.mobility_no_address <- function(spine_rows, seed, reference_year = 2021L) {
  n <- nrow(spine_rows)
  if (!n) return(logical(0))
  age <- suppressWarnings(
    as.integer(reference_year) - as.integer(spine_rows$birth_year))
  age[is.na(age)] <- 40L
  rate <- rep(.MOBILITY_NO_ADDRESS, n) *
    .mobility_disagreement_multiplier(spine_rows, reference_year)
  rate[age < 18L] <- .MOBILITY_NO_ADDRESS_CHILD
  # Keyed on the person: whether a household has an address is the household's
  # business, but whether a person's own records resolve to it is theirs, and
  # that is what the child figures measure.
  .mobility_draw(spine_rows, seed, "address unresolved") < pmin(rate, 1)
}
