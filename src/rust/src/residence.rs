//! Shared monthly physical-presence logic for CORE scope and TRAVELLERS.

pub const STATUS_RESIDENT_PRESENT: i32 = 1;
pub const STATUS_RESIDENT_ABSENT: i32 = 2;
pub const STATUS_NOT_RESIDENT: i32 = 3;

const SECONDS_PER_DAY_HASH: u64 = 86_400;

pub fn days_in_month(year: i32, month: i32) -> i32 {
    match month {
        1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
        4 | 6 | 9 | 11 => 30,
        2 => {
            if is_leap_year(year) {
                29
            } else {
                28
            }
        }
        _ => 30,
    }
}

fn is_leap_year(year: i32) -> bool {
    (year % 4 == 0 && year % 100 != 0) || year % 400 == 0
}

pub fn month_end_date32(year: i32, month: i32) -> i32 {
    days_from_civil(year, month, days_in_month(year, month))
}

fn days_from_civil(year: i32, month: i32, day: i32) -> i32 {
    let mut y = year as i64;
    let m = month as i64;
    let d = day as i64;
    y -= if m <= 2 { 1 } else { 0 };
    let era = if y >= 0 { y } else { y - 399 } / 400;
    let yoe = y - era * 400;
    let mp = m + if m > 2 { -3 } else { 9 };
    let doy = (153 * mp + 2) / 5 + d - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    (era * 146_097 + doe - 719_468) as i32
}

pub fn pp_weight_for_month(
    birth_year: i32,
    month_of_birth: i32,
    country_of_birth: i32,
    year_of_arrival: i32,
    year_of_death: i32,
    month_of_death: i32,
    day_of_death: i32,
    residence_seed: i32,
    year: i32,
    month: i32,
) -> f64 {
    if before_birth(birth_year, month_of_birth, year, month) {
        return 0.0;
    }
    if before_arrival(country_of_birth, year_of_arrival, year) {
        return 0.0;
    }
    if after_death(year_of_death, month_of_death, year, month) {
        return 0.0;
    }

    let overseas = country_of_birth != 0;
    let mut weight = travel_weight(residence_seed, overseas, year, month);

    if in_death_month(year_of_death, month_of_death, year, month) {
        let dim = days_in_month(year, month) as f64;
        let death_share = (day_of_death.clamp(1, days_in_month(year, month)) as f64) / dim;
        weight = weight.min(death_share);
    }

    round_pp_weight(weight)
}

pub fn erp_status_for_quarter(
    birth_year: i32,
    month_of_birth: i32,
    country_of_birth: i32,
    year_of_arrival: i32,
    year_of_death: i32,
    month_of_death: i32,
    day_of_death: i32,
    residence_seed: i32,
    year: i32,
    quarter: i32,
) -> i32 {
    let start_month = (quarter - 1) * 3 + 1;
    let mut eligible = false;
    let mut present = false;
    for month in start_month..=(start_month + 2) {
        let structurally_resident = !before_birth(birth_year, month_of_birth, year, month)
            && !before_arrival(country_of_birth, year_of_arrival, year)
            && !after_death(year_of_death, month_of_death, year, month);
        if structurally_resident {
            eligible = true;
            let w = pp_weight_for_month(
                birth_year,
                month_of_birth,
                country_of_birth,
                year_of_arrival,
                year_of_death,
                month_of_death,
                day_of_death,
                residence_seed,
                year,
                month,
            );
            if w > 0.0 {
                present = true;
            }
        }
    }
    if !eligible {
        STATUS_NOT_RESIDENT
    } else if present {
        STATUS_RESIDENT_PRESENT
    } else {
        STATUS_RESIDENT_ABSENT
    }
}

pub fn pp_status_for_quarter(
    birth_year: i32,
    month_of_birth: i32,
    country_of_birth: i32,
    year_of_arrival: i32,
    year_of_death: i32,
    month_of_death: i32,
    day_of_death: i32,
    residence_seed: i32,
    year: i32,
    quarter: i32,
) -> i32 {
    let start_month = (quarter - 1) * 3 + 1;
    for month in start_month..=(start_month + 2) {
        let w = pp_weight_for_month(
            birth_year,
            month_of_birth,
            country_of_birth,
            year_of_arrival,
            year_of_death,
            month_of_death,
            day_of_death,
            residence_seed,
            year,
            month,
        );
        if w > 0.0 {
            return STATUS_RESIDENT_PRESENT;
        }
    }
    STATUS_NOT_RESIDENT
}

fn before_birth(birth_year: i32, month_of_birth: i32, year: i32, month: i32) -> bool {
    year < birth_year || (year == birth_year && month < month_of_birth.clamp(1, 12))
}

fn before_arrival(country_of_birth: i32, year_of_arrival: i32, year: i32) -> bool {
    country_of_birth != 0 && year_of_arrival != i32::MIN && year < year_of_arrival
}

fn after_death(year_of_death: i32, month_of_death: i32, year: i32, month: i32) -> bool {
    year_of_death != i32::MIN
        && (year > year_of_death || (year == year_of_death && month > month_of_death))
}

fn in_death_month(year_of_death: i32, month_of_death: i32, year: i32, month: i32) -> bool {
    year_of_death != i32::MIN && year == year_of_death && month == month_of_death
}

fn travel_weight(residence_seed: i32, overseas: bool, year: i32, month: i32) -> f64 {
    let full_absence_prob = if overseas { 0.012 } else { 0.005 };
    let partial_absence_prob = if overseas { 0.025 } else { 0.012 };
    let draw = unit_hash(residence_seed, year, month, 0);
    if draw < full_absence_prob {
        return 0.0;
    }
    if draw < full_absence_prob + partial_absence_prob {
        let dim = days_in_month(year, month);
        let present_days = 1 + (hash64(residence_seed, year, month, 1) % (dim as u64 - 1)) as i32;
        return present_days as f64 / dim as f64;
    }
    1.0
}

fn round_pp_weight(x: f64) -> f64 {
    (x * 1_000_000.0).round() / 1_000_000.0
}

fn unit_hash(seed: i32, year: i32, month: i32, salt: u64) -> f64 {
    let h = hash64(seed, year, month, salt);
    ((h % SECONDS_PER_DAY_HASH) as f64) / (SECONDS_PER_DAY_HASH as f64)
}

fn hash64(seed: i32, year: i32, month: i32, salt: u64) -> u64 {
    let mut x = (seed as i64 as u64)
        ^ ((year as i64 as u64) << 32)
        ^ ((month as u64) << 24)
        ^ salt.wrapping_mul(0x9E37_79B9_7F4A_7C15);
    x ^= x >> 30;
    x = x.wrapping_mul(0xBF58_476D_1CE4_E5B9);
    x ^= x >> 27;
    x = x.wrapping_mul(0x94D0_49BB_1331_11EB);
    x ^ (x >> 31)
}
