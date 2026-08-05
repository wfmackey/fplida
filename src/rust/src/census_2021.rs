use extendr_api::prelude::*;
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

use crate::codeframes;
use crate::sampling::{uniform_int, weighted_sample};

// ==========================================================================
// Foundational distributions (embedded from census_2021.toml)
// ==========================================================================

// Sex
const SEX_WEIGHTS: [f64; 2] = [49.3, 50.7]; // Male, Female
const SEX_CODES: [i32; 2] = [1, 2];

// Age 5-year bands: counts used as weights
const AGE_BAND_WEIGHTS: [f64; 18] = [
    1_463_817.0,
    1_586_138.0,
    1_588_051.0,
    1_457_812.0,
    1_579_539.0,
    1_771_676.0,
    1_853_085.0,
    1_838_822.0,
    1_648_843.0,
    1_635_963.0,
    1_610_944.0,
    1_541_911.0,
    1_468_097.0,
    1_298_460.0,
    1_160_768.0,
    821_920.0,
    554_598.0,
    542_342.0,
];
const AGE_BAND_LOWER: [i32; 18] = [
    0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85,
];
const AGE_BAND_UPPER: [i32; 18] = [
    4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 99,
];

// State
const STATE_WEIGHTS: [f64; 9] = [
    8_072_163.0,
    6_503_491.0,
    5_156_138.0,
    1_781_516.0,
    2_660_026.0,
    557_571.0,
    232_605.0,
    454_499.0,
    4_779.0,
];
const STATE_CODES: [i32; 9] = [1, 2, 3, 4, 5, 6, 7, 8, 9];
const STATE_LABELS: [&str; 9] = [
    "New South Wales",
    "Victoria",
    "Queensland",
    "South Australia",
    "Western Australia",
    "Tasmania",
    "Northern Territory",
    "Australian Capital Territory",
    "Other Territories",
];

// Indigenous status
const INGP_WEIGHTS: [f64; 5] = [2.93, 0.13, 0.14, 92.8, 4.0];
const INGP_CODES_ABS: [&str; 5] = ["1", "2", "3", "4", "&"];

// Marital status (for 15+; under-15 get "@")
const MSTP_WEIGHTS_ABS: [f64; 5] = [36.5, 5.0, 8.8, 3.2, 46.5];
const MSTP_CODES_ABS: [&str; 5] = ["1", "2", "3", "4", "5"];

// Country of birth
const COB_WEIGHTS: [f64; 31] = [
    17_019_815.0,
    927_490.0,
    673_354.0,
    549_628.0,
    530_491.0,
    293_899.0,
    257_997.0,
    189_204.0,
    165_605.0,
    163_329.0,
    131_907.0,
    122_507.0,
    102_087.0,
    101_309.0,
    101_256.0,
    100_158.0,
    92_925.0,
    92_305.0,
    89_636.0,
    87_343.0,
    87_068.0,
    83_767.0,
    80_914.0,
    70_891.0,
    68_961.0,
    59_802.0,
    51_495.0,
    45_276.0,
    39_164.0,
    1_580_162.0,
    1_358_653.0,
];
const COB_CODES: [i32; 31] = [
    1101, 2100, 7103, 6101, 1201, 5203, 5105, 9225, 5202, 3104, 7104, 7203, 6201, 8104, 2304, 6102,
    4201, 3207, 7201, 3103, 5204, 5104, 2201, 4203, 1301, 4202, 7102, 6103, 5101, 9999, 0000,
];

// Language at home
const LANP_WEIGHTS: [f64; 16] = [
    18_303_662.0,
    685_274.0,
    367_159.0,
    320_758.0,
    295_281.0,
    239_033.0,
    200_000.0,
    180_000.0,
    160_000.0,
    150_000.0,
    140_000.0,
    120_000.0,
    110_000.0,
    100_000.0,
    2_700_000.0,
    1_251_621.0,
];
const LANP_CODES: [i32; 16] = [
    1201, 7104, 4202, 7101, 6301, 5211, 5203, 5206, 2301, 2401, 2303, 6202, 5208, 7203, 9999, 0000,
];

// Religion
const RELP_WEIGHTS: [f64; 16] = [
    38.9, 20.0, 9.8, 2.7, 2.1, 2.7, 3.2, 2.4, 1.6, 1.4, 1.0, 0.8, 3.1, 0.4, 0.8, 6.9,
];
const RELP_CODES_ABS: [&str; 16] = [
    "7", "1", "2", "3", "4", "5", "6", "69", "71", "72", "73", "207", "201", "601", "0000", "&",
];

// Education attainment (15+; under-15 get "@"). Top-level HEAP codes.
const HEAP_WEIGHTS: [f64; 13] = [
    26.3, 9.4, 3.5, 12.6, 14.9, 4.6, 10.0, 0.1, 0.0, 7.2, 2.4, 0.8, 8.2,
];
const HEAP_CODES_ABS: [&str; 13] = [
    "3", "4", "5", "5", "6", "6", "6", "7", "7", "8", "&", "8", "&",
];

// Highest year of school completed (15+; under-15 get "@").
const HSCP_WEIGHTS: [f64; 7] = [53.0, 7.5, 16.0, 5.0, 4.0, 1.5, 5.0];
const HSCP_CODES_ABS: [&str; 7] = ["1", "2", "3", "4", "5", "6", "&"];

// Personal income weekly (15+)
const INCP_WEIGHTS: [f64; 16] = [
    8.7, 3.3, 4.8, 7.6, 7.6, 7.3, 6.9, 8.1, 9.0, 6.7, 5.9, 4.4, 7.6, 1.8, 3.2, 7.2,
];
const INCP_CODES_ABS: [&str; 16] = [
    "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16",
];

// Labour force status (15+)
// Age-conditional employment rates: [employed, unemployed, NILF]
const AGE_EMPLOYMENT: [[f64; 3]; 15] = [
    [35.0, 10.0, 55.0], // 15-19
    [70.0, 8.0, 22.0],  // 20-24
    [80.0, 5.0, 15.0],  // 25-29
    [80.0, 4.0, 16.0],  // 30-34
    [80.0, 3.5, 16.5],  // 35-39
    [82.0, 3.0, 15.0],  // 40-44
    [82.0, 3.0, 15.0],  // 45-49
    [78.0, 3.0, 19.0],  // 50-54
    [70.0, 3.0, 27.0],  // 55-59
    [55.0, 3.0, 42.0],  // 60-64
    [25.0, 1.0, 74.0],  // 65-69
    [10.0, 0.5, 89.5],  // 70-74
    [4.0, 0.2, 95.8],   // 75-79
    [1.5, 0.1, 98.4],   // 80-84
    [0.5, 0.0, 99.5],   // 85+
];

// Occupation (employed persons only)
const OCCP_WEIGHTS: [f64; 9] = [24.0, 13.7, 12.9, 12.7, 11.5, 9.0, 8.2, 6.3, 1.7];
const OCCP_CODES_ABS: [&str; 9] = [
    "200000", "100000", "300000", "500000", "400000", "800000", "600000", "700000", "&&&&&&",
];
const OCCP_NFD_RATE_MANAGER: f64 = 0.08;
const OCCP_NFD_RATE_OTHER: f64 = 0.025;

// Industry (employed persons only)
const INDP_WEIGHTS: [f64; 20] = [
    14.5, 9.1, 8.9, 8.7, 8.0, 6.7, 6.3, 5.8, 5.0, 3.7, 2.4, 3.4, 3.7, 2.7, 1.7, 1.6, 1.7, 1.1, 2.3,
    2.7,
];
const INDP_CODES_ABS: [&str; 20] = [
    "Q", "G", "E", "P", "M", "O", "H", "C", "I", "K", "B", "N", "S", "F", "J", "R", "L", "D", "A",
    "&&&&",
];

// Hours worked ranges (employed persons)
const HRWRP_WEIGHTS: [f64; 12] = [
    6.5, 7.0, 7.0, 11.4, 7.5, 20.6, 20.3, 6.0, 6.0, 3.0, 2.9, 1.8,
];
const HRWRP_CODES_ABS: [&str; 12] = [
    "00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "&&",
];
const HRWRP_LOWER: [i32; 12] = [0, 1, 10, 20, 30, 35, 40, 45, 50, 60, 70, 0];
const HRWRP_UPPER: [i32; 12] = [0, 9, 19, 29, 34, 39, 44, 49, 59, 69, 99, 0];

// Dwelling structure
const STRD_WEIGHTS_ABS: [f64; 12] = [
    72.3, 8.0, 4.6, 5.0, 3.0, 2.0, 2.0, 2.2, 0.25, 0.20, 0.20, 0.25,
];
const STRD_CODES_ABS: [&str; 12] = [
    "11", "21", "22", "31", "32", "33", "34", "35", "91", "92", "93", "94",
];

// Tenure
const TEND_WEIGHTS_ABS: [f64; 8] = [
    2_872_331.0,
    3_242_449.0,
    100_000.0,
    2_842_378.0,
    100_000.0,
    81_518.0,
    100_000.0,
    136_538.0,
];
const TEND_CODES_ABS: [&str; 8] = ["1", "2", "3", "4", "5", "6", "7", "&"];

// Persons usually resident
const NPRD_WEIGHTS_ABS: [f64; 8] = [25.6, 33.0, 16.0, 15.0, 6.5, 2.0, 1.0, 0.9];
const NPRD_CODES_ABS: [&str; 8] = ["1", "2", "3", "4", "5", "6", "7", "8"];

// Bedrooms
const BEDRD_WEIGHTS_ABS: [f64; 8] = [0.5, 5.3, 19.1, 39.0, 28.0, 6.8, 1.4, 0.2];
const BEDRD_CODES_ABS: [&str; 8] = ["0", "1", "2", "3", "4", "5", "6", "&"];

// Vehicles
const VEHRD_WEIGHTS_ABS: [f64; 6] = [7.3, 36.2, 36.3, 13.8, 5.0, 1.5];
const VEHRD_CODES_ABS: [&str; 6] = ["0", "1", "2", "3", "4", "&"];

// Family composition
const FMCF_WEIGHTS: [f64; 4] = [2_944_140.0, 2_608_834.0, 1_068_268.0, 108_941.0];
const FMCF_CODES_ABS: [&str; 4] = ["2", "1", "3", "9"];

// Citizenship
const CITP_WEIGHTS: [f64; 3] = [78.0, 17.0, 5.0];
const CITP_CODES_ABS: [&str; 3] = ["1", "2", "&"];

// Method of travel to work
const MTWP_WEIGHTS: [f64; 11] = [52.7, 21.0, 11.8, 3.9, 2.5, 1.5, 1.4, 0.5, 0.9, 1.2, 2.6];
const MTWP_CODES_ABS: [&str; 11] = [
    "006", "233", "234", "007", "232", "002", "001", "010", "003", "011", "&&&",
];

// Voluntary work (15+)
const VOLWP_WEIGHTS: [f64; 3] = [14.1, 73.0, 4.9];
const VOLWP_CODES_ABS: [&str; 3] = ["2", "1", "&"];

// Unpaid care (15+)
const UNCAREP_WEIGHTS: [f64; 3] = [11.9, 76.0, 4.1];
const UNCAREP_CODES_ABS: [&str; 3] = ["2", "1", "&"];

// Unpaid child care (15+)
const CHCAREP_WEIGHTS_ABS: [f64; 5] = [61.0, 18.0, 5.0, 3.3, 4.7];
const CHCAREP_CODES_ABS: [&str; 5] = ["1", "2", "3", "4", "&"];

// Domestic work hours (15+)
const DOMP_WEIGHTS: [f64; 6] = [18.0, 19.8, 27.3, 12.1, 8.6, 6.2];
const DOMP_CODES_ABS: [&str; 6] = ["1", "2", "3", "4", "5", "&"];

// Defence service (15+)
const ADFP_WEIGHTS_ABS: [f64; 5] = [0.25, 0.15, 2.4, 91.2, 6.0];
const ADFP_CODES_ABS: [&str; 5] = ["1", "2", "3", "4", "&"];

// Student status
const STUP_WEIGHTS_ABS: [f64; 5] = [72.0, 15.0, 6.0, 2.0, 5.0];
const STUP_CODES_ABS: [&str; 5] = ["1", "2", "3", "4", "&"];

// Spine-conditioned Census person projection constants
const BPLP_OVERSEAS_CODES: [i32; 30] = [
    2100, 7100, 6100, 5100, 5200, 5300, 5400, 5500, 5600, 5700, 5800, 5900, 6200, 6300, 6400, 6500,
    6600, 6700, 6800, 6900, 7200, 7300, 7400, 7500, 7600, 7700, 7800, 7900, 8000, 8100,
];
const BPLP_OVERSEAS_WEIGHTS: [f64; 30] = [
    927490.0, 673354.0, 549628.0, 530491.0, 293899.0, 257997.0, 189204.0, 165605.0, 163329.0,
    131907.0, 122507.0, 102087.0, 101309.0, 101256.0, 100158.0, 92925.0, 92305.0, 89636.0, 87343.0,
    87068.0, 83767.0, 80914.0, 70891.0, 68961.0, 59802.0, 51495.0, 45276.0, 39164.0, 1580162.0,
    1358653.0,
];
const LANG_AT_HOME_WEIGHTS: [f64; 16] = [
    18303662.0, 685274.0, 367159.0, 320758.0, 295281.0, 239033.0, 200000.0, 180000.0, 160000.0,
    150000.0, 140000.0, 120000.0, 110000.0, 100000.0, 2700000.0, 1251621.0,
];
const RELIGION_WEIGHTS: [f64; 16] = [
    38.9, 20.0, 9.8, 2.7, 2.1, 2.7, 3.2, 2.4, 1.6, 1.4, 1.0, 0.8, 3.1, 0.4, 0.8, 6.9,
];
const BELOW_YEAR12_HEAP_CODES: [&str; 7] = ["8", "6", "7", "8", "8", "8", "&"];
const BELOW_YEAR12_WEIGHTS: [f64; 7] = [4.6, 10.0, 0.1, 0.0, 7.2, 0.8, 8.2];
const CERT34_HEAP_CODES: [&str; 2] = ["5", "5"];
const CERT34_WEIGHTS: [f64; 2] = [3.5, 12.6];
const BACHELOR_PLUS_HEAP_CODES: [&str; 3] = ["1", "2", "3"];
const BACHELOR_PLUS_WEIGHTS: [f64; 3] = [8.0, 2.0, 16.0];
const INCOME_WEIGHTS: [f64; 16] = [
    8.7, 3.3, 4.8, 7.6, 7.6, 7.3, 6.9, 8.1, 9.0, 6.7, 5.9, 4.4, 7.6, 1.8, 3.2, 7.2,
];
const NOT_EMPLOYED_CODES_ABS: [&str; 3] = ["4", "5", "6"];
const NOT_EMPLOYED_WEIGHTS: [f64; 3] = [1.8, 1.3, 33.1];
const MTWP_CODES_R: [&str; 11] = [
    "006", "233", "234", "007", "232", "002", "001", "010", "003", "011", "&&&",
];
const MTWP_WEIGHTS_R: [f64; 11] = [52.7, 21.0, 11.8, 3.9, 2.5, 1.5, 1.4, 0.5, 0.9, 1.2, 2.6];
const DOMP_CODES_R: [&str; 6] = ["1", "2", "3", "4", "5", "&"];
const DOMP_WEIGHTS_R: [f64; 6] = [18.0, 19.8, 27.3, 12.1, 8.6, 6.2];
const ASSNP_CODES_R: [&str; 3] = ["1", "2", "&"];
const ASSNP_WEIGHTS_R: [f64; 3] = [5.8, 86.5, 7.7];
const ASSNP_NOT_STATED_RATE: f64 = 0.077;

const HMHCP_PREV: [f64; 8] = [0.050, 0.120, 0.120, 0.100, 0.090, 0.070, 0.050, 0.040];
const HARTP_PREV: [f64; 8] = [0.002, 0.005, 0.015, 0.040, 0.100, 0.180, 0.280, 0.350];
const HASTP_PREV: [f64; 8] = [0.100, 0.090, 0.080, 0.075, 0.070, 0.075, 0.080, 0.085];
const HDIAP_PREV: [f64; 8] = [0.001, 0.005, 0.010, 0.025, 0.050, 0.100, 0.160, 0.200];
const HHEDP_PREV: [f64; 8] = [0.001, 0.002, 0.005, 0.015, 0.040, 0.080, 0.140, 0.200];
const HCANP_PREV: [f64; 8] = [0.001, 0.002, 0.005, 0.015, 0.030, 0.060, 0.100, 0.140];
const HLUNP_PREV: [f64; 8] = [0.003, 0.005, 0.008, 0.010, 0.015, 0.025, 0.040, 0.060];
const HKIDP_PREV: [f64; 8] = [0.0005, 0.001, 0.002, 0.004, 0.008, 0.015, 0.030, 0.050];
const HSTRP_PREV: [f64; 8] = [0.0001, 0.0005, 0.001, 0.003, 0.008, 0.015, 0.030, 0.050];
const HDEMP_PREV: [f64; 8] = [0.000, 0.000, 0.000, 0.001, 0.002, 0.005, 0.020, 0.080];

// ==========================================================================
// Person generation
// ==========================================================================

fn age_band_to_employment_index(age: i32) -> usize {
    match age {
        15..=19 => 0,
        20..=24 => 1,
        25..=29 => 2,
        30..=34 => 3,
        35..=39 => 4,
        40..=44 => 5,
        45..=49 => 6,
        50..=54 => 7,
        55..=59 => 8,
        60..=64 => 9,
        65..=69 => 10,
        70..=74 => 11,
        75..=79 => 12,
        80..=84 => 13,
        _ => 14, // 85+
    }
}

fn weighted_code_i32(rng: &mut StdRng, weights: &[f64], codes: &[i32]) -> i32 {
    codes[weighted_sample(rng, weights)]
}

fn weighted_code_str<'a>(rng: &mut StdRng, weights: &[f64], codes: &[&'a str]) -> &'a str {
    codes[weighted_sample(rng, weights)]
}

fn age5p_code(age: i32) -> String {
    fmt2(((age.max(0) / 5) + 1).min(21))
}

fn age10p_code(age: i32) -> String {
    fmt2(((age.max(0) / 10) + 1).min(11))
}

fn fmt2(value: i32) -> String {
    format!("{:02}", value.clamp(0, 99))
}

fn fmt4(value: i32) -> String {
    format!("{:04}", value.clamp(0, 9999))
}

fn code_i32(value: i32) -> String {
    value.to_string()
}

fn code_or_supp4(value: i32) -> String {
    if value <= 0 || value == 9999 {
        "&&&&".to_string()
    } else {
        fmt4(value)
    }
}

fn industry_code_from_spine(industry: i32) -> String {
    match industry {
        1 => "Q",
        2 => "G",
        3 => "E",
        4 => "P",
        5 => "M",
        6 => "O",
        7 => "H",
        8 => "C",
        9 => "I",
        10 => "K",
        11 => "B",
        12 => "N",
        13 => "S",
        14 => "F",
        15 => "J",
        16 => "R",
        17 => "L",
        18 => "D",
        19 => "A",
        _ => "&&&&",
    }
    .to_string()
}

fn fmt_occp6(value: i32) -> String {
    format!("{value:06}")
}

fn valid_census_occupation_code(code: i32, valid_anzsco_codes: &[i32]) -> bool {
    if !(100000..=899999).contains(&code) {
        return false;
    }

    if valid_anzsco_codes.is_empty() {
        return true;
    }

    valid_anzsco_codes.binary_search(&code).is_ok()
}

fn occupation_major_group(code: i32, major: i32) -> i32 {
    if (1..=8).contains(&major) {
        major
    } else {
        code / 100000
    }
}

fn occupation_major_nfd_code(code: i32, major: i32) -> Option<i32> {
    let major_group = occupation_major_group(code, major);
    if (1..=8).contains(&major_group) {
        Some(major_group * 100000)
    } else {
        None
    }
}

fn occupation_nfd_rate(code: i32, major: i32) -> f64 {
    match occupation_major_group(code, major) {
        1 => OCCP_NFD_RATE_MANAGER,
        2..=8 => OCCP_NFD_RATE_OTHER,
        _ => 0.0,
    }
}

fn occupation_code_from_spine(
    code: i32,
    major: i32,
    valid_anzsco_codes: &[i32],
    rng: &mut StdRng,
) -> String {
    let nfd_code = occupation_major_nfd_code(code, major);

    if valid_census_occupation_code(code, valid_anzsco_codes) {
        if let Some(nfd) = nfd_code {
            if valid_census_occupation_code(nfd, valid_anzsco_codes)
                && rng.gen::<f64>() < occupation_nfd_rate(code, major)
            {
                return fmt_occp6(nfd);
            }
        }
        return fmt_occp6(code);
    }

    if let Some(fallback) = nfd_code {
        if valid_census_occupation_code(fallback, valid_anzsco_codes) {
            fmt_occp6(fallback)
        } else {
            "@@@@@@".to_string()
        }
    } else {
        "@@@@@@".to_string()
    }
}

fn age_band_8_health(age: i32) -> usize {
    if age < 20 {
        0
    } else if age < 30 {
        1
    } else if age < 40 {
        2
    } else if age < 50 {
        3
    } else if age < 60 {
        4
    } else if age < 70 {
        5
    } else if age < 80 {
        6
    } else {
        7
    }
}

fn map_indigenous_from_spine(indigenous: i32, rng: &mut StdRng) -> String {
    if rng.gen::<f64>() < 0.05 {
        "&".to_string()
    } else {
        match indigenous {
            // The shared spine is ordered Non-Indigenous, Aboriginal,
            // Torres Strait Islander, Both. INGP uses the ABS Census order
            // Aboriginal, Torres Strait Islander, Both, Non-Indigenous.
            1 => "4".to_string(),
            2 => "1".to_string(),
            3 => "2".to_string(),
            4 => "3".to_string(),
            _ => "&".to_string(),
        }
    }
}

fn map_citizenship_from_spine(citizenship: i32, rng: &mut StdRng) -> String {
    if rng.gen::<f64>() < 0.05 {
        "&".to_string()
    } else {
        match citizenship {
            1 | 2 => citizenship.to_string(),
            _ => "&".to_string(),
        }
    }
}

fn map_education_from_spine(education: i32, rng: &mut StdRng) -> String {
    match education {
        0 => "@".to_string(),
        1 => weighted_code_str(rng, &BELOW_YEAR12_WEIGHTS, &BELOW_YEAR12_HEAP_CODES).to_string(),
        2 => "6".to_string(),
        3 => weighted_code_str(rng, &CERT34_WEIGHTS, &CERT34_HEAP_CODES).to_string(),
        4 => "4".to_string(),
        5 => weighted_code_str(rng, &BACHELOR_PLUS_WEIGHTS, &BACHELOR_PLUS_HEAP_CODES).to_string(),
        _ => "&".to_string(),
    }
}

fn map_school_year_from_spine(age: i32, education: i32, rng: &mut StdRng) -> String {
    if age < 15 {
        "@".to_string()
    } else if rng.gen::<f64>() < 0.03 {
        "&".to_string()
    } else {
        match education {
            0 => "@".to_string(),
            1 => weighted_code_str(rng, &[16.0, 5.0, 4.0, 1.5, 5.0], &["3", "4", "5", "6", "&"])
                .to_string(),
            2..=5 => "1".to_string(),
            _ => weighted_code_str(rng, &HSCP_WEIGHTS, &HSCP_CODES_ABS).to_string(),
        }
    }
}

fn map_lfsp_from_spine(employed: bool, baseline_hours: i32, age: i32, rng: &mut StdRng) -> String {
    if age < 15 {
        "@".to_string()
    } else if employed {
        if rng.gen::<f64>() < 0.03 {
            "3".to_string()
        } else if baseline_hours >= 35 {
            "1".to_string()
        } else {
            "2".to_string()
        }
    } else {
        weighted_code_str(rng, &NOT_EMPLOYED_WEIGHTS, &NOT_EMPLOYED_CODES_ABS).to_string()
    }
}

fn map_hours_from_spine(hours: i32, employed: bool, rng: &mut StdRng) -> (String, String) {
    if !employed {
        return ("@@".to_string(), "@@".to_string());
    }
    // The 2021 Census Dictionary reports 1.7% non-response for hours worked.
    // Keep the range and single-hour fields missing together.
    if rng.gen::<f64>() < 0.017 {
        return ("&&".to_string(), "&&".to_string());
    }
    let actual = (hours + rng.gen_range(-5..=5)).max(0).min(99);
    let hrwrp = if actual == 0 {
        "00"
    } else if actual < 10 {
        "01"
    } else if actual < 20 {
        "02"
    } else if actual < 30 {
        "03"
    } else if actual < 35 {
        "04"
    } else if actual < 40 {
        "05"
    } else if actual < 45 {
        "06"
    } else if actual < 50 {
        "07"
    } else if actual < 60 {
        "08"
    } else if actual < 70 {
        "09"
    } else {
        "10"
    };
    (hrwrp.to_string(), fmt2(actual))
}

fn health_condition_code(
    has_condition: bool,
    yes_code: &str,
    no_code: &str,
    not_stated: bool,
) -> String {
    if not_stated {
        "&&&".to_string()
    } else if has_condition {
        yes_code.to_string()
    } else {
        no_code.to_string()
    }
}

fn health_comorbidity_code(first: bool, second: bool, not_stated: bool) -> String {
    if not_stated {
        "&".to_string()
    } else if first && second {
        "1".to_string()
    } else {
        "2".to_string()
    }
}

fn employment_flag_code(lfsp: &str) -> &'static str {
    match lfsp {
        "1" | "2" | "3" => "1",
        "4" | "5" | "6" => "2",
        "V" => "V",
        _ => "@",
    }
}

fn labour_force_participation_code(lfsp: &str) -> &'static str {
    match lfsp {
        "1" | "2" | "3" | "4" | "5" => "1",
        "6" => "2",
        "V" => "V",
        _ => "@",
    }
}

fn unemployment_flag_code(lfsp: &str) -> &'static str {
    match lfsp {
        "4" | "5" => "1",
        "1" | "2" | "3" => "2",
        "V" => "V",
        _ => "@",
    }
}

fn labour_force_hours_code(lfsp: &str, hrsp: &str) -> &'static str {
    match lfsp {
        "1" | "2" if hrsp == "&&" => "4",
        "1" => "1",
        "2" => "2",
        "3" => "3",
        "4" => "5",
        "5" => "6",
        "6" => "7",
        "&" => "&",
        "V" => "V",
        _ => "@",
    }
}

fn engagement_code(age: i32, lfsp: &str, stup: &str) -> &'static str {
    if age < 15 {
        return "@";
    }

    let employed = matches!(lfsp, "1" | "2" | "3");
    let full_time_work = lfsp == "1";
    let part_time_work = lfsp == "2";
    let away_from_work = lfsp == "3";
    let not_employed = matches!(lfsp, "4" | "5" | "6");
    let full_time_study = stup == "2";
    let part_time_study = stup == "3";
    let attending_hours_unknown = stup == "4";
    let attending = full_time_study || part_time_study || attending_hours_unknown;

    if lfsp == "V" || stup == "V" {
        "V"
    } else if full_time_work || full_time_study || (employed && attending) {
        "1"
    } else if (part_time_work && stup == "1") || (not_employed && part_time_study) {
        "2"
    } else if (matches!(lfsp, "2" | "3") && stup == "&")
        || (away_from_work && stup == "1")
        || (lfsp == "&" && matches!(stup, "3" | "4"))
        || (not_employed && attending_hours_unknown)
    {
        "3"
    } else if not_employed && stup == "1" {
        "4"
    } else {
        "&"
    }
}

fn assistance_need_code(
    disability_onset_year: i32,
    disability_severity: i32,
    rng: &mut StdRng,
) -> &'static str {
    let onset_missing = disability_onset_year == i32::MIN;
    let severity_missing = disability_severity == i32::MIN;

    // A partially observed or invalid disability record cannot support a
    // substantive assistance classification.
    if onset_missing != severity_missing
        || (!severity_missing && !(1..=4).contains(&disability_severity))
        || rng.gen::<f64>() < ASSNP_NOT_STATED_RATE
    {
        "&"
    } else if !onset_missing
        && disability_onset_year <= 2021
        && (1..=2).contains(&disability_severity)
    {
        "1"
    } else {
        "2"
    }
}

fn year_arrival_range_code(yarp: &str) -> &'static str {
    match yarp {
        "@@@@" => "@",
        "&&&&" => "&",
        "VVVV" => "V",
        _ => match yarp.parse::<i32>() {
            Ok(year) if year <= 1950 => "1",
            Ok(year) if year <= 1960 => "2",
            Ok(year) if year <= 1970 => "3",
            Ok(year) if year <= 1980 => "4",
            Ok(year) if year <= 1990 => "5",
            Ok(year) if year <= 2000 => "6",
            Ok(year) if year <= 2010 => "7",
            Ok(year) if year <= 2020 => "8",
            Ok(_) => "9",
            Err(_) => "&",
        },
    }
}

#[derive(Clone, Copy)]
struct HealthOut {
    clthp: &'static str,
    other: bool,
    not_stated: bool,
    hmhcp: bool,
    hartp: bool,
    hastp: bool,
    hdiap: bool,
    hhedp: bool,
    hcanp: bool,
    hlunp: bool,
    hkidp: bool,
    hstrp: bool,
    hdemp: bool,
}

fn generate_health_conditions_from_spine(
    age: i32,
    task_physical: f64,
    employed: bool,
    rng: &mut StdRng,
) -> HealthOut {
    let band = age_band_8_health(age);

    let hmhcp = rng.gen::<f64>() < HMHCP_PREV[band];

    let mut arth_mod = 1.0_f64;
    if !task_physical.is_nan() && task_physical >= 0.6 {
        arth_mod = 1.3;
    }
    if !employed {
        arth_mod = arth_mod.max(1.2);
    }
    let hartp = rng.gen::<f64>() < (HARTP_PREV[band] * arth_mod).min(1.0);
    let hastp = rng.gen::<f64>() < HASTP_PREV[band];
    let hdiap = rng.gen::<f64>() < HDIAP_PREV[band];
    let hhedp = rng.gen::<f64>() < HHEDP_PREV[band];
    let hcanp = rng.gen::<f64>() < HCANP_PREV[band];
    let hlunp = rng.gen::<f64>() < HLUNP_PREV[band];
    let hkidp = rng.gen::<f64>() < HKIDP_PREV[band];
    let hstrp = rng.gen::<f64>() < HSTRP_PREV[band];
    let hdemp = rng.gen::<f64>() < HDEMP_PREV[band];
    let other = rng.gen::<f64>() < 0.080;
    let not_stated = rng.gen::<f64>() < 0.081;

    let total = hmhcp as i32
        + hartp as i32
        + hastp as i32
        + hdiap as i32
        + hhedp as i32
        + hcanp as i32
        + hlunp as i32
        + hkidp as i32
        + hstrp as i32
        + hdemp as i32;
    let clthp = if not_stated {
        "&"
    } else if total == 0 {
        "0"
    } else if total == 1 {
        "1"
    } else if total == 2 {
        "2"
    } else {
        "3"
    };
    HealthOut {
        clthp,
        other,
        not_stated,
        hmhcp,
        hartp,
        hastp,
        hdiap,
        hhedp,
        hcanp,
        hlunp,
        hkidp,
        hstrp,
        hdemp,
    }
}

/// Project the Census 2021 person table from the spine.
/// @export
#[extendr]
#[allow(clippy::too_many_arguments)]
fn project_census_person__(
    aeuid_abs: Strings,
    birth_year: &[i32],
    sex: &[i32],
    state: &[i32],
    indigenous: &[i32],
    country_of_birth_sacc: &[i32],
    year_of_arrival: &[i32],
    citizenship: &[i32],
    education: &[i32],
    baseline_employed: &[i32],
    baseline_hours: &[i32],
    anzsco_code: &[i32],
    anzsco_major: &[i32],
    industry: &[i32],
    valid_anzsco_codes: &[i32],
    disability_onset_year: &[i32],
    disability_severity: &[i32],
    task_physical: &[f64],
    sa2: &[i32],
    seed: i32,
) -> List {
    let n = birth_year.len();
    let mut rng = StdRng::seed_from_u64((seed as u64).wrapping_add(100));

    let mut synthetic_aeuid: Vec<String> = Vec::with_capacity(n);
    let mut sexp: Vec<i32> = Vec::with_capacity(n);
    let mut agep: Vec<i32> = Vec::with_capacity(n);
    let mut age5p: Vec<String> = Vec::with_capacity(n);
    let mut age10p: Vec<String> = Vec::with_capacity(n);
    let mut dobyp: Vec<i32> = Vec::with_capacity(n);
    let mut ifagep: Vec<String> = Vec::with_capacity(n);
    let mut ifsexp: Vec<String> = Vec::with_capacity(n);
    let mut ifmstp: Vec<String> = Vec::with_capacity(n);
    let mut steucp: Vec<i32> = Vec::with_capacity(n);
    // ASGS 2021 usual-residence geography (consistent with state, from spine).
    let mut sa2ucp: Vec<i32> = Vec::with_capacity(n);
    let mut sa3ucp: Vec<i32> = Vec::with_capacity(n);
    let mut sa4ucp: Vec<i32> = Vec::with_capacity(n);
    let mut ingp: Vec<String> = Vec::with_capacity(n);
    let mut bplp: Vec<String> = Vec::with_capacity(n);
    let mut yarp: Vec<String> = Vec::with_capacity(n);
    let mut yarrp: Vec<String> = Vec::with_capacity(n);
    let mut lanp: Vec<String> = Vec::with_capacity(n);
    let mut relp: Vec<String> = Vec::with_capacity(n);
    let mut citp: Vec<String> = Vec::with_capacity(n);
    let mut heap: Vec<String> = Vec::with_capacity(n);
    let mut hscp: Vec<String> = Vec::with_capacity(n);
    let mut mstp: Vec<String> = Vec::with_capacity(n);
    let mut incp: Vec<String> = Vec::with_capacity(n);
    let mut lfsp: Vec<String> = Vec::with_capacity(n);
    let mut emfp: Vec<String> = Vec::with_capacity(n);
    let mut lffp: Vec<String> = Vec::with_capacity(n);
    let mut uefp: Vec<String> = Vec::with_capacity(n);
    let mut lfhrp: Vec<String> = Vec::with_capacity(n);
    let mut eetp: Vec<String> = Vec::with_capacity(n);
    let mut occp: Vec<String> = Vec::with_capacity(n);
    let mut indp: Vec<String> = Vec::with_capacity(n);
    let mut hrwrp: Vec<String> = Vec::with_capacity(n);
    let mut hrsp: Vec<String> = Vec::with_capacity(n);
    let mut mtwp: Vec<String> = Vec::with_capacity(n);
    let mut clthp: Vec<String> = Vec::with_capacity(n);
    let mut hlthp: Vec<String> = Vec::with_capacity(n);
    let mut holhp: Vec<String> = Vec::with_capacity(n);
    let mut coarasp: Vec<String> = Vec::with_capacity(n);
    let mut coardbp: Vec<String> = Vec::with_capacity(n);
    let mut coarhdp: Vec<String> = Vec::with_capacity(n);
    let mut coarmhp: Vec<String> = Vec::with_capacity(n);
    let mut coashdp: Vec<String> = Vec::with_capacity(n);
    let mut coaslcp: Vec<String> = Vec::with_capacity(n);
    let mut cocnhdp: Vec<String> = Vec::with_capacity(n);
    let mut codbhdp: Vec<String> = Vec::with_capacity(n);
    let mut codbkdp: Vec<String> = Vec::with_capacity(n);
    let mut cohdkdp: Vec<String> = Vec::with_capacity(n);
    let mut cohdmhp: Vec<String> = Vec::with_capacity(n);
    let mut colcmhp: Vec<String> = Vec::with_capacity(n);
    let mut hmhcp: Vec<String> = Vec::with_capacity(n);
    let mut hartp: Vec<String> = Vec::with_capacity(n);
    let mut hastp: Vec<String> = Vec::with_capacity(n);
    let mut hdiap: Vec<String> = Vec::with_capacity(n);
    let mut hhedp: Vec<String> = Vec::with_capacity(n);
    let mut hcanp: Vec<String> = Vec::with_capacity(n);
    let mut hlunp: Vec<String> = Vec::with_capacity(n);
    let mut hkidp: Vec<String> = Vec::with_capacity(n);
    let mut hstrp: Vec<String> = Vec::with_capacity(n);
    let mut hdemp: Vec<String> = Vec::with_capacity(n);
    let mut volwp: Vec<String> = Vec::with_capacity(n);
    let mut uncarep: Vec<String> = Vec::with_capacity(n);
    let mut chcarep: Vec<String> = Vec::with_capacity(n);
    let mut domp: Vec<String> = Vec::with_capacity(n);
    let mut adfp: Vec<String> = Vec::with_capacity(n);
    let mut stup: Vec<String> = Vec::with_capacity(n);
    let mut assnp: Vec<String> = Vec::with_capacity(n);

    for i in 0..n {
        let age = 2021 - birth_year[i];
        let employed = baseline_employed[i] == 1;
        let under15 = age < 15;

        synthetic_aeuid.push(aeuid_abs[i].to_string());
        sexp.push(sex[i]);
        agep.push(age);
        age5p.push(age5p_code(age));
        age10p.push(age10p_code(age));
        dobyp.push(birth_year[i]);
        ifagep.push(if rng.gen::<f64>() < 0.044 { "2" } else { "1" }.to_string());
        ifsexp.push(if rng.gen::<f64>() < 0.047 { "02" } else { "01" }.to_string());
        steucp.push(state[i]);

        // ASGS geography from the spine's SA2 (0 = unknown -> nest as 0).
        let s2 = sa2[i];
        sa2ucp.push(s2);
        sa3ucp.push(if s2 > 0 { s2 / 10_000 } else { 0 });
        sa4ucp.push(if s2 > 0 { s2 / 1_000_000 } else { 0 });

        ingp.push(map_indigenous_from_spine(indigenous[i], &mut rng));

        // BPLP is the SACC country-of-birth code carried on the spine, so a
        // person resolves to the same country across every PLIDA dataset.
        // 1101 = Australia; overseas-born carry a real SACC code.
        bplp.push(country_of_birth_sacc[i].to_string());

        let yarp_i = if year_of_arrival[i] == i32::MIN {
            "@@@@".to_string()
        } else {
            year_of_arrival[i].clamp(1905, 2021).to_string()
        };
        yarrp.push(year_arrival_range_code(&yarp_i).to_string());
        yarp.push(yarp_i);

        // LANP/RELP from the canonical ASCL/ASCRG code frames (full official
        // tables in codeframes.rs), replacing the former compact subsets.
        lanp.push(codeframes::sample_language(&mut rng).to_string());
        relp.push(codeframes::sample_religion(&mut rng).to_string());

        citp.push(map_citizenship_from_spine(citizenship[i], &mut rng));

        heap.push(map_education_from_spine(education[i], &mut rng));
        hscp.push(map_school_year_from_spine(age, education[i], &mut rng));

        if under15 {
            mstp.push("@".to_string());
            ifmstp.push("@".to_string());
            incp.push("@@".to_string());
        } else {
            mstp.push(weighted_code_str(&mut rng, &MSTP_WEIGHTS_ABS, &MSTP_CODES_ABS).to_string());
            ifmstp.push(if rng.gen::<f64>() < 0.055 { "2" } else { "1" }.to_string());
            incp.push(weighted_code_str(&mut rng, &INCOME_WEIGHTS, &INCP_CODES_ABS).to_string());
        }

        let lfsp_i = map_lfsp_from_spine(employed, baseline_hours[i], age, &mut rng);
        lfsp.push(lfsp_i.clone());

        if employed && !under15 {
            occp.push(occupation_code_from_spine(
                anzsco_code[i],
                anzsco_major[i],
                valid_anzsco_codes,
                &mut rng,
            ));
            indp.push(industry_code_from_spine(industry[i]));
            if lfsp_i == "3" {
                // A person away from work worked zero hours in Census week.
                hrwrp.push("00".to_string());
                hrsp.push("00".to_string());
            } else {
                let (hrwrp_i, hrsp_i) = map_hours_from_spine(baseline_hours[i], true, &mut rng);
                hrwrp.push(hrwrp_i);
                hrsp.push(hrsp_i);
            }
            mtwp.push(weighted_code_str(&mut rng, &MTWP_WEIGHTS_R, &MTWP_CODES_R).to_string());
        } else {
            occp.push("@@@@@@".to_string());
            indp.push("@@@@".to_string());
            hrwrp.push("@@".to_string());
            hrsp.push("@@".to_string());
            mtwp.push("@@@".to_string());
        }

        let health =
            generate_health_conditions_from_spine(age, task_physical[i], employed, &mut rng);
        clthp.push(health.clthp.to_string());
        hlthp.push(
            if health.not_stated {
                "&&&"
            } else if health.other || health.clthp != "0" {
                "122"
            } else {
                "121"
            }
            .to_string(),
        );
        holhp.push(
            if health.not_stated {
                "&&&"
            } else if health.other {
                "111"
            } else {
                "112"
            }
            .to_string(),
        );
        coarasp.push(health_comorbidity_code(
            health.hartp,
            health.hastp,
            health.not_stated,
        ));
        coardbp.push(health_comorbidity_code(
            health.hartp,
            health.hdiap,
            health.not_stated,
        ));
        coarhdp.push(health_comorbidity_code(
            health.hartp,
            health.hhedp,
            health.not_stated,
        ));
        coarmhp.push(health_comorbidity_code(
            health.hartp,
            health.hmhcp,
            health.not_stated,
        ));
        coashdp.push(health_comorbidity_code(
            health.hastp,
            health.hhedp,
            health.not_stated,
        ));
        coaslcp.push(health_comorbidity_code(
            health.hastp,
            health.hlunp,
            health.not_stated,
        ));
        cocnhdp.push(health_comorbidity_code(
            health.hcanp,
            health.hhedp,
            health.not_stated,
        ));
        codbhdp.push(health_comorbidity_code(
            health.hdiap,
            health.hhedp,
            health.not_stated,
        ));
        codbkdp.push(health_comorbidity_code(
            health.hdiap,
            health.hkidp,
            health.not_stated,
        ));
        cohdkdp.push(health_comorbidity_code(
            health.hhedp,
            health.hkidp,
            health.not_stated,
        ));
        cohdmhp.push(health_comorbidity_code(
            health.hhedp,
            health.hmhcp,
            health.not_stated,
        ));
        colcmhp.push(health_comorbidity_code(
            health.hlunp,
            health.hmhcp,
            health.not_stated,
        ));
        hmhcp.push(health_condition_code(
            health.hmhcp,
            "091",
            "092",
            health.not_stated,
        ));
        hartp.push(health_condition_code(
            health.hartp,
            "011",
            "012",
            health.not_stated,
        ));
        hastp.push(health_condition_code(
            health.hastp,
            "021",
            "022",
            health.not_stated,
        ));
        hdiap.push(health_condition_code(
            health.hdiap,
            "051",
            "052",
            health.not_stated,
        ));
        hhedp.push(health_condition_code(
            health.hhedp,
            "061",
            "062",
            health.not_stated,
        ));
        hcanp.push(health_condition_code(
            health.hcanp,
            "031",
            "032",
            health.not_stated,
        ));
        hlunp.push(health_condition_code(
            health.hlunp,
            "081",
            "082",
            health.not_stated,
        ));
        hkidp.push(health_condition_code(
            health.hkidp,
            "071",
            "072",
            health.not_stated,
        ));
        hstrp.push(health_condition_code(
            health.hstrp,
            "101",
            "102",
            health.not_stated,
        ));
        hdemp.push(health_condition_code(
            health.hdemp,
            "041",
            "042",
            health.not_stated,
        ));

        if under15 {
            volwp.push("@".to_string());
            uncarep.push("@".to_string());
            chcarep.push("@".to_string());
            domp.push("@".to_string());
            adfp.push("@".to_string());
        } else {
            volwp.push(weighted_code_str(&mut rng, &VOLWP_WEIGHTS, &VOLWP_CODES_ABS).to_string());
            uncarep.push(
                weighted_code_str(&mut rng, &UNCAREP_WEIGHTS, &UNCAREP_CODES_ABS).to_string(),
            );
            chcarep.push(
                weighted_code_str(&mut rng, &CHCAREP_WEIGHTS_ABS, &CHCAREP_CODES_ABS).to_string(),
            );
            domp.push(weighted_code_str(&mut rng, &DOMP_WEIGHTS_R, &DOMP_CODES_R).to_string());
            adfp.push(weighted_code_str(&mut rng, &ADFP_WEIGHTS_ABS, &ADFP_CODES_ABS).to_string());
        }

        let stup_i = weighted_code_str(&mut rng, &STUP_WEIGHTS_ABS, &STUP_CODES_ABS).to_string();
        stup.push(stup_i.clone());
        emfp.push(employment_flag_code(&lfsp_i).to_string());
        lffp.push(labour_force_participation_code(&lfsp_i).to_string());
        uefp.push(unemployment_flag_code(&lfsp_i).to_string());
        lfhrp.push(labour_force_hours_code(&lfsp_i, hrsp.last().unwrap()).to_string());
        eetp.push(engagement_code(age, &lfsp_i, &stup_i).to_string());
        assnp.push(
            assistance_need_code(disability_onset_year[i], disability_severity[i], &mut rng)
                .to_string(),
        );
    }

    list!(
        SYNTHETIC_AEUID = synthetic_aeuid,
        SEXP = sexp,
        AGEP = agep,
        AGE5P = age5p,
        AGE10P = age10p,
        DOBYP = dobyp,
        IFAGEP = ifagep,
        IFSEXP = ifsexp,
        IFMSTP = ifmstp,
        STEUCP = steucp,
        SA2UCP = sa2ucp,
        SA3UCP = sa3ucp,
        SA4UCP = sa4ucp,
        INGP = ingp,
        BPLP = bplp,
        YARP = yarp,
        YARRP = yarrp,
        LANP = lanp,
        RELP = relp,
        CITP = citp,
        HEAP = heap,
        HSCP = hscp,
        MSTP = mstp,
        INCP = incp,
        LFSP = lfsp,
        EMFP = emfp,
        LFFP = lffp,
        UEFP = uefp,
        LFHRP = lfhrp,
        EETP = eetp,
        OCCP = occp,
        INDP = indp,
        HRWRP = hrwrp,
        HRSP = hrsp,
        MTWP = mtwp,
        CLTHP = clthp,
        HLTHP = hlthp,
        HOLHP = holhp,
        COARASP = coarasp,
        COARDBP = coardbp,
        COARHDP = coarhdp,
        COARMHP = coarmhp,
        COASHDP = coashdp,
        COASLCP = coaslcp,
        COCNHDP = cocnhdp,
        CODBHDP = codbhdp,
        CODBKDP = codbkdp,
        COHDKDP = cohdkdp,
        COHDMHP = cohdmhp,
        COLCMHP = colcmhp,
        HMHCP = hmhcp,
        HARTP = hartp,
        HASTP = hastp,
        HDIAP = hdiap,
        HHEDP = hhedp,
        HCANP = hcanp,
        HLUNP = hlunp,
        HKIDP = hkidp,
        HSTRP = hstrp,
        HDEMP = hdemp,
        VOLWP = volwp,
        UNCAREP = uncarep,
        CHCAREP = chcarep,
        DOMP = domp,
        ADFP = adfp,
        STUP = stup,
        ASSNP = assnp
    )
}

/// Generate the Census 2021 person table.
/// Returns a list of named vectors that R will bind into a data.frame.
#[extendr]
fn generate_census_2021_person(n: i32, seed: i32) -> List {
    let n = n as usize;
    let mut rng = StdRng::seed_from_u64(seed as u64);

    let mut synthetic_aeuid: Vec<String> = Vec::with_capacity(n);
    let mut sexp: Vec<i32> = Vec::with_capacity(n);
    let mut agep: Vec<i32> = Vec::with_capacity(n);
    let mut age5p: Vec<String> = Vec::with_capacity(n);
    let mut age10p: Vec<String> = Vec::with_capacity(n);
    let mut ifagep: Vec<String> = Vec::with_capacity(n);
    let mut ifsexp: Vec<String> = Vec::with_capacity(n);
    let mut ifmstp: Vec<String> = Vec::with_capacity(n);
    let mut steucp: Vec<i32> = Vec::with_capacity(n);
    let mut ingp: Vec<String> = Vec::with_capacity(n);
    let mut mstp: Vec<String> = Vec::with_capacity(n);
    let mut bplp: Vec<String> = Vec::with_capacity(n);
    let mut lanp: Vec<String> = Vec::with_capacity(n);
    let mut relp: Vec<String> = Vec::with_capacity(n);
    let mut heap: Vec<String> = Vec::with_capacity(n);
    let mut hscp: Vec<String> = Vec::with_capacity(n);
    let mut incp: Vec<String> = Vec::with_capacity(n);
    let mut lfsp: Vec<String> = Vec::with_capacity(n);
    let mut emfp: Vec<String> = Vec::with_capacity(n);
    let mut lffp: Vec<String> = Vec::with_capacity(n);
    let mut uefp: Vec<String> = Vec::with_capacity(n);
    let mut lfhrp: Vec<String> = Vec::with_capacity(n);
    let mut eetp: Vec<String> = Vec::with_capacity(n);
    let mut occp: Vec<String> = Vec::with_capacity(n);
    let mut indp: Vec<String> = Vec::with_capacity(n);
    let mut hrwrp: Vec<String> = Vec::with_capacity(n);
    let mut hrsp: Vec<String> = Vec::with_capacity(n);
    let mut citp: Vec<String> = Vec::with_capacity(n);
    let mut mtwp: Vec<String> = Vec::with_capacity(n);
    let mut clthp: Vec<String> = Vec::with_capacity(n);
    let mut hlthp: Vec<String> = Vec::with_capacity(n);
    let mut holhp: Vec<String> = Vec::with_capacity(n);
    let mut coarasp: Vec<String> = Vec::with_capacity(n);
    let mut coardbp: Vec<String> = Vec::with_capacity(n);
    let mut coarhdp: Vec<String> = Vec::with_capacity(n);
    let mut coarmhp: Vec<String> = Vec::with_capacity(n);
    let mut coashdp: Vec<String> = Vec::with_capacity(n);
    let mut coaslcp: Vec<String> = Vec::with_capacity(n);
    let mut cocnhdp: Vec<String> = Vec::with_capacity(n);
    let mut codbhdp: Vec<String> = Vec::with_capacity(n);
    let mut codbkdp: Vec<String> = Vec::with_capacity(n);
    let mut cohdkdp: Vec<String> = Vec::with_capacity(n);
    let mut cohdmhp: Vec<String> = Vec::with_capacity(n);
    let mut colcmhp: Vec<String> = Vec::with_capacity(n);
    let mut hmhcp: Vec<String> = Vec::with_capacity(n);
    let mut hartp: Vec<String> = Vec::with_capacity(n);
    let mut hastp: Vec<String> = Vec::with_capacity(n);
    let mut hdiap: Vec<String> = Vec::with_capacity(n);
    let mut hhedp: Vec<String> = Vec::with_capacity(n);
    let mut hcanp: Vec<String> = Vec::with_capacity(n);
    let mut hlunp: Vec<String> = Vec::with_capacity(n);
    let mut hkidp: Vec<String> = Vec::with_capacity(n);
    let mut hstrp: Vec<String> = Vec::with_capacity(n);
    let mut hdemp: Vec<String> = Vec::with_capacity(n);
    let mut volwp: Vec<String> = Vec::with_capacity(n);
    let mut uncarep: Vec<String> = Vec::with_capacity(n);
    let mut chcarep: Vec<String> = Vec::with_capacity(n);
    let mut domp: Vec<String> = Vec::with_capacity(n);
    let mut adfp: Vec<String> = Vec::with_capacity(n);
    let mut stup: Vec<String> = Vec::with_capacity(n);
    let mut dobyp: Vec<i32> = Vec::with_capacity(n);
    let mut yarp: Vec<String> = Vec::with_capacity(n);
    let mut yarrp: Vec<String> = Vec::with_capacity(n);
    let mut assnp: Vec<String> = Vec::with_capacity(n);

    for i in 0..n {
        synthetic_aeuid.push(format!("P{:010}", i + 1));

        let sex_idx = weighted_sample(&mut rng, &SEX_WEIGHTS);
        sexp.push(SEX_CODES[sex_idx]);

        let age_idx = weighted_sample(&mut rng, &AGE_BAND_WEIGHTS);
        let age = uniform_int(&mut rng, AGE_BAND_LOWER[age_idx], AGE_BAND_UPPER[age_idx]);
        agep.push(age);
        age5p.push(age5p_code(age));
        age10p.push(age10p_code(age));
        ifagep.push(if rng.gen::<f64>() < 0.044 { "2" } else { "1" }.to_string());
        ifsexp.push(if rng.gen::<f64>() < 0.047 { "02" } else { "01" }.to_string());

        let is_adult = age >= 15;

        dobyp.push(2021 - age);
        steucp.push(STATE_CODES[weighted_sample(&mut rng, &STATE_WEIGHTS)]);

        ingp.push(weighted_code_str(&mut rng, &INGP_WEIGHTS, &INGP_CODES_ABS).to_string());

        let cob_idx = weighted_sample(&mut rng, &COB_WEIGHTS);
        bplp.push(code_or_supp4(COB_CODES[cob_idx]));

        if cob_idx != 0 && COB_CODES[cob_idx] != 0 {
            let birth_year = 2021 - age;
            let earliest = birth_year.max(1905);
            let yarp_i = uniform_int(&mut rng, earliest, 2021).to_string();
            yarrp.push(year_arrival_range_code(&yarp_i).to_string());
            yarp.push(yarp_i);
        } else {
            let yarp_i = if COB_CODES[cob_idx] == 0 {
                "&&&&"
            } else {
                "@@@@"
            };
            yarrp.push(year_arrival_range_code(yarp_i).to_string());
            yarp.push(yarp_i.to_string());
        }

        lanp.push(code_or_supp4(
            LANP_CODES[weighted_sample(&mut rng, &LANP_WEIGHTS)],
        ));

        relp.push(weighted_code_str(&mut rng, &RELP_WEIGHTS, &RELP_CODES_ABS).to_string());

        if is_adult {
            heap.push(weighted_code_str(&mut rng, &HEAP_WEIGHTS, &HEAP_CODES_ABS).to_string());
            hscp.push(weighted_code_str(&mut rng, &HSCP_WEIGHTS, &HSCP_CODES_ABS).to_string());
            mstp.push(weighted_code_str(&mut rng, &MSTP_WEIGHTS_ABS, &MSTP_CODES_ABS).to_string());
            ifmstp.push(if rng.gen::<f64>() < 0.055 { "2" } else { "1" }.to_string());
            incp.push(weighted_code_str(&mut rng, &INCP_WEIGHTS, &INCP_CODES_ABS).to_string());
        } else {
            heap.push("@".to_string());
            hscp.push("@".to_string());
            mstp.push("@".to_string());
            ifmstp.push("@".to_string());
            incp.push("@@".to_string());
        }

        citp.push(weighted_code_str(&mut rng, &CITP_WEIGHTS, &CITP_CODES_ABS).to_string());

        let is_employed;
        if is_adult {
            let emp_idx = age_band_to_employment_index(age);
            let emp_weights = &AGE_EMPLOYMENT[emp_idx];
            let lfs_idx = weighted_sample(&mut rng, emp_weights);
            is_employed = lfs_idx == 0;
            match lfs_idx {
                0 => {
                    if rng.gen::<f64>() < 0.03 {
                        lfsp.push("3".to_string());
                    } else if rng.gen::<f64>() < 0.65 {
                        lfsp.push("1".to_string());
                    } else {
                        lfsp.push("2".to_string());
                    }
                }
                1 => {
                    lfsp.push(weighted_code_str(&mut rng, &[1.8, 1.3], &["4", "5"]).to_string());
                }
                _ => {
                    lfsp.push("6".to_string());
                }
            }
        } else {
            is_employed = false;
            lfsp.push("@".to_string());
        }

        if is_employed {
            occp.push(weighted_code_str(&mut rng, &OCCP_WEIGHTS, &OCCP_CODES_ABS).to_string());
            indp.push(weighted_code_str(&mut rng, &INDP_WEIGHTS, &INDP_CODES_ABS).to_string());

            if lfsp.last().unwrap() == "3" {
                hrwrp.push("00".to_string());
                hrsp.push("00".to_string());
            } else {
                let hr_idx = weighted_sample(&mut rng, &HRWRP_WEIGHTS);
                let hrwrp_code = HRWRP_CODES_ABS[hr_idx];
                hrwrp.push(hrwrp_code.to_string());
                let hrs = if hr_idx == 0 {
                    0
                } else if hrwrp_code == "&&" {
                    0
                } else {
                    uniform_int(&mut rng, HRWRP_LOWER[hr_idx], HRWRP_UPPER[hr_idx])
                };
                hrsp.push(if hrwrp_code == "&&" {
                    "&&".to_string()
                } else {
                    fmt2(hrs)
                });
            }

            mtwp.push(weighted_code_str(&mut rng, &MTWP_WEIGHTS, &MTWP_CODES_ABS).to_string());
        } else {
            occp.push("@@@@@@".to_string());
            indp.push("@@@@".to_string());
            hrwrp.push("@@".to_string());
            hrsp.push("@@".to_string());
            mtwp.push("@@@".to_string());
        }

        let health = generate_health_conditions_from_spine(age, f64::NAN, is_employed, &mut rng);
        clthp.push(health.clthp.to_string());
        hlthp.push(
            if health.not_stated {
                "&&&"
            } else if health.other || health.clthp != "0" {
                "122"
            } else {
                "121"
            }
            .to_string(),
        );
        holhp.push(
            if health.not_stated {
                "&&&"
            } else if health.other {
                "111"
            } else {
                "112"
            }
            .to_string(),
        );
        coarasp.push(health_comorbidity_code(
            health.hartp,
            health.hastp,
            health.not_stated,
        ));
        coardbp.push(health_comorbidity_code(
            health.hartp,
            health.hdiap,
            health.not_stated,
        ));
        coarhdp.push(health_comorbidity_code(
            health.hartp,
            health.hhedp,
            health.not_stated,
        ));
        coarmhp.push(health_comorbidity_code(
            health.hartp,
            health.hmhcp,
            health.not_stated,
        ));
        coashdp.push(health_comorbidity_code(
            health.hastp,
            health.hhedp,
            health.not_stated,
        ));
        coaslcp.push(health_comorbidity_code(
            health.hastp,
            health.hlunp,
            health.not_stated,
        ));
        cocnhdp.push(health_comorbidity_code(
            health.hcanp,
            health.hhedp,
            health.not_stated,
        ));
        codbhdp.push(health_comorbidity_code(
            health.hdiap,
            health.hhedp,
            health.not_stated,
        ));
        codbkdp.push(health_comorbidity_code(
            health.hdiap,
            health.hkidp,
            health.not_stated,
        ));
        cohdkdp.push(health_comorbidity_code(
            health.hhedp,
            health.hkidp,
            health.not_stated,
        ));
        cohdmhp.push(health_comorbidity_code(
            health.hhedp,
            health.hmhcp,
            health.not_stated,
        ));
        colcmhp.push(health_comorbidity_code(
            health.hlunp,
            health.hmhcp,
            health.not_stated,
        ));
        hmhcp.push(health_condition_code(
            health.hmhcp,
            "091",
            "092",
            health.not_stated,
        ));
        hartp.push(health_condition_code(
            health.hartp,
            "011",
            "012",
            health.not_stated,
        ));
        hastp.push(health_condition_code(
            health.hastp,
            "021",
            "022",
            health.not_stated,
        ));
        hdiap.push(health_condition_code(
            health.hdiap,
            "051",
            "052",
            health.not_stated,
        ));
        hhedp.push(health_condition_code(
            health.hhedp,
            "061",
            "062",
            health.not_stated,
        ));
        hcanp.push(health_condition_code(
            health.hcanp,
            "031",
            "032",
            health.not_stated,
        ));
        hlunp.push(health_condition_code(
            health.hlunp,
            "081",
            "082",
            health.not_stated,
        ));
        hkidp.push(health_condition_code(
            health.hkidp,
            "071",
            "072",
            health.not_stated,
        ));
        hstrp.push(health_condition_code(
            health.hstrp,
            "101",
            "102",
            health.not_stated,
        ));
        hdemp.push(health_condition_code(
            health.hdemp,
            "041",
            "042",
            health.not_stated,
        ));

        if is_adult {
            volwp.push(weighted_code_str(&mut rng, &VOLWP_WEIGHTS, &VOLWP_CODES_ABS).to_string());
            uncarep.push(
                weighted_code_str(&mut rng, &UNCAREP_WEIGHTS, &UNCAREP_CODES_ABS).to_string(),
            );
            chcarep.push(
                weighted_code_str(&mut rng, &CHCAREP_WEIGHTS_ABS, &CHCAREP_CODES_ABS).to_string(),
            );
            domp.push(weighted_code_str(&mut rng, &DOMP_WEIGHTS, &DOMP_CODES_ABS).to_string());
            adfp.push(weighted_code_str(&mut rng, &ADFP_WEIGHTS_ABS, &ADFP_CODES_ABS).to_string());
        } else {
            volwp.push("@".to_string());
            uncarep.push("@".to_string());
            chcarep.push("@".to_string());
            domp.push("@".to_string());
            adfp.push("@".to_string());
        }

        stup.push(weighted_code_str(&mut rng, &STUP_WEIGHTS_ABS, &STUP_CODES_ABS).to_string());
        let lfsp_i = lfsp.last().unwrap();
        let stup_i = stup.last().unwrap();
        emfp.push(employment_flag_code(lfsp_i).to_string());
        lffp.push(labour_force_participation_code(lfsp_i).to_string());
        uefp.push(unemployment_flag_code(lfsp_i).to_string());
        lfhrp.push(labour_force_hours_code(lfsp_i, hrsp.last().unwrap()).to_string());
        eetp.push(engagement_code(age, lfsp_i, stup_i).to_string());
        assnp.push(weighted_code_str(&mut rng, &ASSNP_WEIGHTS_R, &ASSNP_CODES_R).to_string());
    }

    list!(
        SYNTHETIC_AEUID = synthetic_aeuid,
        SEXP = sexp,
        AGEP = agep,
        AGE5P = age5p,
        AGE10P = age10p,
        IFAGEP = ifagep,
        IFSEXP = ifsexp,
        IFMSTP = ifmstp,
        DOBYP = dobyp,
        STEUCP = steucp,
        INGP = ingp,
        MSTP = mstp,
        BPLP = bplp,
        YARP = yarp,
        YARRP = yarrp,
        LANP = lanp,
        RELP = relp,
        CITP = citp,
        HEAP = heap,
        HSCP = hscp,
        INCP = incp,
        LFSP = lfsp,
        EMFP = emfp,
        LFFP = lffp,
        UEFP = uefp,
        LFHRP = lfhrp,
        EETP = eetp,
        OCCP = occp,
        INDP = indp,
        HRWRP = hrwrp,
        HRSP = hrsp,
        MTWP = mtwp,
        CLTHP = clthp,
        HLTHP = hlthp,
        HOLHP = holhp,
        COARASP = coarasp,
        COARDBP = coardbp,
        COARHDP = coarhdp,
        COARMHP = coarmhp,
        COASHDP = coashdp,
        COASLCP = coaslcp,
        COCNHDP = cocnhdp,
        CODBHDP = codbhdp,
        CODBKDP = codbkdp,
        COHDKDP = cohdkdp,
        COHDMHP = cohdmhp,
        COLCMHP = colcmhp,
        HMHCP = hmhcp,
        HARTP = hartp,
        HASTP = hastp,
        HDIAP = hdiap,
        HHEDP = hhedp,
        HCANP = hcanp,
        HLUNP = hlunp,
        HKIDP = hkidp,
        HSTRP = hstrp,
        HDEMP = hdemp,
        VOLWP = volwp,
        UNCAREP = uncarep,
        CHCAREP = chcarep,
        DOMP = domp,
        ADFP = adfp,
        STUP = stup,
        ASSNP = assnp
    )
}

// ==========================================================================
// Dwelling generation
// ==========================================================================

#[extendr]
fn generate_census_2021_dwelling(n: i32, seed: i32) -> List {
    let n = n as usize;
    let mut rng = StdRng::seed_from_u64(seed as u64);

    let mut dwelling_id: Vec<String> = Vec::with_capacity(n);
    let mut strd: Vec<String> = Vec::with_capacity(n);
    let mut tend: Vec<String> = Vec::with_capacity(n);
    let mut nprd: Vec<String> = Vec::with_capacity(n);
    let mut bedrd: Vec<String> = Vec::with_capacity(n);
    let mut vehrd: Vec<String> = Vec::with_capacity(n);
    let mut steucd: Vec<i32> = Vec::with_capacity(n);
    let mut rntd: Vec<String> = Vec::with_capacity(n);
    let mut mred: Vec<String> = Vec::with_capacity(n);

    for i in 0..n {
        dwelling_id.push(format!("D{:010}", i + 1));

        strd.push(weighted_code_str(&mut rng, &STRD_WEIGHTS_ABS, &STRD_CODES_ABS).to_string());

        let tenure_code = weighted_code_str(&mut rng, &TEND_WEIGHTS_ABS, &TEND_CODES_ABS);
        tend.push(tenure_code.to_string());

        nprd.push(weighted_code_str(&mut rng, &NPRD_WEIGHTS_ABS, &NPRD_CODES_ABS).to_string());

        bedrd.push(weighted_code_str(&mut rng, &BEDRD_WEIGHTS_ABS, &BEDRD_CODES_ABS).to_string());
        vehrd.push(weighted_code_str(&mut rng, &VEHRD_WEIGHTS_ABS, &VEHRD_CODES_ABS).to_string());

        // State (dwelling)
        steucd.push(STATE_CODES[weighted_sample(&mut rng, &STATE_WEIGHTS)]);

        // Rent (only for renters, lognormal around median $375)
        if tenure_code == "4" {
            let median = 375.0_f64;
            let log_mean = median.ln();
            let log_std = 0.5;
            let log_val = log_mean + log_std * rng.gen::<f64>() * 2.0 - log_std;
            let rent = log_val.exp().round() as i32;
            let rent = rent.max(50).min(2000);
            rntd.push(fmt4(rent));
            mred.push("@@@@".to_string());
        } else if tenure_code == "2" {
            let median = 1863.0_f64;
            let log_mean = median.ln();
            let log_std = 0.45;
            let log_val = log_mean + log_std * rng.gen::<f64>() * 2.0 - log_std;
            let mort = log_val.exp().round() as i32;
            let mort = mort.max(200).min(9999);
            mred.push(fmt4(mort));
            rntd.push("@@@@".to_string());
        } else if tenure_code == "&" {
            rntd.push("&&&&".to_string());
            mred.push("&&&&".to_string());
        } else {
            rntd.push("@@@@".to_string());
            mred.push("@@@@".to_string());
        }
    }

    list!(
        DWELLING_ID = dwelling_id,
        STRD = strd,
        TEND = tend,
        NPRD = nprd,
        BEDRD = bedrd,
        VEHRD = vehrd,
        STEUCD = steucd,
        RNTD = rntd,
        MRED = mred
    )
}

// ==========================================================================
// Family generation
// ==========================================================================

#[extendr]
fn generate_census_2021_family(n: i32, seed: i32) -> List {
    let n = n as usize;
    let mut rng = StdRng::seed_from_u64(seed as u64);

    let mut family_id: Vec<String> = Vec::with_capacity(n);
    let mut fmcf: Vec<String> = Vec::with_capacity(n);
    let mut cdcf: Vec<String> = Vec::with_capacity(n);
    let mut cprf: Vec<String> = Vec::with_capacity(n);
    let mut sscf: Vec<String> = Vec::with_capacity(n);

    let children_weights: [f64; 4] = [43.0, 38.0, 14.0, 5.0];
    let ssc_weights: [f64; 2] = [98.5, 1.5];

    for i in 0..n {
        family_id.push(format!("F{:010}", i + 1));

        let comp_idx = weighted_sample(&mut rng, &FMCF_WEIGHTS);
        let comp_code_abs = FMCF_CODES_ABS[comp_idx];
        fmcf.push(comp_code_abs.to_string());

        let n_children = match comp_code_abs {
            "2" | "3" => {
                let c_idx = weighted_sample(&mut rng, &children_weights);
                (c_idx + 1) as i32
            }
            _ => 0,
        };
        let cdcf_code = match comp_code_abs {
            "2" => fmt2(n_children.clamp(1, 6)),
            "3" => fmt2((n_children + 7).clamp(8, 13)),
            _ => "@@".to_string(),
        };
        cdcf.push(cdcf_code);

        let n_persons = match comp_code_abs {
            "2" => 2 + n_children,
            "1" => 2,
            "3" => 1 + n_children,
            _ => uniform_int(&mut rng, 2, 4),
        };
        cprf.push(if n_persons >= 6 {
            "6".to_string()
        } else {
            n_persons.to_string()
        });

        if comp_code_abs == "1" || comp_code_abs == "2" {
            if weighted_sample(&mut rng, &ssc_weights) == 1 {
                sscf.push(if rng.gen::<f64>() < 0.5 { "1" } else { "2" }.to_string());
            } else {
                sscf.push("3".to_string());
            }
        } else {
            sscf.push("@".to_string());
        }
    }

    list!(
        FAMILY_ID = family_id,
        FMCF = fmcf,
        CDCF = cdcf,
        CPRF = cprf,
        SSCF = sscf
    )
}

// Register all Census 2021 functions
extendr_module! {
    mod census_2021;
    fn generate_census_2021_person;
    fn project_census_person__;
    fn generate_census_2021_dwelling;
    fn generate_census_2021_family;
}
