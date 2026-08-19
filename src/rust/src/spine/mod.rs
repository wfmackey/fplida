use extendr_api::prelude::*;
use rand::prelude::*;
use rand::rngs::StdRng;
use rand::SeedableRng;

pub mod anzsco_table;
mod demographics;
mod education;
mod geography;
mod income;
mod linkage;
mod occupation;

use crate::disability;
use crate::sampling::weighted_sample;
use crate::seeds;
use crate::service_profiles;
use anzsco_table::{ANZSCO_CODES, ANZSCO_TITLES};

/// A synthetic person in the fplida spine.
#[derive(Default)]
pub struct Person {
    pub id: u32,

    // Demographics
    pub birth_year: i32,
    pub sex: u8,
    pub state: u8,
    pub indigenous: u8,
    pub country_of_birth: u8,
    /// SACC 2016 country-of-birth code. 1101 if born in Australia, else a
    /// real overseas country code. The authoritative cross-dataset country
    /// identity; `country_of_birth` above is the legacy 0/1 born-overseas flag.
    pub country_of_birth_sacc: i32,
    pub year_of_arrival: Option<i32>,
    pub citizenship: u8,
    pub month_of_birth: i32,
    pub year_of_death: Option<i32>,
    pub month_of_death: Option<i32>,
    pub day_of_death: Option<i32>,
    pub residence_seed: i32,

    // Geography (ASGS 2021): SA2 of usual residence, nesting in `state`.
    // SA3/SA4 are derived from sa2 on emit (sa2/10_000, sa2/1_000_000).
    pub sa2: u32,

    // Education
    pub education: u8,

    // Occupation
    pub archetype: u8,
    pub anzsco_index: u16,
    pub task_cognitive: f32,
    pub task_physical: f32,
    pub task_vision: f32,
    pub task_hearing: f32,
    pub task_manual_dexterity: f32,
    pub task_communication: f32,
    pub industry: u8,
    pub sector: u8,

    // Income
    pub individual_fe: f32,
    pub occ_match_quality: f32,
    pub baseline_employed: bool,
    pub baseline_hours: u8,
    pub baseline_income: f32,

    // Linkage
    pub has_spine_link: bool,

    // Disability (Phase 2)
    pub disability_onset_year: Option<i32>,
    pub disability_type: Option<i32>, // SDAC type derived from person_type
    pub disability_severity: Option<i32>, // 1=profound, 2=moderate, 3=mild, 4=condition
    pub is_dc: Option<bool>,          // true=DC, false=NC
    pub disability_dose: Option<f64>, // occupation score on affected dimension
    pub person_type: Option<u8>,      // 1-27 condition person type
    pub comorbidity_flags: Option<i32>, // bitmask: bit0=dep, bit1=diabetes, bit2=cvd, bit3=pain, bit4=anxiety
}

/// Build a Vec<Person> of length n with the given seed. Pure Rust,
/// no R interop. Used both by the classic `generate_spine` extendr
/// entry point and by the spine template machinery.
pub fn build_persons(n: usize, seed: i32) -> Vec<Person> {
    let base_seed = seed as u64;

    // Separate sub-RNGs per step so adding later steps doesn't change earlier
    // outputs. Offsets are registered in `seeds::spine`; new attribute groups
    // claim a fresh offset and leave every existing column bit-identical.
    let mut rng_demo = StdRng::seed_from_u64(base_seed.wrapping_add(seeds::spine::DEMOGRAPHICS));
    let mut rng_edu = StdRng::seed_from_u64(base_seed.wrapping_add(seeds::spine::EDUCATION));
    let mut rng_occ = StdRng::seed_from_u64(base_seed.wrapping_add(seeds::spine::OCCUPATION));
    let mut rng_inc = StdRng::seed_from_u64(base_seed.wrapping_add(seeds::spine::INCOME));
    let mut rng_link = StdRng::seed_from_u64(base_seed.wrapping_add(seeds::spine::LINKAGE));
    let mut rng_dis = StdRng::seed_from_u64(base_seed.wrapping_add(seeds::spine::DISABILITY));
    let mut rng_geo = StdRng::seed_from_u64(base_seed.wrapping_add(seeds::spine::GEOGRAPHY));
    let mut rng_cob = StdRng::seed_from_u64(base_seed.wrapping_add(seeds::spine::COUNTRY_SACC));
    let mut rng_vitals = StdRng::seed_from_u64(base_seed.wrapping_add(seeds::spine::VITALS));
    let mut rng_residence = StdRng::seed_from_u64(base_seed.wrapping_add(seeds::spine::RESIDENCE));

    let mut persons: Vec<Person> = Vec::with_capacity(n);
    for i in 0..n {
        let mut p = Person::default();
        p.id = i as u32;
        demographics::assign(&mut p, &mut rng_demo);
        // Independent of rng_demo so existing demographics are unchanged.
        demographics::assign_country_sacc(&mut p, &mut rng_cob);
        demographics::assign_vitals(&mut p, &mut rng_vitals);
        p.residence_seed = rng_residence.gen_range(1..=i32::MAX);
        geography::assign(&mut p, &mut rng_geo);
        education::assign(&mut p, &mut rng_edu);
        occupation::assign(&mut p, &mut rng_occ);
        income::assign(&mut p, &mut rng_inc);
        linkage::assign(&mut p, &mut rng_link);
        assign_disability(&mut p, &mut rng_dis);
        persons.push(p);
    }
    persons
}

/// Generate the person spine.
/// Returns a list of column vectors suitable for conversion to a data.frame.
#[extendr]
fn generate_spine(n: i32, seed: i32) -> List {
    let mut persons = build_persons(n as usize, seed);
    let birth_years: Vec<i32> = persons.iter().map(|p| p.birth_year).collect();
    let states: Vec<u8> = persons.iter().map(|p| p.state).collect();
    let household_ids = assign_household_ids(&birth_years, &states, seed);
    let dwelling_ids = assign_dwelling_ids(&household_ids, seed);
    align_household_geography(&mut persons, &household_ids, seed);
    to_r_list(&persons, &household_ids, &dwelling_ids)
}

/// Put a household at one address.
///
/// Every person draws an SA2 of their own in `geography::assign`, which is the
/// right marginal and the wrong joint: a couple ends up in two different SA2s,
/// so no product can treat a household as a place. This gives the household one
/// of its members' SA2s and writes it to all of them, which leaves the SA2
/// distribution close to what it was — the anchor is drawn from the same
/// population-weighted pool — while making co-residence mean something.
///
/// The anchor is the household's oldest member, not a random one, so the
/// address follows the person most likely to hold the tenancy, and the choice
/// is deterministic in the household rather than in the draw order.
pub fn align_household_geography(persons: &mut [Person], household_ids: &[i32], _seed: i32) {
    use std::collections::HashMap;
    let mut anchor: HashMap<i32, (i32, u32)> = HashMap::new();
    for (i, p) in persons.iter().enumerate() {
        let household = match household_ids.get(i) {
            Some(&h) if h != 0 => h,
            _ => continue,
        };
        // No usable SA2 cannot anchor anybody: `sa2 == 0` is the package's
        // "no geography" marker and spreading it over a household would erase
        // geography the other members do have.
        if p.sa2 == 0 {
            continue;
        }
        let age = 2021 - p.birth_year;
        let entry = anchor.entry(household).or_insert((age, p.sa2));
        if age > entry.0 {
            *entry = (age, p.sa2);
        }
    }
    for (i, p) in persons.iter_mut().enumerate() {
        if let Some(&h) = household_ids.get(i) {
            if let Some(&(_, sa2)) = anchor.get(&h) {
                p.sa2 = sa2;
            }
        }
    }
}

/// The household-anchored SA2, for the template path.
///
/// Same rule as `align_household_geography`, over plain column vectors rather
/// than `Person`s, because the template path samples rows into Arrow arrays and
/// never builds a `Person`. Keep the two in step.
pub fn align_household_sa2(sa2: &[i32], birth_year: &[i32], household_ids: &[i32]) -> Vec<i32> {
    use std::collections::HashMap;
    let mut anchor: HashMap<i32, (i32, i32)> = HashMap::new();
    for i in 0..sa2.len() {
        let household = match household_ids.get(i) {
            Some(&h) if h != 0 => h,
            _ => continue,
        };
        if sa2[i] <= 0 {
            continue;
        }
        let age = 2021 - birth_year.get(i).copied().unwrap_or(2021);
        let entry = anchor.entry(household).or_insert((age, sa2[i]));
        if age > entry.0 {
            *entry = (age, sa2[i]);
        }
    }
    (0..sa2.len())
        .map(|i| match household_ids.get(i).and_then(|h| anchor.get(h)) {
            Some(&(_, anchored)) => anchored,
            None => sa2[i],
        })
        .collect()
}

/// Assign a household id to each of `n` output persons, grouping them into
/// realistic households: ~55% of adults pair into age-matched couples, the
/// rest are single / single-parent households, and children are attached to a
/// parenting-age household (capped at 4 per household). Households are numbered
/// contiguously from 1 so the Census dwelling/family tables can be sized to
/// match. Assigned at OUTPUT time on the full population (not stored on the
/// template), so households never split across build slices.
///
/// A household is formed WITHIN a state. Pairing on age alone put 754 of 1,529
/// households across more than one state, which is why nothing downstream could
/// treat a household as a place: co-residents drew their geography
/// independently, so "these people live together" and "these people live here"
/// were different claims. Nobody's state changes here — the pairing is
/// restricted, not the draw — so the state marginal is exactly what
/// `demographics::assign` produced.
pub fn assign_household_ids(birth_year: &[i32], state: &[u8], seed: i32) -> Vec<i32> {
    let n = birth_year.len();
    let mut rng = StdRng::seed_from_u64((seed as u64).wrapping_add(seeds::spine::HOUSEHOLD));
    let mut hh = vec![0i32; n];
    let age = |i: usize| 2021 - birth_year[i];
    let state_of = |i: usize| state.get(i).copied().unwrap_or(0);

    let adults: Vec<usize> = (0..n).filter(|&i| age(i) >= 18).collect();
    let children: Vec<usize> = (0..n).filter(|&i| age(i) < 18).collect();

    // Sorted by state first, then by a jittered birth year, so couples are
    // close in age and always in the same state.
    let mut keyed: Vec<(u8, i32, usize)> = adults
        .iter()
        .map(|&i| (state_of(i), birth_year[i] + rng.gen_range(-4..=4), i))
        .collect();
    keyed.sort_by_key(|&(s, k, _)| (s, k));
    let sorted_adults: Vec<usize> = keyed.into_iter().map(|(_, _, i)| i).collect();

    let mut next: i32 = 1;
    // Households that can take children, bucketed by state so a child is never
    // attached to a household in another state.
    let mut parent_hh: Vec<Vec<i32>> = vec![Vec::new(); 10];
    let mut k = 0usize;
    while k < sorted_adults.len() {
        let a = sorted_adults[k];
        let state_a = state_of(a) as usize;
        // The sort leaves one adult at each state boundary whose neighbour is
        // in the next state. Checking the state here is what stops them pairing
        // across it.
        let pairs = k + 1 < sorted_adults.len()
            && state_of(sorted_adults[k + 1]) == state_of(a)
            && rng.gen::<f64>() < 0.55;
        if pairs {
            let b = sorted_adults[k + 1];
            hh[a] = next;
            hh[b] = next;
            if age(a).min(age(b)) <= 55 && state_a < parent_hh.len() {
                parent_hh[state_a].push(next);
            }
            next += 1;
            k += 2;
        } else {
            hh[a] = next;
            if (25..=55).contains(&age(a))
                && state_a < parent_hh.len()
                && rng.gen::<f64>() < 0.20
            {
                parent_hh[state_a].push(next);
            }
            next += 1;
            k += 1;
        }
    }

    // Children attach to a parenting-age household in their own state, capped
    // at 4 children each. A child whose state has no parenting-age household
    // becomes their own household rather than moving interstate.
    let mut sc = children.clone();
    for i in (1..sc.len()).rev() {
        let j = rng.gen_range(0..=i);
        sc.swap(i, j);
    }
    let mut counts: std::collections::HashMap<i32, u8> = std::collections::HashMap::new();
    for &child in &sc {
        let s = state_of(child) as usize;
        let pool = if s < parent_hh.len() {
            &parent_hh[s]
        } else {
            &parent_hh[0]
        };
        if pool.is_empty() {
            hh[child] = next;
            next += 1;
            continue;
        }
        let mut h = pool[rng.gen_range(0..pool.len())];
        let mut tries = 0;
        while *counts.get(&h).unwrap_or(&0) >= 4 && tries < 8 {
            h = pool[rng.gen_range(0..pool.len())];
            tries += 1;
        }
        *counts.entry(h).or_insert(0) += 1;
        hh[child] = h;
    }
    hh
}

/// The dwelling each person lives in, one per household.
///
/// `household_id` says these people are a household; `dwelling_id` says they
/// live at this address, and it is what every residential identifier is keyed
/// on. The two are one to one today. They are kept apart because they are
/// different claims: a dwelling outlives the household in it, and a later model
/// that lets one address house successive households needs somewhere to say so.
///
/// Scattered rather than copied so the identifier space does not overlap
/// `household_id`, whose small contiguous integers would collide with it in any
/// key built from both. Multiplying by an odd constant modulo 2^31 is a
/// bijection, which matters more than it looks: a drawn identifier would
/// collide by the birthday bound long before the population is interesting —
/// five million households drawn from two billion values produce some six
/// thousand collisions — and two households sharing a dwelling identifier would
/// read as co-residence, which is the one thing this column exists to state.
///
/// The result stays under 2^31, so it survives the double-precision arithmetic
/// in `.address_key_hex()`, which is exact only below 2^53 and multiplies by
/// 1000003.
pub fn assign_dwelling_ids(household_ids: &[i32], seed: i32) -> Vec<i32> {
    const SCATTER: u32 = 2_654_435_761; // odd, so the map is invertible
    let salt = (seed as u32).wrapping_mul(0x9E37_79B9);
    household_ids
        .iter()
        .map(|&household| {
            // 0 is reserved for "no dwelling", which is what a person with no
            // household carries. Adding one to a 30-bit result keeps the map
            // injective and clear of it; masking to 31 bits and forcing the low
            // bit would not, since it would fold two households onto one
            // dwelling.
            if household == 0 {
                return 0;
            }
            let scattered = (household as u32)
                .wrapping_add(salt)
                .wrapping_mul(SCATTER)
                & 0x3FFF_FFFF;
            (scattered + 1) as i32
        })
        .collect()
}

/// Assign disability/CHC onset to an eligible person using person-type-first logic.
/// ~18% of employed working-age adults with occupations get disability/CHC.
/// Severity 1-2 (profound/moderate) drive employment effects + DSP.
/// Severity 3-4 (mild/condition-only) drive condition-specific MBS/PBS only.
/// Onset years staggered 2010-2020.
fn assign_disability(p: &mut Person, rng: &mut StdRng) {
    let age_2015 = 2015 - p.birth_year; // midpoint of onset window
    let eligible = p.baseline_employed && p.education > 0 && age_2015 >= 25 && age_2015 <= 60;

    if !eligible || rng.gen::<f64>() >= 0.18 {
        return;
    }

    // Onset year: uniform 2010-2020
    let onset_year = 2010 + (rng.gen::<f64>() * 11.0).min(10.0) as i32;

    // Check working age at onset
    let age_at_onset = onset_year - p.birth_year;
    if age_at_onset < 25 || age_at_onset > 60 {
        return;
    }

    // Draw PERSON TYPE (1-27) from prevalence weights
    let pt = service_profiles::draw_person_type(rng);
    let profile = &service_profiles::PROFILES[(pt - 1) as usize];

    // Derive SDAC type from person type profile
    let sdac_type = profile.sdac_primary;

    // Draw severity (1-4)
    let sev_idx = weighted_sample(rng, disability::SEVERITY_WEIGHTS);
    let severity = (sev_idx + 1) as i32;

    // Classify DC/NC using person type's primary dimension
    let scores = [
        p.task_cognitive as f64,
        p.task_physical as f64,
        p.task_vision as f64,
        p.task_hearing as f64,
        p.task_manual_dexterity as f64,
        p.task_communication as f64,
    ];
    let is_dc = disability::classify_dc_by_dim(profile.dim_primary, &scores);
    let dose = disability::get_dose_by_dim(profile.dim_primary, &scores);

    // Draw comorbidity flags
    let comorb_flags = service_profiles::draw_comorbidity_flags(rng, profile.comorbidity_eligible);

    p.disability_onset_year = Some(onset_year);
    p.disability_type = Some(sdac_type);
    p.disability_severity = Some(severity);
    p.is_dc = Some(is_dc);
    p.disability_dose = Some(dose);
    p.person_type = Some(pt);
    p.comorbidity_flags = Some(comorb_flags);
}

fn to_r_list(persons: &[Person], household_ids: &[i32], dwelling_ids: &[i32]) -> List {
    let n = persons.len();

    let mut id: Vec<String> = Vec::with_capacity(n);
    let mut spine_id: Vec<Option<String>> = Vec::with_capacity(n);
    let mut birth_year: Vec<i32> = Vec::with_capacity(n);
    let mut sex: Vec<i32> = Vec::with_capacity(n);
    let mut state: Vec<i32> = Vec::with_capacity(n);
    let mut indigenous: Vec<i32> = Vec::with_capacity(n);
    let mut country_of_birth: Vec<i32> = Vec::with_capacity(n);
    let mut country_of_birth_sacc: Vec<i32> = Vec::with_capacity(n);
    let mut year_of_arrival: Vec<Rint> = Vec::with_capacity(n);
    let mut citizenship: Vec<i32> = Vec::with_capacity(n);
    let mut month_of_birth: Vec<i32> = Vec::with_capacity(n);
    let mut year_of_death: Vec<Rint> = Vec::with_capacity(n);
    let mut month_of_death: Vec<Rint> = Vec::with_capacity(n);
    let mut day_of_death: Vec<Rint> = Vec::with_capacity(n);
    let mut residence_seed: Vec<i32> = Vec::with_capacity(n);
    let mut sa2_code: Vec<i32> = Vec::with_capacity(n);
    let mut sa3_code: Vec<i32> = Vec::with_capacity(n);
    let mut sa4_code: Vec<i32> = Vec::with_capacity(n);
    let mut education: Vec<i32> = Vec::with_capacity(n);
    let mut archetype: Vec<i32> = Vec::with_capacity(n);
    let mut anzsco_code: Vec<i32> = Vec::with_capacity(n);
    let mut anzsco_unit: Vec<i32> = Vec::with_capacity(n);
    let mut anzsco_major: Vec<i32> = Vec::with_capacity(n);
    let mut anzsco_title: Vec<Option<String>> = Vec::with_capacity(n);
    let mut skill_level: Vec<i32> = Vec::with_capacity(n);
    let mut task_cognitive: Vec<f64> = Vec::with_capacity(n);
    let mut task_physical: Vec<f64> = Vec::with_capacity(n);
    let mut task_vision: Vec<f64> = Vec::with_capacity(n);
    let mut task_hearing: Vec<f64> = Vec::with_capacity(n);
    let mut task_manual_dexterity: Vec<f64> = Vec::with_capacity(n);
    let mut task_communication: Vec<f64> = Vec::with_capacity(n);
    let mut industry: Vec<i32> = Vec::with_capacity(n);
    let mut sector: Vec<i32> = Vec::with_capacity(n);
    let mut individual_fe: Vec<f64> = Vec::with_capacity(n);
    let mut occ_match_quality: Vec<f64> = Vec::with_capacity(n);
    let mut baseline_employed: Vec<bool> = Vec::with_capacity(n);
    let mut baseline_hours: Vec<i32> = Vec::with_capacity(n);
    let mut baseline_income: Vec<f64> = Vec::with_capacity(n);

    // Disability columns
    let mut dis_onset_year: Vec<Rint> = Vec::with_capacity(n);
    let mut dis_type: Vec<Rint> = Vec::with_capacity(n);
    let mut dis_severity: Vec<Rint> = Vec::with_capacity(n);
    let mut dis_is_dc: Vec<Rbool> = Vec::with_capacity(n);
    let mut dis_dose: Vec<Rfloat> = Vec::with_capacity(n);
    let mut dis_person_type: Vec<Rint> = Vec::with_capacity(n);
    let mut dis_comorbidity: Vec<Rint> = Vec::with_capacity(n);

    for p in persons {
        id.push(format!("P{:010}", p.id + 1));
        spine_id.push(if p.has_spine_link {
            Some(format!("SP{:010}", p.id + 1))
        } else {
            None
        });
        birth_year.push(p.birth_year);
        sex.push(p.sex as i32);
        state.push(p.state as i32);
        indigenous.push(p.indigenous as i32);
        country_of_birth.push(p.country_of_birth as i32);
        country_of_birth_sacc.push(p.country_of_birth_sacc);
        year_of_arrival.push(match p.year_of_arrival {
            Some(y) => Rint::from(y),
            None => Rint::na(),
        });
        citizenship.push(p.citizenship as i32);
        month_of_birth.push(p.month_of_birth);
        year_of_death.push(match p.year_of_death {
            Some(y) => Rint::from(y),
            None => Rint::na(),
        });
        month_of_death.push(match p.month_of_death {
            Some(m) => Rint::from(m),
            None => Rint::na(),
        });
        day_of_death.push(match p.day_of_death {
            Some(d) => Rint::from(d),
            None => Rint::na(),
        });
        residence_seed.push(p.residence_seed);
        sa2_code.push(p.sa2 as i32);
        sa3_code.push((p.sa2 / 10_000) as i32);
        sa4_code.push((p.sa2 / 1_000_000) as i32);
        education.push(p.education as i32);
        archetype.push(p.archetype as i32);

        // Look up ANZSCO details from the table
        if p.education > 0 && (p.anzsco_index as usize) < ANZSCO_CODES.len() {
            let entry = &ANZSCO_CODES[p.anzsco_index as usize];
            anzsco_code.push(entry.code as i32);
            anzsco_unit.push(entry.unit_code as i32);
            anzsco_major.push(entry.major as i32);
            anzsco_title.push(Some(ANZSCO_TITLES[p.anzsco_index as usize].to_string()));
            skill_level.push(entry.skill_level as i32);
        } else {
            anzsco_code.push(0);
            anzsco_unit.push(0);
            anzsco_major.push(0);
            anzsco_title.push(None);
            skill_level.push(0);
        }

        task_cognitive.push(p.task_cognitive as f64);
        task_physical.push(p.task_physical as f64);
        task_vision.push(p.task_vision as f64);
        task_hearing.push(p.task_hearing as f64);
        task_manual_dexterity.push(p.task_manual_dexterity as f64);
        task_communication.push(p.task_communication as f64);
        industry.push(p.industry as i32);
        sector.push(p.sector as i32);
        individual_fe.push(p.individual_fe as f64);
        occ_match_quality.push(p.occ_match_quality as f64);
        baseline_employed.push(p.baseline_employed);
        baseline_hours.push(p.baseline_hours as i32);
        baseline_income.push(p.baseline_income as f64);

        // Disability columns
        match p.disability_onset_year {
            Some(y) => {
                dis_onset_year.push(Rint::from(y));
                dis_type.push(Rint::from(p.disability_type.unwrap_or(0)));
                dis_severity.push(Rint::from(p.disability_severity.unwrap_or(0)));
                dis_is_dc.push(Rbool::from(p.is_dc.unwrap_or(false)));
                dis_dose.push(Rfloat::from(p.disability_dose.unwrap_or(0.0)));
                dis_person_type.push(match p.person_type {
                    Some(pt) => Rint::from(pt as i32),
                    None => Rint::na(),
                });
                dis_comorbidity.push(match p.comorbidity_flags {
                    Some(f) => Rint::from(f),
                    None => Rint::na(),
                });
            }
            None => {
                dis_onset_year.push(Rint::na());
                dis_type.push(Rint::na());
                dis_severity.push(Rint::na());
                dis_is_dc.push(Rbool::na());
                dis_dose.push(Rfloat::na());
                dis_person_type.push(Rint::na());
                dis_comorbidity.push(Rint::na());
            }
        }
    }

    list!(
        id = id,
        spine_id = spine_id,
        birth_year = birth_year,
        sex = sex,
        state = state,
        indigenous = indigenous,
        country_of_birth = country_of_birth,
        country_of_birth_sacc = country_of_birth_sacc,
        year_of_arrival = year_of_arrival,
        citizenship = citizenship,
        month_of_birth = month_of_birth,
        year_of_death = year_of_death,
        month_of_death = month_of_death,
        day_of_death = day_of_death,
        residence_seed = residence_seed,
        sa2_code = sa2_code,
        sa3_code = sa3_code,
        sa4_code = sa4_code,
        education = education,
        archetype = archetype,
        anzsco_code = anzsco_code,
        anzsco_unit = anzsco_unit,
        anzsco_major = anzsco_major,
        anzsco_title = anzsco_title,
        skill_level = skill_level,
        task_cognitive = task_cognitive,
        task_physical = task_physical,
        task_vision = task_vision,
        task_hearing = task_hearing,
        task_manual_dexterity = task_manual_dexterity,
        task_communication = task_communication,
        industry = industry,
        sector = sector,
        individual_fe = individual_fe,
        occ_match_quality = occ_match_quality,
        baseline_employed = baseline_employed,
        baseline_hours = baseline_hours,
        baseline_income = baseline_income,
        disability_onset_year = dis_onset_year,
        disability_type = dis_type,
        disability_severity = dis_severity,
        is_dc = dis_is_dc,
        disability_dose = dis_dose,
        person_type = dis_person_type,
        comorbidity_flags = dis_comorbidity,
        household_id = household_ids.to_vec(),
        dwelling_id = dwelling_ids.to_vec()
    )
}

extendr_module! {
    mod spine;
    fn generate_spine;
}
