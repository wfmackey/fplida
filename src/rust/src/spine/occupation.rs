use rand::rngs::StdRng;
use rand::Rng;

use super::anzsco_table::ARCHETYPE_RANGES;
use super::Person;
use crate::sampling::{normal_sample, weighted_sample};

// 8 archetype definitions with 6D task score centres.
// Order: cognitive, physical, vision, hearing, manual_dexterity, communication
struct ArchetypeDef {
    scores: [f32; 6], // [cog, phys, vis, hear, mandex, comm]
}

const ARCHETYPES: [ArchetypeDef; 8] = [
    // 0: Labourer (Major 8 + Major 7)
    ArchetypeDef {
        scores: [0.20, 0.75, 0.40, 0.35, 0.55, 0.20],
    },
    // 1: Skilled trade (Major 3, submajor 32-39)
    ArchetypeDef {
        scores: [0.50, 0.70, 0.50, 0.40, 0.70, 0.30],
    },
    // 2: Health worker (Major 4)
    ArchetypeDef {
        scores: [0.50, 0.50, 0.55, 0.55, 0.50, 0.70],
    },
    // 3: Office worker (Major 5)
    ArchetypeDef {
        scores: [0.50, 0.15, 0.55, 0.40, 0.45, 0.45],
    },
    // 4: Professional (Major 2)
    ArchetypeDef {
        scores: [0.75, 0.10, 0.50, 0.40, 0.35, 0.50],
    },
    // 5: Manager (Major 1)
    ArchetypeDef {
        scores: [0.60, 0.10, 0.45, 0.45, 0.30, 0.65],
    },
    // 6: Technical (Major 3, submajor 31)
    ArchetypeDef {
        scores: [0.75, 0.15, 0.55, 0.40, 0.55, 0.25],
    },
    // 7: Service worker (Major 6)
    ArchetypeDef {
        scores: [0.30, 0.20, 0.45, 0.50, 0.35, 0.75],
    },
];

// Education-archetype affinity matrix (5 education levels x 8 archetypes)
// Rows: 1=BelowYr12, 2=Yr12, 3=CertIII-IV, 4=Diploma, 5=Bachelor+
// Cols: 0=Labourer, 1=SkilledTrade, 2=Health, 3=Office, 4=Professional,
//       5=Manager, 6=Technical, 7=Service
// Calibrated so marginal archetype shares ≈ target (9/14/10/13/25/13/7/9%)
// given the cohort-conditioned education distribution.
const EDUCATION_ARCHETYPE_WEIGHTS: [[f64; 8]; 5] = [
    // Below Yr12: heavy Labourer/Service
    [25.0, 14.0, 5.0, 10.0, 3.0, 3.0, 4.0, 20.0],
    // Yr12: spread across Office/Professional
    [8.0, 10.0, 8.0, 22.0, 18.0, 7.0, 5.0, 12.0],
    // Cert III-IV: heavy Trade/Health/Technical
    [8.0, 32.0, 12.0, 12.0, 5.0, 6.0, 10.0, 7.0],
    // Diploma: Professional/Office/Health
    [3.0, 8.0, 14.0, 16.0, 22.0, 14.0, 8.0, 7.0],
    // Bachelor+: heavy Professional/Manager
    [1.0, 2.0, 6.0, 5.0, 58.0, 28.0, 5.0, 1.0],
];

// Industry weights per archetype (19 ANZSIC divisions, codes 1-19)
// Simplified: top industries for each archetype, rest get equal small weights
const INDUSTRY_WEIGHTS: [[f64; 19]; 8] = [
    // 0 Labourer: Manufacturing(3), Construction(5), Transport(9), Agriculture(1)
    [
        8.0, 2.0, 12.0, 2.0, 15.0, 3.0, 5.0, 3.0, 12.0, 2.0, 2.0, 5.0, 3.0, 8.0, 3.0, 3.0, 5.0,
        3.0, 4.0,
    ],
    // 1 Skilled trade: Construction(5), Manufacturing(3), Mining(2)
    [
        3.0, 5.0, 15.0, 3.0, 30.0, 2.0, 5.0, 2.0, 3.0, 2.0, 2.0, 3.0, 5.0, 5.0, 2.0, 3.0, 5.0, 3.0,
        2.0,
    ],
    // 2 Health worker: Health care(14), Education(13), Social assistance
    [
        1.0, 1.0, 2.0, 1.0, 1.0, 1.0, 2.0, 5.0, 1.0, 1.0, 1.0, 1.0, 5.0, 50.0, 1.0, 5.0, 5.0, 10.0,
        6.0,
    ],
    // 3 Office worker: Financial(10), Public admin(15), Professional services(12)
    [
        1.0, 1.0, 3.0, 1.0, 2.0, 3.0, 5.0, 5.0, 5.0, 10.0, 5.0, 8.0, 12.0, 8.0, 15.0, 3.0, 5.0,
        5.0, 3.0,
    ],
    // 4 Professional: Professional services(12), Health(14), Education(13)
    [
        1.0, 2.0, 3.0, 2.0, 2.0, 2.0, 5.0, 8.0, 2.0, 5.0, 3.0, 5.0, 15.0, 15.0, 8.0, 5.0, 8.0, 5.0,
        4.0,
    ],
    // 5 Manager: Retail(7), Construction(5), Professional services(12)
    [
        5.0, 3.0, 5.0, 2.0, 8.0, 3.0, 10.0, 8.0, 5.0, 8.0, 3.0, 5.0, 10.0, 5.0, 5.0, 3.0, 5.0, 3.0,
        4.0,
    ],
    // 6 Technical: Professional services(12), Manufacturing(3), Mining(2)
    [
        2.0, 8.0, 10.0, 5.0, 8.0, 2.0, 3.0, 8.0, 3.0, 3.0, 2.0, 5.0, 15.0, 5.0, 5.0, 3.0, 8.0, 3.0,
        2.0,
    ],
    // 7 Service worker: Retail(7), Accommodation/food(8), Arts(16)
    [
        1.0, 1.0, 2.0, 1.0, 1.0, 1.0, 25.0, 25.0, 3.0, 3.0, 3.0, 5.0, 3.0, 3.0, 3.0, 8.0, 5.0, 5.0,
        3.0,
    ],
];

// Sector: Private(1) / Public(2) probability of public, by archetype
const PUBLIC_SECTOR_RATE: [f64; 8] = [
    0.15, // Labourer
    0.10, // Skilled trade
    0.40, // Health worker
    0.30, // Office worker
    0.25, // Professional
    0.20, // Manager
    0.20, // Technical
    0.10, // Service worker
];

const JITTER_SD: f64 = 0.08;

pub fn assign(person: &mut Person, rng: &mut StdRng) {
    if person.education == 0 {
        return; // children / not applicable
    }

    let edu_idx = (person.education - 1) as usize; // 0-4
    let weights = &EDUCATION_ARCHETYPE_WEIGHTS[edu_idx];
    let arch = weighted_sample(rng, weights);
    person.archetype = arch as u8;

    // Pick a random ANZSCO code within this archetype's range
    let (start, end) = ARCHETYPE_RANGES[arch];
    let code_idx = rng.gen_range(start..end);
    person.anzsco_index = code_idx as u16;

    // Jitter 6D task scores around archetype centre
    let centres = &ARCHETYPES[arch].scores;
    person.task_cognitive = jitter(rng, centres[0]);
    person.task_physical = jitter(rng, centres[1]);
    person.task_vision = jitter(rng, centres[2]);
    person.task_hearing = jitter(rng, centres[3]);
    person.task_manual_dexterity = jitter(rng, centres[4]);
    person.task_communication = jitter(rng, centres[5]);

    // Industry
    let ind_weights = &INDUSTRY_WEIGHTS[arch];
    person.industry = (weighted_sample(rng, ind_weights) + 1) as u8; // 1-19

    // Sector
    person.sector = if rng.gen::<f64>() < PUBLIC_SECTOR_RATE[arch] {
        2
    } else {
        1
    };
}

fn jitter(rng: &mut StdRng, centre: f32) -> f32 {
    let val = centre as f64 + normal_sample(rng, 0.0, JITTER_SD);
    val.clamp(0.0, 1.0) as f32
}
