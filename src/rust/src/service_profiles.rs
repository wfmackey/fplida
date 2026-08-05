// ============================================================================
// service_profiles.rs — Disability person type profiles
//
// Defines 27 person types with SDAC mappings, 6D capability dimensions,
// onset trajectories, DSP/NDIS probabilities, and comorbidity eligibility.
//
// Previously contained condition-specific PBS/MBS signatures for each type.
// Those were removed to simplify generation — MBS/PBS now use age-driven
// baseline utilisation only.
// ============================================================================

use crate::sampling::weighted_sample;
use rand::rngs::StdRng;
use rand::Rng;

// ============================================================================
// Type definitions
// ============================================================================

/// Onset sharpness category for temporal trajectory.
#[derive(Clone, Copy, PartialEq)]
pub enum OnsetType {
    Acute,    // SCI, stroke, amputation — sharp onset
    Subacute, // First psychotic episode, seizure, heart failure decompensation
    Gradual,  // Chronic pain, diabetes, COPD, depression — slow ramp
    Lifelong, // Intellectual disability, autism, T1 diabetes — always present
}

/// A complete person type profile.
pub struct PersonTypeProfile {
    pub id: u8,
    pub label: &'static str,
    pub sdac_primary: i32,  // primary SDAC disability type
    pub dim_primary: usize, // 6D index: 0=cognitive, 1=physical, 2=vision, 3=hearing, 4=manual_dex, 5=communication
    pub dim_secondary: i32, // -1 = none, else 6D index
    pub onset_sharpness: OnsetType,
    pub dsp_probs: [f64; 4], // [profound/severe, moderate, mild, condition_only]
    pub ndis_probs: [f64; 4],
    pub comorbidity_eligible: u8, // bitmask: bit0=depression, bit1=diabetes, bit2=cvd, bit3=pain, bit4=anxiety
}

// ============================================================================
// Comorbidity bitmask constants
// ============================================================================

pub const COMORB_DEPRESSION: u8 = 1; // bit 0
pub const COMORB_DIABETES: u8 = 2; // bit 1
pub const COMORB_CVD: u8 = 4; // bit 2
pub const COMORB_PAIN: u8 = 8; // bit 3
pub const COMORB_ANXIETY: u8 = 16; // bit 4

/// Comorbidity draw rates (applied when eligible flag is set)
pub const COMORB_RATES: [(u8, f64); 5] = [
    (COMORB_DEPRESSION, 0.30),
    (COMORB_DIABETES, 0.15),
    (COMORB_CVD, 0.25),
    (COMORB_PAIN, 0.20),
    (COMORB_ANXIETY, 0.20),
];

// ============================================================================
// Master profiles array (indexed by person_type - 1)
// ============================================================================

pub const PROFILES: [PersonTypeProfile; 27] = [
    // Type 1: Chronic Back Pain
    PersonTypeProfile {
        id: 1,
        label: "CHRONIC_BACK_PAIN",
        sdac_primary: 5,
        dim_primary: 1,
        dim_secondary: 4,
        onset_sharpness: OnsetType::Gradual,
        dsp_probs: [0.50, 0.20, 0.05, 0.00],
        ndis_probs: [0.15, 0.03, 0.00, 0.00],
        comorbidity_eligible: 21, // dep+anx+cvd
    },
    // Type 2: Depression / Anxiety
    PersonTypeProfile {
        id: 2,
        label: "DEPRESSION_ANXIETY",
        sdac_primary: 11,
        dim_primary: 5,
        dim_secondary: 0,
        onset_sharpness: OnsetType::Gradual,
        dsp_probs: [0.40, 0.15, 0.03, 0.00],
        ndis_probs: [0.10, 0.02, 0.00, 0.00],
        comorbidity_eligible: 12, // pain+cvd
    },
    // Type 3: Schizophrenia
    PersonTypeProfile {
        id: 3,
        label: "SCHIZOPHRENIA",
        sdac_primary: 14,
        dim_primary: 0,
        dim_secondary: 5,
        onset_sharpness: OnsetType::Subacute,
        dsp_probs: [0.70, 0.45, 0.15, 0.00],
        ndis_probs: [0.35, 0.15, 0.05, 0.00],
        comorbidity_eligible: 1, // dep
    },
    // Type 4: Bipolar Disorder
    PersonTypeProfile {
        id: 4,
        label: "BIPOLAR_DISORDER",
        sdac_primary: 14,
        dim_primary: 0,
        dim_secondary: 5,
        onset_sharpness: OnsetType::Subacute,
        dsp_probs: [0.45, 0.20, 0.05, 0.00],
        ndis_probs: [0.10, 0.03, 0.00, 0.00],
        comorbidity_eligible: 17, // dep+anx
    },
    // Type 5: Type 2 Diabetes
    PersonTypeProfile {
        id: 5,
        label: "TYPE2_DIABETES",
        sdac_primary: 5,
        dim_primary: 1,
        dim_secondary: 2,
        onset_sharpness: OnsetType::Gradual,
        dsp_probs: [0.25, 0.08, 0.02, 0.00],
        ndis_probs: [0.05, 0.01, 0.00, 0.00],
        comorbidity_eligible: 5, // dep+cvd
    },
    // Type 6: Type 1 Diabetes
    PersonTypeProfile {
        id: 6,
        label: "TYPE1_DIABETES",
        sdac_primary: 5,
        dim_primary: 1,
        dim_secondary: 2,
        onset_sharpness: OnsetType::Lifelong,
        dsp_probs: [0.20, 0.05, 0.01, 0.00],
        ndis_probs: [0.05, 0.01, 0.00, 0.00],
        comorbidity_eligible: 5, // dep+cvd
    },
    // Type 7: COPD / Respiratory
    PersonTypeProfile {
        id: 7,
        label: "COPD_RESPIRATORY",
        sdac_primary: 4,
        dim_primary: 1,
        dim_secondary: -1,
        onset_sharpness: OnsetType::Gradual,
        dsp_probs: [0.45, 0.15, 0.03, 0.00],
        ndis_probs: [0.08, 0.02, 0.00, 0.00],
        comorbidity_eligible: 21, // dep+anx+cvd
    },
    // Type 8: Rheumatoid Arthritis
    PersonTypeProfile {
        id: 8,
        label: "RHEUMATOID_ARTHRITIS",
        sdac_primary: 5,
        dim_primary: 4,
        dim_secondary: 1,
        onset_sharpness: OnsetType::Gradual,
        dsp_probs: [0.35, 0.12, 0.03, 0.00],
        ndis_probs: [0.10, 0.03, 0.00, 0.00],
        comorbidity_eligible: 13, // dep+pain+cvd
    },
    // Type 9: Epilepsy
    PersonTypeProfile {
        id: 9,
        label: "EPILEPSY",
        sdac_primary: 6,
        dim_primary: 1,
        dim_secondary: 0,
        onset_sharpness: OnsetType::Subacute,
        dsp_probs: [0.40, 0.15, 0.05, 0.00],
        ndis_probs: [0.15, 0.05, 0.01, 0.00],
        comorbidity_eligible: 17, // dep+anx
    },
    // Type 10: Multiple Sclerosis
    PersonTypeProfile {
        id: 10,
        label: "MULTIPLE_SCLEROSIS",
        sdac_primary: 10,
        dim_primary: 1,
        dim_secondary: 2,
        onset_sharpness: OnsetType::Subacute,
        dsp_probs: [0.60, 0.30, 0.08, 0.00],
        ndis_probs: [0.50, 0.25, 0.05, 0.00],
        comorbidity_eligible: 9, // dep+pain
    },
    // Type 11: Hearing Loss
    PersonTypeProfile {
        id: 11,
        label: "HEARING_LOSS",
        sdac_primary: 2,
        dim_primary: 3,
        dim_secondary: 5,
        onset_sharpness: OnsetType::Gradual,
        dsp_probs: [0.25, 0.08, 0.02, 0.00],
        ndis_probs: [0.40, 0.10, 0.02, 0.00],
        comorbidity_eligible: 17, // dep+anx
    },
    // Type 12: Vision Loss
    PersonTypeProfile {
        id: 12,
        label: "VISION_LOSS",
        sdac_primary: 1,
        dim_primary: 2,
        dim_secondary: -1,
        onset_sharpness: OnsetType::Gradual,
        dsp_probs: [0.35, 0.10, 0.03, 0.00],
        ndis_probs: [0.45, 0.10, 0.02, 0.00],
        comorbidity_eligible: 3, // dep+diabetes
    },
    // Type 13: Spinal Cord Injury
    PersonTypeProfile {
        id: 13,
        label: "SPINAL_CORD_INJURY",
        sdac_primary: 10,
        dim_primary: 1,
        dim_secondary: 4,
        onset_sharpness: OnsetType::Acute,
        dsp_probs: [0.65, 0.35, 0.10, 0.00],
        ndis_probs: [0.70, 0.40, 0.10, 0.00],
        comorbidity_eligible: 9, // dep+pain
    },
    // Type 14: Stroke / ABI
    PersonTypeProfile {
        id: 14,
        label: "STROKE_ABI",
        sdac_primary: 17,
        dim_primary: 0,
        dim_secondary: 1,
        onset_sharpness: OnsetType::Acute,
        dsp_probs: [0.60, 0.30, 0.08, 0.00],
        ndis_probs: [0.55, 0.25, 0.05, 0.00],
        comorbidity_eligible: 13, // dep+pain+cvd
    },
    // Type 15: Parkinson's Disease
    PersonTypeProfile {
        id: 15,
        label: "PARKINSONS",
        sdac_primary: 10,
        dim_primary: 1,
        dim_secondary: 4,
        onset_sharpness: OnsetType::Gradual,
        dsp_probs: [0.55, 0.25, 0.05, 0.00],
        ndis_probs: [0.40, 0.15, 0.03, 0.00],
        comorbidity_eligible: 1, // dep
    },
    // Type 16: Dementia
    PersonTypeProfile {
        id: 16,
        label: "DEMENTIA",
        sdac_primary: 15,
        dim_primary: 0,
        dim_secondary: 5,
        onset_sharpness: OnsetType::Gradual,
        dsp_probs: [0.55, 0.30, 0.05, 0.00],
        ndis_probs: [0.30, 0.15, 0.03, 0.00],
        comorbidity_eligible: 1, // dep
    },
    // Type 17: Upper Limb Impairment
    PersonTypeProfile {
        id: 17,
        label: "UPPER_LIMB",
        sdac_primary: 8,
        dim_primary: 4,
        dim_secondary: 1,
        onset_sharpness: OnsetType::Acute,
        dsp_probs: [0.40, 0.15, 0.03, 0.00],
        ndis_probs: [0.55, 0.30, 0.05, 0.00],
        comorbidity_eligible: 9, // dep+pain
    },
    // Type 18: Lower Limb Impairment
    PersonTypeProfile {
        id: 18,
        label: "LOWER_LIMB",
        sdac_primary: 10,
        dim_primary: 1,
        dim_secondary: -1,
        onset_sharpness: OnsetType::Acute,
        dsp_probs: [0.55, 0.25, 0.05, 0.00],
        ndis_probs: [0.60, 0.30, 0.05, 0.00],
        comorbidity_eligible: 11, // dep+pain+diabetes
    },
    // Type 19: Cardiovascular Disease
    PersonTypeProfile {
        id: 19,
        label: "CARDIOVASCULAR",
        sdac_primary: 5,
        dim_primary: 1,
        dim_secondary: -1,
        onset_sharpness: OnsetType::Subacute,
        dsp_probs: [0.35, 0.12, 0.03, 0.00],
        ndis_probs: [0.05, 0.01, 0.00, 0.00],
        comorbidity_eligible: 19, // dep+diabetes+anx
    },
    // Type 20: Chronic Kidney Disease
    PersonTypeProfile {
        id: 20,
        label: "CHRONIC_KIDNEY",
        sdac_primary: 5,
        dim_primary: 1,
        dim_secondary: -1,
        onset_sharpness: OnsetType::Gradual,
        dsp_probs: [0.50, 0.15, 0.03, 0.00],
        ndis_probs: [0.10, 0.02, 0.00, 0.00],
        comorbidity_eligible: 7, // dep+diabetes+cvd
    },
    // Type 21: PTSD
    PersonTypeProfile {
        id: 21,
        label: "PTSD",
        sdac_primary: 11,
        dim_primary: 5,
        dim_secondary: 0,
        onset_sharpness: OnsetType::Subacute,
        dsp_probs: [0.30, 0.10, 0.02, 0.00],
        ndis_probs: [0.08, 0.02, 0.00, 0.00],
        comorbidity_eligible: 24, // anx+pain
    },
    // Type 22: Carpal Tunnel / RSI
    PersonTypeProfile {
        id: 22,
        label: "CARPAL_TUNNEL_RSI",
        sdac_primary: 8,
        dim_primary: 4,
        dim_secondary: -1,
        onset_sharpness: OnsetType::Gradual,
        dsp_probs: [0.10, 0.03, 0.01, 0.00],
        ndis_probs: [0.03, 0.00, 0.00, 0.00],
        comorbidity_eligible: 9, // dep+pain
    },
    // Type 23: Intellectual Disability
    PersonTypeProfile {
        id: 23,
        label: "INTELLECTUAL_DISABILITY",
        sdac_primary: 7,
        dim_primary: 0,
        dim_secondary: 5,
        onset_sharpness: OnsetType::Lifelong,
        dsp_probs: [0.75, 0.50, 0.20, 0.05],
        ndis_probs: [0.80, 0.60, 0.30, 0.05],
        comorbidity_eligible: 17, // dep+anx
    },
    // Type 24: Autism Spectrum Disorder
    PersonTypeProfile {
        id: 24,
        label: "AUTISM_ASD",
        sdac_primary: 16,
        dim_primary: 5,
        dim_secondary: 0,
        onset_sharpness: OnsetType::Lifelong,
        dsp_probs: [0.35, 0.15, 0.05, 0.00],
        ndis_probs: [0.50, 0.30, 0.10, 0.02],
        comorbidity_eligible: 17, // dep+anx
    },
    // Type 25: Fibromyalgia
    PersonTypeProfile {
        id: 25,
        label: "FIBROMYALGIA",
        sdac_primary: 5,
        dim_primary: 1,
        dim_secondary: 0,
        onset_sharpness: OnsetType::Gradual,
        dsp_probs: [0.20, 0.08, 0.02, 0.00],
        ndis_probs: [0.05, 0.01, 0.00, 0.00],
        comorbidity_eligible: 17, // dep+anx
    },
    // Type 26: Speech Difficulties
    PersonTypeProfile {
        id: 26,
        label: "SPEECH_DIFFICULTIES",
        sdac_primary: 3,
        dim_primary: 5,
        dim_secondary: 0,
        onset_sharpness: OnsetType::Acute,
        dsp_probs: [0.25, 0.10, 0.03, 0.00],
        ndis_probs: [0.35, 0.15, 0.03, 0.00],
        comorbidity_eligible: 1, // dep
    },
    // Type 27: Social / Behavioural
    PersonTypeProfile {
        id: 27,
        label: "SOCIAL_BEHAVIOURAL",
        sdac_primary: 16,
        dim_primary: 5,
        dim_secondary: 0,
        onset_sharpness: OnsetType::Gradual,
        dsp_probs: [0.25, 0.10, 0.03, 0.00],
        ndis_probs: [0.08, 0.02, 0.00, 0.00],
        comorbidity_eligible: 16, // anx
    },
];

/// Prevalence weights for person type assignment (sum to ~1.0).
pub const PERSON_TYPE_WEIGHTS: [f64; 27] = [
    0.12, // 1: Chronic Back Pain
    0.14, // 2: Depression/Anxiety
    0.03, // 3: Schizophrenia
    0.03, // 4: Bipolar
    0.08, // 5: Type 2 Diabetes
    0.02, // 6: Type 1 Diabetes
    0.05, // 7: COPD
    0.04, // 8: RA
    0.03, // 9: Epilepsy
    0.02, // 10: MS
    0.04, // 11: Hearing Loss
    0.03, // 12: Vision Loss
    0.02, // 13: SCI
    0.03, // 14: Stroke/ABI
    0.02, // 15: Parkinson's
    0.02, // 16: Dementia
    0.02, // 17: Upper Limb
    0.03, // 18: Lower Limb
    0.05, // 19: CVD
    0.02, // 20: CKD
    0.04, // 21: PTSD
    0.03, // 22: Carpal Tunnel
    0.02, // 23: Intellectual Disability
    0.02, // 24: Autism
    0.03, // 25: Fibromyalgia
    0.01, // 26: Speech
    0.01, // 27: Social/Behavioural
];

// ============================================================================
// Helper functions
// ============================================================================

/// Get the profile for a person type (1-indexed).
/// Returns None if out of range.
pub fn get_profile(person_type: i32) -> Option<&'static PersonTypeProfile> {
    let idx = (person_type - 1) as usize;
    PROFILES.get(idx)
}

/// Draw comorbidity flags for a disabled person.
pub fn draw_comorbidity_flags(rng: &mut StdRng, eligible: u8) -> i32 {
    let mut flags: i32 = 0;
    for &(bit, rate) in &COMORB_RATES {
        if (eligible & bit) != 0 && rng.gen::<f64>() < rate {
            flags |= bit as i32;
        }
    }
    flags
}

/// Assign person type from prevalence weights.
pub fn draw_person_type(rng: &mut StdRng) -> u8 {
    let idx = weighted_sample(rng, &PERSON_TYPE_WEIGHTS);
    (idx + 1) as u8
}
