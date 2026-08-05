use rand::Rng;

/// Sample from a categorical distribution defined by weights.
/// Returns the index of the selected category.
pub fn weighted_sample(rng: &mut impl Rng, weights: &[f64]) -> usize {
    let total: f64 = weights.iter().sum();
    let mut r = rng.gen::<f64>() * total;
    for (i, &w) in weights.iter().enumerate() {
        r -= w;
        if r <= 0.0 {
            return i;
        }
    }
    weights.len() - 1
}

/// Sample n values from a categorical distribution, returning indices.
pub fn weighted_sample_n(rng: &mut impl Rng, weights: &[f64], n: usize) -> Vec<usize> {
    (0..n).map(|_| weighted_sample(rng, weights)).collect()
}

/// Generate a uniform integer in [lo, hi] inclusive.
pub fn uniform_int(rng: &mut impl Rng, lo: i32, hi: i32) -> i32 {
    rng.gen_range(lo..=hi)
}

/// Sample from N(mean, sd) using the Box-Muller transform.
pub fn normal_sample(rng: &mut impl Rng, mean: f64, sd: f64) -> f64 {
    let u1: f64 = rng.gen::<f64>().max(1e-15); // avoid log(0)
    let u2: f64 = rng.gen::<f64>();
    let z = (-2.0 * u1.ln()).sqrt() * (2.0 * std::f64::consts::PI * u2).cos();
    mean + sd * z
}
