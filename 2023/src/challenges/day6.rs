use anyhow::{anyhow, Result};
use itertools::Itertools;

#[derive(Debug)]
struct Race {
    duration: u64,
    record: u64,
}

impl Race {
    fn parse(s: &str) -> Result<Vec<Self>> {
        let (time_str, dist_str) = s.split_once("\n").ok_or(anyhow!("Malformed input"))?;
        time_str
            .split_whitespace()
            .zip(dist_str.split_whitespace())
            .skip(1)
            .map(|(ts, ds)| {
                Ok(Race {
                    duration: ts.parse::<u64>()?,
                    record: ds.parse::<u64>()?,
                })
            })
            .try_collect()
    }

    fn parse_kerning(s: &str) -> Result<Self> {
        let (time_str, dist_str) = s.split_once("\n").ok_or(anyhow!("Malformed input"))?;
        let duration = time_str
            .chars()
            .flat_map(|x| x.to_digit(10))
            .fold(0u64, |a, c| a * 10 + c as u64);
        let record = dist_str
            .chars()
            .flat_map(|x| x.to_digit(10))
            .fold(0u64, |a, c| a * 10 + c as u64);
        Ok(Self { record, duration })
    }

    fn ways_to_beat(&self) -> u64 {
        // The distance we get to can be obtained with the formula (let x the number of
        // milliseconds with the button held, d the duration of the race, and r the record distance):
        //
        // dist(x) = x(d - x) = -x² + dx
        //
        // This is a inverted parabola, so ∀x∈]r1;r2[, dist(x) > r, where r1 and r2 are the
        // solutions to the following polynomial
        //
        // p(x) = x^2 + dx - r (since d,r∈ℕ, x∈ℕ => p(x)∈ℕ)
        //
        // We'll assume p(x) to be strictly increasing around r1 and strictly decreasing around r2,
        // As is the case with any lare enough parabola, like the one we get in the input.
        //
        // We now want to find s,e∈ℕ such that ∀x∈[s;e], p(x) > r
        //
        // s = ⌊r1⌋ + 1 => s∈ℕ and s > r1 => p(s) > p(r1) <=> p(s) > 0 with p(s)∈ℕ
        // e = ⌈r2⌉ - 1 => e∈ℕ and e < r1 => p(e) > p(r2) <=> p(e) > 0 with p(e)∈ℕ
        //   p(x) is decreasing here, so we flip ─┘
        //
        // ∴ s and e are the first and last integers for which dist is more than r. The number of
        // ways to beat the record is then the number of integers in [s;e] which is e - s + 1
        let (start, end) = {
            let d = self.duration as f64;
            let r = self.record as f64;
            let delta = d * d - 4.0 * r;
            if delta < 0.0 {
                return 0;
            }

            let half_sqrt_delta = delta.sqrt() / 2.0;
            let middle = d / 2.0;
            (
                (middle - half_sqrt_delta).floor() as u64 + 1, 
                (middle + half_sqrt_delta).ceil() as u64 - 1,
            )
        };

        end - start + 1
    }
}

pub async fn day6(input: String) -> Result<(String, String)> {
    let races = Race::parse(&input)?;
    let race = Race::parse_kerning(&input)?;

    let part1: u64 = races.iter().map(|x| x.ways_to_beat()).product();

    let part2 = race.ways_to_beat();

    Ok((part1.to_string(), part2.to_string()))
}
