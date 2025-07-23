use std::num::ParseIntError;

use anyhow::Result;

pub async fn day01(input: String) -> Result<(String, String)> {
    let masses = input
        .lines()
        .map(str::parse::<u32>)
        .collect::<Result<Vec<u32>, ParseIntError>>()?;

    let part1 = masses.iter().map(|&m| m / 3 - 2).sum::<u32>();
    let part2 = masses
        .iter()
        .copied()
        .map(|mut m| {
            let mut total = 0;
            while m > 0 {
                m = (m / 3).saturating_sub(2);
                total += m;
            }
            total
        })
        .sum::<u32>();

    Ok((part1.to_string(), part2.to_string()))
}
