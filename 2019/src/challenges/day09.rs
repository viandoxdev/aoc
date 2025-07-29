use anyhow::{Result, anyhow};

use crate::intcode::Intcode;

pub async fn day09(input: String) -> Result<(String, String)> {
    let program: Intcode<91> = input.parse()?;

    let part1 = program
        .clone()
        .run(&[1])?
        .last()
        .copied()
        .ok_or_else(|| anyhow!("No output"))?;
    let part2 = program
        .clone()
        .run(&[2])?
        .last()
        .copied()
        .ok_or_else(|| anyhow!("No output"))?;

    Ok((part1.to_string(), part2.to_string()))
}

