use anyhow::{Context, Result, anyhow};
use itertools::Itertools;

use crate::intcode::Intcode;

fn run_program(mut program: Intcode<191>, code: &[&str]) -> Result<i64> {
    let input = code
        .iter()
        .flat_map(|&ins| ins.chars().map(|c| c as i64).chain(std::iter::once(10i64)))
        .collect_vec();

    let output = program.run(&input)?;

    let Some(&res) = output.iter().find(|&&x| x > 127) else {
        let view = output
            .iter()
            .map(|&c| {
                if let Some(c) = char::from_u32(c as u32) {
                    c.to_string()
                } else {
                    format!("<{c}>")
                }
            })
            .collect::<String>();
        return Err(anyhow!("Robot fell: \n {view}"));
    };

    Ok(res)
}

pub async fn day21(input: String) -> Result<(String, String)> {
    let program: Intcode<191> = input.parse()?;

    // Jump if there is a hole in 1-3 AND there is ground on 4
    let part1 = run_program(program.clone(), &[
        "NOT A T",
        "OR T J",
        "NOT B T",
        "OR T J",
        "NOT C T",
        "OR T J",
        "AND D J",
        "WALK"
    ])?;

    // Jump if there is a hole in 1-3 AND there is ground on 4 AND there is ground on 5 or 8
    let part2 = run_program(program, &[
        "NOT A T",
        "OR T J",
        "NOT B T",
        "OR T J",
        "NOT C T",
        "OR T J",
        "AND D J",
        "AND E T",
        "OR H T",
        "AND T J",
        "RUN"
    ])?;

    Ok((part1.to_string(), part2.to_string()))
}

