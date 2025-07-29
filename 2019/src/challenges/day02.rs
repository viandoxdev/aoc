use std::num::ParseIntError;

use anyhow::{Result, anyhow};
use itertools::Itertools;

use crate::intcode::Intcode;

fn run_noun_vern(ints: &[i64], noun: i64, verb: i64) -> Result<i64> {
    let mut program = ints.to_vec();
    program[1] = noun;
    program[2] = verb;

    let mut intcode = Intcode::<22>::new(program);

    intcode.run(&[])?;

    Ok(intcode.program[0])
}

pub async fn day02(input: String) -> Result<(String, String)> {
    let ints = input
        .trim()
        .split(",")
        .map(str::parse::<i64>)
        .collect::<Result<Vec<i64>, ParseIntError>>()?;

    let part1 = run_noun_vern(&ints, 12, 2)?;

    let part2 = (0..100)
        .cartesian_product(0..100)
        .find_map(|(noun, verb)| {
            (run_noun_vern(&ints, noun, verb).ok()? == 19690720).then_some(noun * 100 + verb)
        })
        .ok_or_else(|| anyhow!("Couldn't find noun / verb pair"))?;

    Ok((part1.to_string(), part2.to_string()))
}

