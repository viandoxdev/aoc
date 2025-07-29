use anyhow::{Result, anyhow};
use itertools::Itertools;

use crate::intcode::Intcode;

fn thruster_signal(settings: &[u8], program: &Intcode<71>) -> Result<i64> {
    let mut input = 0;
    for &setting in settings {
        let mut amp = program.clone();
        input = amp
            .run(&[setting as i64, input])?
            .last()
            .copied()
            .ok_or_else(|| anyhow!("No output"))?;
    }

    Ok(input)
}

fn thruster_signal_feedback_loop(settings: &[u8], program: &Intcode<72>) -> Result<i64> {
    let mut amps = (0..5).map(|i| {
        let mut amp = program.clone();
        amp.feed_input(&[settings[i] as i64])?;
        Ok(amp)
    }).collect::<Result<Vec<Intcode<72>>>>()?;

    let mut input = 0;
    let mut output = 0;
    for i in (0..5).cycle() {
        if let Some(next) = amps[i].run_until_output(&[input])? {
            input = next;

            // Save signal output from E
            if i == 4 {
                output = next;
            }
        } else {
            // Amplifier halted, we can't get any more signal from E, so we use the last one
            // (output)
            return Ok(output)
        }
    }

    unreachable!()
}

pub async fn day07(input: String) -> Result<(String, String)> {
    let program: Intcode<71> = input.parse()?;

    let part1 = (0..5)
        .permutations(5)
        .map(|setting| thruster_signal(&setting, &program))
        .fold_ok(0, |a, v| a.max(v))?;

    let program: Intcode<72> = program.into_version();

    let part2 = (5..10)
        .permutations(5)
        .map(|setting| thruster_signal_feedback_loop(&setting, &program))
        .fold_ok(0, |a, v| a.max(v))?;

    Ok((part1.to_string(), part2.to_string()))
}

