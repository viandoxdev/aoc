use anyhow::{Result, anyhow};

use crate::intcode::{Intcode, parse_int_list};

pub async fn day05(input: String) -> Result<(String, String)> {
    let ints = parse_int_list(&input)?;

    let part1 = {
        let mut intcode = Intcode::<51>::new(ints.clone());

        intcode
            .run(&[1])?
            .last()
            .copied()
            .ok_or_else(|| anyhow!("No program output"))?
    };

    let part2 = {
        let mut intcode = Intcode::<52>::new(ints);

        intcode
            .run(&[5])?
            .last()
            .copied()
            .ok_or_else(|| anyhow!("No program output"))?
    };

    Ok((part1.to_string(), part2.to_string()))
}
