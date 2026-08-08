use std::str::FromStr;

use anyhow::{Context, Result, anyhow};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Instruction {
    Cut(i128),
    Deal(i128),
    Reverse,
}

#[derive(Debug, Clone, Copy)]
struct Shuffle {
    offset: i128,
    mul: i128,
    order: i128,
}

impl FromStr for Instruction {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut words = s.split_ascii_whitespace();
        let first = words
            .next()
            .context("Instruction should contain at least one word")?;
        let last = words
            .last()
            .context("Instruction should contain at least two words")?;

        if first == "cut" {
            let n: i128 = last.parse()?;

            Ok(Instruction::Cut(n))
        } else if first == "deal" {
            if last == "stack" {
                Ok(Instruction::Reverse)
            } else {
                let n: i128 = last.parse()?;

                Ok(Instruction::Deal(n))
            }
        } else {
            Err(anyhow!("Couldn't parse instruction"))
        }
    }
}

fn compile_instructions(ins: &[Instruction], order: i128) -> Shuffle {
    let mut offset = 0;
    let mut mul = 1;

    for &ins in ins {
        match ins {
            Instruction::Cut(n) => offset = (offset - n).rem_euclid(order),
            Instruction::Deal(n) => {
                offset = (offset * n).rem_euclid(order);
                mul = (mul * n).rem_euclid(order);
            }
            Instruction::Reverse => {
                offset = (-offset - 1).rem_euclid(order);
                mul = (-mul).rem_euclid(order);
            }
        }
    }

    Shuffle { offset, mul, order }
}

fn fast_prime_modular_exp(a: i128, e: i128, m: i128) -> i128 {
    match e {
        2.. => {
            let b = fast_prime_modular_exp(a, e / 2, m);

            ((b * b).rem_euclid(m) * fast_prime_modular_exp(a, e % 2, m)).rem_euclid(m)
        }
        1 => a,
        0 => 1,
        _ => fast_prime_modular_exp(prime_modular_inverse(a, m), -e, m),
    }
}

fn prime_modular_inverse(a: i128, m: i128) -> i128 {
    // Fermat's little theorem says that if m is prime
    // a^(m - 1) = 1 [m], therefore $a^(m - 2) is its modular inverse

    fast_prime_modular_exp(a, m - 2, m)
}

impl Shuffle {
    fn identity(order: i128) -> Self {
        Self {
            order,
            offset: 0,
            mul: 1,
        }
    }

    fn reverse(self) -> Self {
        // Forward is
        // res <- pos * mul + offset
        // So
        // pos <- (res - offset) * mul^(-1)
        let inv_mul = prime_modular_inverse(self.mul, self.order);
        Self {
            mul: inv_mul,
            order: self.order,
            offset: (-self.offset * inv_mul).rem_euclid(self.order),
        }
    }

    fn chain(self, rhs: Self) -> Self {
        debug_assert_eq!(self.order, rhs.order);
        // tmp <- pos * mul_self + offset_self
        // res <- tmp * mul_rhs + offset_rhs
        //
        // res <- pos * (mul_self * mul_rhs) + mul_rhs * offset_self + offset_rhs

        Self {
            mul: (self.mul * rhs.mul).rem_euclid(self.order),
            order: self.order,
            offset: ((self.offset * rhs.mul).rem_euclid(self.order) + rhs.offset)
                .rem_euclid(self.order),
        }
    }

    fn repeat(self, times: usize) -> Self {
        match times {
            2.. => {
                let half = self.repeat(times / 2);
                half.chain(half).chain(self.repeat(times % 2))
            }
            1 => self,
            0 => Self::identity(self.order),
        }
    }

    fn perform(self, pos: i128) -> i128 {
        ((pos * self.mul).rem_euclid(self.order) + self.offset).rem_euclid(self.order)
    }
}

pub async fn day22(input: String) -> Result<(String, String)> {
    let ins = input
        .lines()
        .map(|x| x.parse::<Instruction>())
        .collect::<Result<Vec<Instruction>>>()?;

    let part1_shuffle = compile_instructions(&ins, 10007);
    let part2_shuffle = compile_instructions(&ins, 119315717514047)
        .reverse()
        .repeat(101741582076661);

    Ok((
        part1_shuffle.perform(2019).to_string(),
        part2_shuffle.perform(2020).to_string(),
    ))
}

