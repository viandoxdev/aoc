use std::num::ParseIntError;

use anyhow::Result;
use num::Integer;

#[derive(Debug, Clone, Copy)]
struct Moon {
    p: [i32; 3],
    v: [i32; 3],
}

impl Moon {
    fn new(x: i32, y: i32, z: i32) -> Self {
        Self {
            p: [x, y, z],
            v: [0; 3],
        }
    }
    #[inline(always)]
    fn kinetic_energy(&self) -> i32 {
        self.v[0].abs() + self.v[1].abs() + self.v[2].abs()
    }
    #[inline(always)]
    fn potential_energy(&self) -> i32 {
        self.p[0].abs() + self.p[1].abs() + self.p[2].abs()
    }
    #[inline(always)]
    fn energy(&self) -> i32 {
        self.kinetic_energy() * self.potential_energy()
    }
}

#[derive(Clone, Copy, Debug)]
struct State {
    moons: [Moon; 4],
}

impl State {
    #[inline]
    fn step_coord<const C: usize>(&mut self) {
        for a in 0..4 {
            for b in (a + 1)..4 {
                self.moons[a].v[C] += (self.moons[b].p[C] - self.moons[a].p[C]).signum();
                self.moons[b].v[C] += (self.moons[a].p[C] - self.moons[b].p[C]).signum();
            }
        }
        for a in 0..4 {
            self.moons[a].p[C] += self.moons[a].v[C];
        }
    }
    #[inline(always)]
    fn eq_coord<const C: usize>(&self, other: &Self) -> bool {
        self.moons[0].p[C] == other.moons[0].p[C]
        && self.moons[1].p[C] == other.moons[1].p[C]
        && self.moons[2].p[C] == other.moons[2].p[C]
        && self.moons[3].p[C] == other.moons[3].p[C]
        && self.moons[0].v[C] == other.moons[0].v[C]
        && self.moons[1].v[C] == other.moons[1].v[C]
        && self.moons[2].v[C] == other.moons[2].v[C]
        && self.moons[3].v[C] == other.moons[3].v[C]
    }
    #[inline]
    fn cycle<const C: usize>(&self) -> usize {
        let mut length = 1;
        let mut state = *self;
        state.step_coord::<C>();
        while !self.eq_coord::<C>(&state) {
            state.step_coord::<C>();
            length += 1;
        }

        length
    }
    #[inline(always)]
    fn step(&mut self) {
        self.step_coord::<0>();
        self.step_coord::<1>();
        self.step_coord::<2>();
    }
}

pub async fn day12(input: String) -> Result<(String, String)> {
    let state = input
        .trim()
        .chars()
        .filter(|c| c.is_ascii_digit() || matches!(c, '-' | ',' | '\n'))
        .collect::<String>()
        .lines()
        .map(|l| {
            let components = l
                .split(",")
                .map(|n| n.parse())
                .collect::<Result<Vec<i32>, ParseIntError>>()?;
            Ok(Moon::new(components[0], components[1], components[2]))
        })
        .collect::<Result<Vec<Moon>, ParseIntError>>()?;
    let state = State {
        moons: [state[0], state[1], state[2], state[3]],
    };

    let part1 = {
        let mut state = state.clone();
        for _ in 0..1000 {
            state.step();
        }
        state.moons.iter().map(Moon::energy).sum::<i32>()
    };

    let part2 = {
        let x_cycle = state.cycle::<0>();
        let y_cycle = state.cycle::<1>();
        let z_cycle = state.cycle::<2>();

        let xy_cycle = x_cycle.lcm(&y_cycle);
        z_cycle * xy_cycle / z_cycle.gcd(&xy_cycle)
    };

    Ok((part1.to_string(), part2.to_string()))
}
