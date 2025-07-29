use std::num::ParseIntError;

use anyhow::Result;
use itertools::Itertools;
use num::Integer;
use rustc_hash::FxHashSet;

#[derive(Debug, Clone, Copy)]
struct Moon {
    x: i32,
    y: i32,
    z: i32,
    vx: i32,
    vy: i32,
    vz: i32,
}

impl Moon {
    pub fn new(x: i32, y: i32, z: i32) -> Self {
        Self {
            x,
            y,
            z,
            vx: 0,
            vy: 0,
            vz: 0,
        }
    }
}

pub async fn day12(input: String) -> Result<(String, String)> {
    let moons = input
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

    let part1 = {
        let mut moons = moons.clone();
        for _ in 0..1000 {
            // Gravity
            for (a, b) in (0..moons.len()).tuple_combinations() {
                moons[a].vx += (moons[b].x - moons[a].x).signum();
                moons[a].vy += (moons[b].y - moons[a].y).signum();
                moons[a].vz += (moons[b].z - moons[a].z).signum();
                moons[b].vx += (moons[a].x - moons[b].x).signum();
                moons[b].vy += (moons[a].y - moons[b].y).signum();
                moons[b].vz += (moons[a].z - moons[b].z).signum();
            }

            // Velocity
            for m in &mut moons {
                m.x += m.vx;
                m.y += m.vy;
                m.z += m.vz;
            }
        }

        moons
            .iter()
            .map(|&m| (m.x.abs() + m.y.abs() + m.z.abs()) * (m.vx.abs() + m.vy.abs() + m.vz.abs()))
            .sum::<i32>()
    };

    let part2 = {
        // Make sure the size is known at compile time so the compiler has a better shot at
        // optimizing
        let mut m = [moons[0], moons[1], moons[2], moons[3]];
        macro_rules! find_cycle {
            ($pos:ident, $vel:ident) => {
                'block: {
                    let mut states = FxHashSet::<([i32; 4], [i32; 4])>::default();

                    for i in 0.. {
                        if !states.insert(([m[0].$pos, m[1].$pos, m[2].$pos, m[3].$pos], [m[0].$vel, m[1].$vel, m[2].$vel, m[3].$vel])) {
                            break 'block i as u128
                        }

                        for a in 0..4 {
                            for b in (a+1)..4 {
                                m[a].$vel += (m[b].$pos - m[a].$pos).signum();
                                m[b].$vel += (m[a].$pos - m[b].$pos).signum();
                            }
                        }

                        for a in 0..4 {
                            m[a].$pos += m[a].$vel;
                        }
                    }

                    unreachable!()
                }
            };
        }
        let x_cycle = find_cycle!(x, vx);
        let y_cycle = find_cycle!(y, vy);
        let z_cycle = find_cycle!(z, vz);

        let xy_cycle = x_cycle.lcm(&y_cycle);
        z_cycle * xy_cycle / z_cycle.gcd(&xy_cycle)
    };

    Ok((part1.to_string(), part2.to_string()))
}

