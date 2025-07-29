use anyhow::{Result, anyhow};
use itertools::Itertools;
use num::Integer;

/// n choose p
fn binom_mod_2(n: u32, p: u32) -> u32 {
    // Lucas or Kummer theorem
    p & (n - p)
}

/// n choose p
fn binom_mod_5(mut n: u32, mut p: u32) -> u32 {
    static LUT: [[u32; 5]; 5] = [
        [1, 0, 0, 0, 0],
        [1, 1, 0, 0, 0],
        [1, 2, 1, 0, 0],
        [1, 3, 3, 1, 0],
        [1, 4, 6, 4, 1],
    ];

    let mut res = 1;
    let mut dn;
    let mut dp;
    while n > 0 || p > 0 {
        (n, dn) = n.div_rem(&5);
        (p, dp) = p.div_rem(&5);

        res *= LUT[dn as usize][dp as usize];
    }

    res % 5
}

/// n choose p
fn binom_mod_10(n: u32, p: u32) -> u32 {
    static LUT: [[u32; 5]; 2] = [
        [0, 2, 6, 8, 4],
        [5, 1, 7, 3, 9],
    ];
    
    let mod_2 = binom_mod_2(n, p);
    let mod_5 = binom_mod_5(n, p);

    LUT[mod_2 as usize][mod_5 as usize]
}

fn fft_phase_into(data: &[i32], buf: &mut [i32]) {
    for (i, d) in buf.iter_mut().enumerate() {
        let size = i + 1;
        let mut total = 0;
        let mut j = i;
        while j < data.len() {
            for k in 0..(size.min(data.len() - j)) {
                total += data[j + k]
            }
            j += size * 4;
        }

        j = i + 2 * size;
        while j < data.len() {
            for k in 0..(size.min(data.len() - j)) {
                total -= data[j + k]
            }
            j += size * 4;
        }

        *d = (total % 10).abs();
    }
}

const PART2_FACTOR: usize = 10000;

pub async fn day16(input: String) -> Result<(String, String)> {
    let list = input
        .trim()
        .chars()
        .map(|c| c.to_digit(10).map(|d| d as i32))
        .collect::<Option<Vec<i32>>>()
        .ok_or_else(|| anyhow!("Bad input"))?;

    let part1 = {
        let mut list = list.clone();
        let mut buf = vec![0i32; list.len()];

        for _ in 0..100 {
            fft_phase_into(&list, &mut buf);
            (list, buf) = (buf, list);
        }

        list.iter().copied().take(8).fold(0, |a, d| a * 10 + d)
    };

    let part2 = {
        let mut phases = vec![Vec::new(); 101];
        let len = list.len() * PART2_FACTOR;

        let phase_100_offset = list.iter().copied().take(7).fold(0, |a, d| a * 10 + d) as usize;

        let rev_list = list.iter().copied().rev().collect_vec();

        phases[0].extend(
            std::iter::repeat_with(|| rev_list.iter().copied())
                .flatten()
                .take(len - phase_100_offset),
        );

        for phase in 1..=100 {
            let (low_phases, high_phases) = phases.split_at_mut(phase);
            low_phases[phase - 1]
                .iter()
                .copied()
                .fold(0i32, |total, el| {
                    let next = total + el;
                    high_phases[0].push((next % 10).abs());
                    next
                });
        }

        (phase_100_offset..phase_100_offset + 8)
            .map(|index| phases[100][len - index - 1])
            .fold(0, |a, d| a * 10 + d)
    };

    Ok((part1.to_string(), part2.to_string()))
}

