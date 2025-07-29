use anyhow::{Result, anyhow};
use itertools::Itertools;
use num::Integer;

/// n choose p
fn binom_mod_2(n: u32, p: u32) -> u32 {
    // Lucas or Kummer theorem
    (n >= p && p & (n - p) == 0) as u32
}

/// n choose p
fn binom_mod_5(mut n: u32, mut p: u32) -> u32 {
    // Lucas or Kummer theorem again but this time we can't cheat with bitwise operations.
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
    // Chinese remainder theorem
    static LUT: [[u32; 5]; 2] = [[0, 6, 2, 8, 4], [5, 1, 7, 3, 9]];

    let mod_2 = binom_mod_2(n, p);
    let mod_5 = binom_mod_5(n, p);

    LUT[mod_2 as usize][mod_5 as usize]
}

// Do one fft phase into the given buffer
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
        // Do a hundred fft phases by swapping buffers to avoid memory allocations
        let mut list = list.clone();
        let mut buf = vec![0i32; list.len()];

        for _ in 0..100 {
            fft_phase_into(&list, &mut buf);
            (list, buf) = (buf, list);
        }

        list.iter().copied().take(8).fold(0, |a, d| a * 10 + d)
    };

    let part2 = {
        // Trick: the values we are asked to compute are very far into the resulting list,
        // therefore the mask used will be essentially just zeroes with a few ones at the end:
        // So to get a value at a sufficiently large index i of a phase, we just need to add up 
        // the values of the previous phase from index i until the end.
        //
        // With a bit of math we can find the explicit formula for the number of time each number
        // of the first phase is added in the intermediary phase, this turns out to be a binomial
        // coefficient, so we compute a large list of them (turns out there can be one shared list
        // for all 8 digits we want to compute)
        //
        // (This is only possible because we work mod 10, where the coefficients are bounded,
        // otherwise the resulting numbers would be far too large to work with)
        //
        // We also use some math to compute these coefficients faster and without the numbers
        // getting too big, see binom_mod_10 for more information.

        // Theoretical length of the list
        let len = list.len() * PART2_FACTOR;
        // Offset into the resulting list
        let offset = list.iter().copied().take(7).fold(0, |a, d| a * 10 + d) as usize;
        // All the coefficients we'll need
        let binom = (0..(len - offset) as u32)
            .map(|x| binom_mod_10(99 + x, x))
            .collect_vec();

        // Go over each digit's index
        (offset..offset + 8).map(|offset| {
            // Add up the numbers from the current digit to the end of the list scaled by the
            // binomial coefficients
            ((offset..len)
                .map(|index| list[index % list.len()])
                .zip(binom.iter().copied())
                .map(|(num, coef)| num * coef as i32)
                .sum::<i32>()
                % 10)
                .abs()
        }).fold(0, |a, d| a * 10 + d)
    };

    Ok((part1.to_string(), part2.to_string()))
}
