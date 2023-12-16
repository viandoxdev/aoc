use anyhow::Result;
use itertools::Itertools;

// Vector of bits
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Bits(u32);

#[derive(Debug)]
enum Reflection {
    Horizontal(usize),
    Vertical(usize),
}

impl Reflection {
    pub fn summarize(self) -> usize {
        match self {
            Self::Horizontal(x) => 100 * x,
            Self::Vertical(x) => x,
        }
    }
}

/// Count number of ones in the binary representation of n
const fn count_ones(mut n: usize) -> usize {
    let mut count = 0;
    while n > 0 {
        count += 1;
        n = n & (n - 1);
    }
    count
}

/// Number of ones in the binary representations of a byte
static ONE_COUNT_MAP: [usize; 256] = {
    let mut res = [0; 256];
    let mut n = 0;
    while n < 256 {
        res[n] = count_ones(n);
        n += 1;
    }
    res
};

impl Bits {
    /// Number of bits with value one
    fn ones(&self) -> usize {
        // Count in each bytes and sum them up
        let x0 = (self.0 & 0xFF) as usize;
        let x1 = (self.0 >> 8 & 0xFF) as usize;
        let x2 = (self.0 >> 16 & 0xFF) as usize;
        let x3 = (self.0 >> 24) as usize;
        ONE_COUNT_MAP[x0] + ONE_COUNT_MAP[x1] + ONE_COUNT_MAP[x2] + ONE_COUNT_MAP[x3]
    }
}

impl std::ops::BitXor for Bits {
    type Output = Self;
    fn bitxor(self, rhs: Self) -> Self::Output {
        Self(self.0 ^ rhs.0)
    }
}

/// Look for reflection in slice, expected_diff specifies how many bits of difference are expected.
/// For a perfect reflection we expect 0, for a reflection with a single smudge we expect 1.
fn find_reflection(slice: &[Bits], expected_diff: usize) -> Option<usize> {
    let len = slice.len();
    let mid = len / 2;
    // Iterate from the middle and going left then right, ...
    // (this is because most reflections seemed to be near the middle)
    'outer: for i in (0..mid).rev().interleave(mid..(len - 1)) {
        // How far we look for the reflection, because we don't need to do the whole slice if the
        // reflection is off center
        let reach = (i + 1).min(len - i - 1);
        // Total difference from what we would expect of a reflection
        let mut total_diff = 0;

        for r in 0..reach {
            // How many bits are different
            let diff = (slice[i - r] ^ slice[i + 1 + r]).ones();

            total_diff += diff;

            if total_diff > expected_diff {
                continue 'outer;
            }
        }

        if total_diff == expected_diff {
            return Some(i);
        }
    }

    None
}

pub async fn day13(input: String) -> Result<(String, String)> {
    // Parse the input into cols and rows, each containing bitvecs reprensenting the row / column
    let parsed = input
        .split("\n\n")
        .map(|s| {
            let width = s.find('\n').unwrap();

            let rows = s
                .lines()
                .map(|l| Bits(l.chars().fold(0, |a, x| (a << 1) | (x == '#') as u32)))
                .collect_vec();
            let cols = (0..width)
                .map(|i| {
                    Bits(
                        s[i..]
                            .chars()
                            .step_by(width + 1)
                            .fold(0, |a, x| (a << 1) | (x == '#') as u32),
                    )
                })
                .collect_vec();
            (rows, cols)
        })
        .collect_vec();

    // Part1 and Part2 are the same with expected_diff different
    let (part1, part2) = (0..=1)
        .map(|ediff| {
            parsed
                .iter()
                .map(|(rows, cols)| {
                    let horizontal =
                        find_reflection(rows, ediff).map(|x| Reflection::Horizontal(x + 1));
                    let vertical =
                        find_reflection(cols, ediff).map(|x| Reflection::Vertical(x + 1));

                    horizontal
                        .xor(vertical)
                        .expect("No reflection or both reflections")
                })
                .map(Reflection::summarize)
                .sum::<usize>()
        })
        .collect_tuple().unwrap();

    Ok((part1.to_string(), part2.to_string()))
}
