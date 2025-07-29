use anyhow::{Result, anyhow};

fn digits(str: &str) -> Result<Vec<u8>> {
    str.chars().rev().map(|x| x.to_digit(10).map(|d| d as u8)).collect::<Option<Vec<u8>>>().ok_or_else(|| anyhow!("Bad number"))
}

#[derive(Debug, Clone, Copy)]
enum Double {
    /// No double yet
    Missing = 0,
    /// A double that can be part of a larger group
    Naive = 1,
    /// A double that is no more than two digits
    Real = 2,
}

impl Double {
    fn next(self, digit: u8, prev: Option<u8>, prev_group: GroupSize, remaining: u8) -> Self {
        match (self, prev_group) {
            // Nothing better than having a real double, so if we have one, keep it
            (Self::Real, _) => Self::Real,
            // If we find two matching digits not part of a previous group at the end of the code
            // we can conclude that they are real without checking the next digit (because there
            // isn't any)
            (_, GroupSize::One) if Some(digit) == prev && remaining == 1 => Self::Real,
            // If we are in a group of size two or more we know we at least have a naive double
            (_, _) if Some(digit) == prev => Self::Naive,
            // The previous digit isn't the same as the current one, so the previous group ended,
            // if it ended with size two, we have a real double
            (_, GroupSize::Two) => Self::Real,
            (_, _) => self,
        }
    }
    fn naive(self) -> bool {
        matches!(self, Self::Naive | Self::Real)
    }
    fn real(self) -> bool {
        matches!(self, Self::Real)
    }
}

#[derive(Debug, Clone, Copy)]
enum GroupSize {
    One = 0,
    Two = 1,
    ThreeOrMore = 2,
}

impl GroupSize {
    fn next(self, digit: u8, previous: Option<u8>) -> Self {
        match (self, Some(digit) == previous) {
            (Self::One, true) => Self::Two,
            (Self::Two, true) => Self::ThreeOrMore,
            (Self::ThreeOrMore, true) => Self::ThreeOrMore,
            (_, false) => Self::One,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Key {
    /// What kind of double is present in the code (3 values)
    double: Double,
    /// Has the code any chance to be less than min_code (2 values)
    low: bool,
    /// Has the code any chance to be more than max_code (2 values)
    high: bool,
    /// The size of the group of matching characters the previous digit is part of (3 values)
    previous_group: GroupSize,
    // What the previous digit was if any (11 values)
    previous_digit: Option<u8>,
    // How many digits remain to be choosen (7 values)
    remaining: u8,
}

impl Key {
    pub const KEY_COUNT: usize = 2 * 2 * 3 * 3 * 7 * 11;

    fn new() -> Self {
        Self {
            double: Double::Missing,
            low: true,
            high: true,
            previous_group: GroupSize::One,
            previous_digit: None,
            remaining: 6,
        }
    }

    fn to_usize(self) -> usize {
        (self.low as usize | (self.high as usize) << 1)
        + self.double as usize * 2 * 2
        + self.previous_group as usize * 2 * 2 * 3
        + self.remaining as usize * 2 * 2 * 3 * 3
        + self.previous_digit.unwrap_or(10) as usize * 2 * 2 * 3 * 3 * 7
    }
}

fn get_comb(key: Key, memo: &mut [Option<(u32, u32)>], min_code: &[u8], max_code: &[u8]) -> (u32, u32) {
    let index = key.to_usize();

    // Pick from memo if already computed
    if let Some(comb) = memo[index] {
        return comb;
    }

    // Base cases
    if key.remaining == 0 {
        return (key.double.naive() as u32, key.double.real() as u32);
    }

    let dindex = key.remaining as usize - 1;
    let comb = (key.previous_digit.unwrap_or(0)..=9)
        .filter(|&d| (!key.low || min_code[dindex] <= d) && (!key.high || d <= max_code[dindex]))
        .map(|d| {
            get_comb(
                Key {
                    previous_digit: Some(d), 
                    remaining: key.remaining - 1,
                    double: key.double.next(d, key.previous_digit, key.previous_group, key.remaining),
                    low: key.low && d == min_code[dindex],
                    high: key.high && d == max_code[dindex],
                    previous_group: key.previous_group.next(d, key.previous_digit),
                },
                memo,
                min_code,
                max_code
            )
        })
        .reduce(|(a, b), (c, d)| (a + c, b + d))
        .unwrap_or_default();

    memo[index] = Some(comb);

    comb
}

pub async fn day04(input: String) -> Result<(String, String)> {
    let (min_code, max_code) = input.trim().split_once("-").ok_or_else(|| anyhow!("Bad input"))?;
    let (min_code, max_code): (Vec<u8>, Vec<u8>) = (digits(min_code)?, digits(max_code)?);

    let (part1, part2) = {
        let mut memo = [None; Key::KEY_COUNT];
        get_comb(Key::new(), &mut memo, &min_code, &max_code)
    };

    Ok((part1.to_string(), part2.to_string()))
}

