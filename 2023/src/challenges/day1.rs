use anyhow::{anyhow, Result};

pub async fn day1(input: String) -> Result<(String, String)> {
    let mut sum_part1 = 0;
    for line in input.lines() {
        // Get all the digits in the line
        let mut digits = line.chars().flat_map(|c| c.to_digit(10));
        // Get the first (we return an error if there is none)
        let first = digits.next().ok_or(anyhow!("No digit in the line"))?;
        // Get the last (of the remaining ones), if none, just pick the first again
        let last = digits.last().unwrap_or(first);
        // Concat the digits into a 2 digit number and add it to sum
        sum_part1 += first * 10 + last;
    }

    let spelled_out_numbers = [
        "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    ];
    let mut sum_part2 = 0;
    for line in input.lines() {
        // Loop over each character and its index
        let mut numbers = line.char_indices().flat_map(|(i, c)| {
            // Try to get the digit if it is one
            let digit = c.to_digit(10);
            // Match each spelled out numbers against the substring starting at index i and get the
            // first matching one if any
            let spelled = spelled_out_numbers
                .iter()
                .enumerate()
                .find_map(|(d, s)| line[i..].starts_with(s).then_some(d as u32 + 1));
            // Return either one, note that this returns None if neither, which isn't correct
            // behavior, but since we assume the input to be correct we don't check for that
            digit.or(spelled)
        });

        // Same as part1
        let first = numbers.next().ok_or(anyhow!("No number in the line"))?;
        let last = numbers.last().unwrap_or(first);
        sum_part2 += first * 10 + last;
    }

    Ok((sum_part1.to_string(), sum_part2.to_string()))
}
