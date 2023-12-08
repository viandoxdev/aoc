use anyhow::{anyhow, Result};
use itertools::Itertools;

pub async fn day3(input: String) -> Result<(String, String)> {
    // Compute line width (+1 for the new line)
    let line_width = input.find('\n').ok_or(anyhow!("Malformed input"))? as i32 + 1;
    // Get the indices of the neighbours of a character
    let neighbours = |i| {
        // Fun fact, this is actually bugged in a way: Because we haven't actually created the 2D
        // array we're working with here (instead we've just kept it 1D), the only bounds checking
        // done is the one done by the slice get function, which only checks for out of bounds in
        // the 1D sense. As such if i is the index of a character on the edge of the grid, this
        // functions returns some wrong indices. For example, i + 1 should be on the next line,
        // which is *not* a neighbour. However because we kept the newlines in the input, this
        // instead points to a newline, and so this still works if we just assume '\n' to behave
        // like '.'. Same thing for all the -1 but the other way.
        [
            i - line_width - 1,
            i - line_width,
            i - line_width + 1,
            i - 1,
            i + 1,
            i + line_width - 1,
            i + line_width,
            i + line_width + 1,
        ]
        .into_iter()
    };

    let mut part1_sum = 0;

    // Get the chars in a vec since Strings aren't indexable in rust
    let input_chars = input.chars().collect_vec();

    // The current number we are going over
    let mut number = None;
    let mut is_part = false;
    // The index of the start of the number
    let mut span_start = 0;
    let mut part_number_spans = Vec::new();

    for (i, &c) in input_chars.iter().enumerate() {
        let i = i as i32;

        // Check if symbol and set is_part
        if c.is_digit(10) {
            let has_symbol = neighbours(i)
                .flat_map(|i| input_chars.get(usize::try_from(i).ok()?).copied())
                .any(|x| !matches!(x, '0'..='9' | '.' | '\n'));
            is_part = is_part || has_symbol;
        }

        if let Some(n) = number {
            // We already started a number

            if let Some(d) = c.to_digit(10) {
                // Continue it
                number = Some(n * 10 + d);
            } else {
                // We reached the end

                if is_part {
                    // The number was a part number, use it for part1 and keep its span for part2
                    part1_sum += n;
                    part_number_spans.push(span_start..(i as usize));
                }

                number = None;
                is_part = false;
            }
        } else {
            // We haven't started a number
            // Start one if we have a digit
            number = c.to_digit(10);
            span_start = i as usize;
        }
    }

    let mut part2_sum = 0;
    // Iterate over the indices of all the stars in the input
    for i in input_chars
        .iter()
        .enumerate()
        .flat_map(|(i, &c)| (c == '*').then_some(i))
    {
        // Get the spans each neighbour belongs to (if any), this can return duplicates if two
        // neighbours belong to the same span
        let dup_spans = neighbours(i as i32)
            .flat_map(|i| usize::try_from(i))
            .flat_map(|i| part_number_spans.iter().find(|s| s.contains(&i)));

        // Remove duplicates
        let mut spans = Vec::with_capacity(8); // Can't have more spans than neighbours
        for span in dup_spans {
            if !spans.contains(&span) {
                spans.push(span)
            }
        }

        let (len, prod) = spans
            .into_iter()
            .map(|s| {
                // Parse the number in the span
                input_chars[s.clone()]
                    .iter()
                    .fold(0u32, |a, c| a * 10 + c.to_digit(10).unwrap_or_default())
            })
            // get the length and product at once
            .fold((0, 1), |(l, p), n| (l + 1, p * n));

        if len == 2 {
            part2_sum += prod;
        }
    }

    Ok((part1_sum.to_string(), part2_sum.to_string()))
}
