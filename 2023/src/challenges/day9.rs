use anyhow::Result;
use itertools::Itertools;

pub async fn day9(input: String) -> Result<(String, String)> {
    // Sum for part1 and part2
    let mut part1 = 0;
    let mut part2 = 0;

    for line in input.lines() {
        // List of all the bellow rows (the ones that are derived)
        let mut rows = Vec::new();
        // Parse the row
        let mut top_row: Vec<i64> = line.split_whitespace().map(|x| x.parse()).try_collect()?;

        // Get the new top row by deriving the top one
        while top_row.iter().any(|&x| x != 0) {
            let new_top = top_row.windows(2).map(|s| s[1] - s[0]).collect_vec();
            rows.push(top_row);
            top_row = new_top;
        }

        // The top row is full of zeroes so extending it is easy
        top_row.push(0);
        top_row.insert(0, 0);

        // Expend the rest (consuming them)
        while !rows.is_empty() {
            let &left_delta = top_row.first().unwrap();
            let &right_delta = top_row.last().unwrap();
            top_row = rows.pop().unwrap();
            top_row.push(top_row.last().unwrap() + right_delta);
            top_row.insert(0, top_row.first().unwrap() - left_delta);
        }

        // Top row is now the first row we parsed, extended
        part1 += top_row.last().unwrap();
        part2 += top_row.first().unwrap();
    }

    Ok((part1.to_string(), part2.to_string()))
}

