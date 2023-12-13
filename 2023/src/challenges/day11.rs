use anyhow::{Context, Result};
use itertools::Itertools;

fn manhattan_dist((ax, ay): (usize, usize), (bx, by): (usize, usize)) -> usize {
    ax.abs_diff(bx) + ay.abs_diff(by)
}

pub async fn day11(input: String) -> Result<(String, String)> {
    // Parse the input retreive the (unexpanded) galaxies' coordinates
    let (galaxies, _, _) = input.chars().fold(
        (Vec::new(), 0usize, 0usize),
        |(mut galaxies, x, y), c| match c {
            '\n' => (galaxies, 0, y + 1),
            '#' => {
                galaxies.push((x, y));
                (galaxies, x + 1, y)
            }
            _ => (galaxies, x + 1, y),
        },
    );

    let width = input.find('\n').context("Malformed input")?;

    // Get the y coordinates of all the rows which get expanded
    let expanded_rows = input
        .lines()
        .enumerate()
        // Keep only the ones that are only ....(...)....
        .flat_map(|(i, l)| l.chars().all(|c| c == '.').then_some(i))
        .collect_vec();
    let expanded_columns =
        (0..width) // All the columns
            // Step by skips enough characters to get to go down one row, and input[c..] gets to the
            // column c in the first row, so we check the entire column c and make sure its all .
            .filter(|&c| input[c..].chars().step_by(width + 1).all(|c| c == '.'))
            .collect_vec();

    // Get the sum of manhattan distances between every galaxies given the expansion factor
    let solve = |exp_fac: usize| {
        galaxies
            .iter()
            // Expand the galaxies
            .map(|&(x, y)| {
                // Count the number of expanded columns and rows that are before the current galaxy
                let exp_cols = expanded_columns.iter().filter(|&&c| c < x).count();
                let exp_rows = expanded_rows.iter().filter(|&&r| r < y).count();

                // Move the galaxy accordingly
                (x + exp_cols * (exp_fac - 1), y + exp_rows * (exp_fac - 1))
            })
            // Get every pair, compute manhattan distance and sum
            .tuple_combinations()
            .map(|(a, b)| manhattan_dist(a, b))
            .sum::<usize>()
    };

    let part1 = solve(2);
    let part2 = solve(1_000_000);

    Ok((part1.to_string(), part2.to_string()))
}
