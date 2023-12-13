use anyhow::{Context, Result};
use itertools::Itertools;

use Direction::*;
use Pipe::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Direction {
    Up,
    Right,
    Down,
    Left,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Pipe {
    Vertical,
    Horizontal,
    UpLeft,
    UpRight,
    DownLeft,
    DownRight,
    Nothing,
}

impl From<char> for Pipe {
    fn from(value: char) -> Self {
        match value {
            '|' => Vertical,
            '-' => Horizontal,
            'F' => DownRight,
            '7' => DownLeft,
            'L' => UpRight,
            'J' => UpLeft,
            _ => Nothing,
        }
    }
}

pub async fn day10(input: String) -> Result<(String, String)> {
    // input as a Vec<char> to index individual chars.
    let grid = input.chars().collect_vec();
    // Grid of pipes, with only the pipes that are in the loop
    let mut grid_loop = vec![Nothing; grid.len()];
    let width = grid
        .iter()
        .position(|&x| x == '\n')
        .context("Malformed input")?;
    let start_coord = grid
        .iter()
        .position(|&x| x == 'S')
        .context("Malformed input")?;

    // Get the index of all the valid neighbours (with the direction to it)
    let neighbours = |x: usize| {
        // Get options of the indices (make sure no underflow)
        [
            x.checked_sub(width + 1).map(|i| (Up, i)),
            x.checked_add(1).map(|i| (Right, i)),
            x.checked_add(width + 1).map(|i| (Down, i)),
            x.checked_sub(1).map(|i| (Left, i)),
        ]
        .into_iter()
        .flatten() // Get rid of underflows
        .filter(|&(_, x)| x < grid.len()) // Get rid of out of bounds
        // Get rid of out of row index (accidentaly indexing the next / previous row)
        .filter_map(|(dir, i)| (grid[i] != '\n').then_some((dir, i, grid[i])))
    };

    // Filter the neighbours depending on the current point
    fn valid_steps_from<I: Iterator<Item = (Direction, usize, char)>>(
        iter: I,
        cur: char,
    ) -> impl Iterator<Item = (Direction, usize, char)> {
        iter.filter(move |&(dir, _, _)| match cur {
            'S' => true,
            '|' => matches!(dir, Up | Down),
            '-' => matches!(dir, Right | Left),
            'L' => matches!(dir, Up | Right),
            'J' => matches!(dir, Up | Left),
            'F' => matches!(dir, Down | Right),
            '7' => matches!(dir, Down | Left),
            _ => false,
        })
    }

    // Filter the neighbours depending on it can accept us (only used for S)
    fn valid_steps_to<I: Iterator<Item = (Direction, usize, char)>>(
        mut iter: I,
    ) -> Option<(Direction, usize)> {
        iter.find(|&(dir, _, c)| match dir {
            Up => matches!(c, '|' | '7' | 'F'),
            Right => matches!(c, '-' | 'J' | '7'),
            Down => matches!(c, '|' | 'J' | 'L'),
            Left => matches!(c, '-' | 'L' | 'F'),
        })
        .map(|(d, i, _)| (d, i))
    }

    // Get the next step from all the possible steps
    fn next_step<I: Iterator<Item = (Direction, usize, char)>>(
        iter: I,
        prev: usize,
        cur: usize,
        grid: &[char],
    ) -> Option<(Direction, usize)> {
        valid_steps_to(valid_steps_from(
            // Don't walk back
            iter.filter(|&(_, i, _)| i != prev),
            grid[cur],
        ))
    }

    let part1 = 'block: {
        // Store the previous to avoid back steps
        let mut prev_back = start_coord;
        let mut prev_front = start_coord;
        // We walk front and back at the same time to get the max distance
        // Indices of front and back
        let mut back = start_coord;
        let mut front = start_coord;
        // Number of steps taken so far
        let mut steps = 0;

        loop {
            // Get the new tile and direction we took front and back
            let (nb_dir, new_back) =
                next_step(neighbours(back), prev_back, back, &grid).context("No next")?;
            // Same thing, but go through neighbours in reverse to avoid going the same direction
            // at the start
            let (nf_dir, new_front) =
                next_step(neighbours(front).rev(), prev_front, front, &grid).context("No next")?;

            grid_loop[new_back] = Pipe::from(grid[new_back]);
            grid_loop[new_front] = Pipe::from(grid[new_front]);

            // Set the pipe for the start, we have to figure it out from where it goes
            if back == start_coord && front == start_coord {
                grid_loop[start_coord] = match (nb_dir, nf_dir) {
                    (Up, Down) | (Down, Up) => Vertical,
                    (Right, Up) | (Up, Right) => UpRight,
                    (Left, Up) | (Up, Left) => UpLeft,
                    (Right, Down) | (Down, Right) => DownRight,
                    (Left, Down) | (Down, Left) => DownLeft,
                    (Right, Left) | (Left, Right) => Horizontal,
                    _ => unreachable!(),
                }
            }

            steps += 1;

            // Once the front and back both reached the same tile we know we're at the furthest
            // point in the loop (and we've travelled the whole loop)
            if new_back == new_front {
                break 'block steps;
            }

            prev_back = back;
            prev_front = front;
            back = new_back;
            front = new_front;
        }
    };

    let mut part2 = 0;
    // We do part2 using the even odd rule, scanning each line horizontally
    for line in grid_loop.chunks(width + 1) {
        part2 += line
            .iter()
            // This fold counts the number of inside tiles are in the line
            .fold(
                (false, None, 0),
                |(inside, last_angle, count), &boundary| match (boundary, last_angle) {
                    // We crossed a vertical pipe, flip inside
                    (Vertical, _) => (!inside, None, count),
                    // We crossed an angle when we don't have one currently
                    // -> Remember it
                    (UpRight, None) => (inside, Some(UpRight), count),
                    (DownRight, None) => (inside, Some(DownRight), count),

                    // We crossed an angle that makes the pipe cross vertically
                    // i.e └──(...)──┐ or ┌──(...)──┘ -> flip inside
                    (UpLeft, Some(DownRight)) => (!inside, None, count),
                    (DownLeft, Some(UpRight)) => (!inside, None, count),

                    // We crossed an angle that doesn't make the pipe cross vertically
                    // i.e  └──(...)──┘ or ┌──(...)──┐ -> reset angle and keep inside the same
                    (UpLeft, Some(UpRight)) => (inside, None, count),
                    (DownLeft, Some(DownRight)) => (inside, None, count),

                    // We got an horizontal, keep the angles the same and do nothing
                    (Horizontal, Some(_)) => (inside, last_angle, count),
                    // We got an empty tile that is inside, count it
                    (Nothing, None) if inside => (inside, None, count + 1),
                    // We got an empty tile that is not inside, skip it
                    (Nothing, None) => (inside, None, count),

                    // Any other state is invalid
                    _ => unreachable!(),
                },
            )
            .2;
    }

    Ok((part1.to_string(), part2.to_string()))
}
