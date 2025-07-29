use std::
    fmt::Display
;

use anyhow::{Result, anyhow};
use itertools::Itertools;
use tracing::trace;

use crate::intcode::Intcode;

const WIDTH: usize = 35;
const HEIGHT: usize = 61;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Movement {
    TurnLeft,
    TurnRight,
    Forward(u8),
}

#[derive(Debug, Clone, Copy)]
enum Pattern {
    First,
    Second,
    Third,
}

impl Pattern {
    fn write_into(self, buf: &mut Vec<i64>) {
        match self {
            Self::First => buf.push(b'A' as i64),
            Self::Second => buf.push(b'B' as i64),
            Self::Third => buf.push(b'C' as i64),
        }
    }
}

struct DisplaySequence<'a>(&'a [Movement]);
struct DisplayPatterns<'a>(&'a [Pattern]);

impl<'a> Display for DisplayPatterns<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut first = false;
        for p in self.0 {
            if first {
                first = false;
                write!(f, "{p}")?;
            } else {
                write!(f, " {p}")?;
            }
        }
        Ok(())
    }
}

impl<'a> Display for DisplaySequence<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut first = false;
        for m in self.0 {
            if first {
                first = false;
                write!(f, "{m}")?;
            } else {
                write!(f, " {m}")?;
            }
        }
        Ok(())
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::First => write!(f, "A"),
            Self::Second => write!(f, "B"),
            Self::Third => write!(f, "C"),
        }
    }
}

impl Display for Movement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TurnLeft => write!(f, "L"),
            Self::TurnRight => write!(f, "R"),
            Self::Forward(n) => write!(f, "{n}"),
        }
    }
}

impl Movement {
    const MAX_MOVEMENT: u8 = 32;
    fn key(self) -> u8 {
        match self {
            Self::TurnLeft => 0,
            Self::TurnRight => 1,
            Self::Forward(n) => n + 2,
        }
    }

    // How many integers would it take to encode this movement
    fn len(self) -> usize {
        match self {
            Self::TurnLeft => 2,
            Self::TurnRight => 2,
            Self::Forward(n) => {
                if n >= 10 {
                    3
                } else {
                    2
                }
            }
        }
    }

    fn write_into(self, buf: &mut Vec<i64>) {
        match self {
            Self::TurnLeft => buf.push(b'L' as i64),
            Self::TurnRight => buf.push(b'R' as i64),
            Self::Forward(n) => buf.extend(n.to_string().chars().map(|c| c as i64)),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn turn_right(self) -> Self {
        match self {
            Self::Up => Self::Right,
            Self::Right => Self::Down,
            Self::Down => Self::Left,
            Self::Left => Self::Up,
        }
    }
    fn turn_left(self) -> Self {
        match self {
            Self::Up => Self::Left,
            Self::Left => Self::Down,
            Self::Down => Self::Right,
            Self::Right => Self::Up,
        }
    }
    fn position(self, (x, y): (usize, usize)) -> Option<(usize, usize)> {
        match self {
            Self::Up => (y > 0).then_some((x, y.wrapping_sub(1))),
            Self::Down => (y < HEIGHT - 1).then_some((x, y.wrapping_add(1))),
            Self::Left => (x > 0).then_some((x.wrapping_sub(1), y)),
            Self::Right => (x < WIDTH - 1).then_some((x.wrapping_add(1), y)),
        }
    }
}

impl ToString for Direction {
    fn to_string(&self) -> String {
        match self {
            Self::Up => "^",
            Self::Right => ">",
            Self::Down => "v",
            Self::Left => "<",
        }
        .to_string()
    }
}

impl From<char> for Direction {
    fn from(value: char) -> Self {
        match value {
            '^' => Self::Up,
            '>' => Self::Right,
            'v' => Self::Down,
            '<' => Self::Left,
            unk => panic!("Invalid direction: {unk}"),
        }
    }
}

fn pattern_length(pat: &[Movement]) -> usize {
    pat.iter().copied().map(Movement::len).sum()
}

struct Grid {
    scaffolding: [[bool; WIDTH]; HEIGHT],
    facing: Direction,
    robot: (usize, usize),
    intersections: Vec<(usize, usize)>,
}

impl Grid {
    fn try_move(&self, pos: (usize, usize), direction: Direction) -> Option<(usize, usize)> {
        let (x, y) = direction.position(pos)?;
        self.scaffolding[y][x].then_some((x, y))
    }
    fn get_sequence(&self) -> Vec<Movement> {
        let mut pos = self.robot;
        let mut facing = self.facing;
        let mut seq = Vec::new();

        loop {
            if let Some(new_pos) = self.try_move(pos, facing) {
                if let Some(Movement::Forward(n)) = seq.last_mut() {
                    *n += 1;
                } else {
                    seq.push(Movement::Forward(1));
                }
                pos = new_pos;
            } else if self.try_move(pos, facing.turn_right()).is_some() {
                seq.push(Movement::TurnRight);
                facing = facing.turn_right();
            } else if self.try_move(pos, facing.turn_left()).is_some() {
                seq.push(Movement::TurnLeft);
                facing = facing.turn_left();
            } else {
                break seq;
            }
        }
    }

    fn compress(sequence: &[Movement]) -> Option<([&[Movement]; 3], Vec<Pattern>)> {
        // Limit is 20 ASCII characters, a movement is at least one, and needs to be followed by
        // either a comma or a newline, so the maximum is 10.
        let max_len = 10;
        // List of patterns: patterns[len][movement.key() as usize] is the list of all patterns of
        // length len + 1 that start with movement and how many instances of said pattern there is
        // in the sequence (type: Vec<Vec<Vec<(u32, &[Movement])>>>)
        let mut patterns = vec![vec![vec![]; Movement::MAX_MOVEMENT as usize]; max_len];

        // Build the list
        for l in 1..=max_len {
            for pat in sequence.windows(l) {
                let pats = &mut patterns[l - 1][pat[0].key() as usize];

                if let Some((count, _)) = pats.iter_mut().find(|(_, p)| *p == pat) {
                    *count += 1;
                } else {
                    pats.push((1u32, pat))
                }
            }
        }

        let patterns = patterns
            .into_iter()
            .enumerate()
            .flat_map(|(_, pats)| {
                pats.into_iter()
                    .flat_map(move |pats| pats.into_iter().map(move |(count, pat)| (count, pat)))
            })
            .filter(|&(_, pat)| pattern_length(pat) <= 21)
            .sorted_by_key(|&(count, pat)| -(pat.len() as i32) * (count as i32))
            .collect_vec();

        trace!("Patterns: (first 20)");
        for &(count, pat) in patterns.iter().take(20) {
            trace!(
                " - {} ({} * {count} = {}) ",
                DisplaySequence(pat),
                pat.len(),
                pat.len() as u32 * count
            );
        }

        trace!("Trying patterns");

        for &(_, first_pattern) in &patterns {
            trace!("FIRST: {}", DisplaySequence(first_pattern));
            // All the sub sequences, split by all the instances of main_pattern.
            // There may be empty sub sequences if the main_pattern repeats consecutively
            let (sub_sequences, _) = sequence
                .windows(first_pattern.len())
                .enumerate()
                .filter(|&(_, slice)| slice == first_pattern)
                .fold((vec![sequence], 0), |(mut sub_seqs, at), (i, _)| {
                    // check if pattern overlaps itself
                    if i < at {
                        // Ignore
                        return (sub_seqs, at);
                    }

                    if let Some(last) = sub_seqs.last_mut() {
                        *last = &last[0..i - at];
                    }
                    let new_at = i + first_pattern.len();
                    sub_seqs.push(&sequence[new_at..]);
                    (sub_seqs, new_at)
                });

            trace!("  Subsequences:");
            for &sub_seq in &sub_sequences {
                trace!("    - {}", DisplaySequence(sub_seq));
            }

            // Go through all the patterns all over again and count how many time they appear in
            // the sub sequences, to see which second pattern is the best candidate
            let new_patterns = patterns
                .iter()
                .map(|&(_, pat)| {
                    // How many times this pattern appears in the sub sequences
                    let new_count = sub_sequences
                        .iter()
                        .filter(|&&seq| pat.len() <= seq.len())
                        .map(|&seq| seq.windows(pat.len()).filter(|&s| s == pat).count())
                        .sum::<usize>();
                    (new_count, pat)
                })
                .sorted_by_key(|&(count, pat)| -(pat.len() as i32) * (count as i32));

            // Choose a second pattern among the best candidates
            'second: for (_, second_pattern) in new_patterns {
                trace!("  SECOND: {}", DisplaySequence(second_pattern));
                // Look through the sub sequences, and see if there is a reoccuring pattern left
                // after removing second_pattern

                // The potential third pattern
                let mut third_pattern = None;

                for &sub_seq in &sub_sequences {
                    trace!("    Checking subsequence {}", DisplaySequence(sub_seq));
                    let mut i = 0;
                    // Index of the start of the third pattern
                    let mut start = None;
                    loop {
                        if i >= sub_seq.len() {
                            if let Some(start) = start
                                && third_pattern.is_none()
                            {
                                trace!(
                                    "    Identified third pattern: {}",
                                    DisplaySequence(&sub_seq[start..])
                                );
                                third_pattern = Some(&sub_seq[start..]);
                            }

                            break;
                        }

                        if i + second_pattern.len() <= sub_seq.len()
                            && &sub_seq[i..i + second_pattern.len()] == second_pattern
                        {
                            if let Some(start) = start
                                && third_pattern.is_none()
                            {
                                if pattern_length(&sub_seq[start..i]) > 21 {
                                    // The third pattern is too long, we must have made a mistake
                                    // when choosing a prior pattern.
                                    trace!("    Identified third pattern, but it is too long");
                                    continue 'second;
                                }

                                trace!(
                                    "    Identified third pattern: {}",
                                    DisplaySequence(&sub_seq[start..i])
                                );
                                third_pattern = Some(&sub_seq[start..i])
                            }

                            i += second_pattern.len();

                            continue;
                        }

                        match third_pattern {
                            None => {
                                if start.is_none() {
                                    start = Some(i);
                                }

                                i += 1;
                            }
                            Some(third_pattern) => {
                                if i + third_pattern.len() > sub_seq.len()
                                    || &sub_seq[i..i + third_pattern.len()] != third_pattern
                                {
                                    trace!(
                                        "    Found mismatch: {}",
                                        DisplaySequence(
                                            &sub_seq[i..sub_seq.len().min(i + third_pattern.len())]
                                        )
                                    );
                                    // We have found a sub sequence that fits neither first_pattern,
                                    // second_pattern or third_pattern : We choose the wrong
                                    // first_pattern or third_pattern

                                    // Choose a new second pattern
                                    continue 'second;
                                }

                                i += third_pattern.len();
                            }
                        };
                    }
                }

                // We have found a first, second and third pattern that make up the entire sequence
                let third_pattern = third_pattern.expect("No third pattern");

                trace!("Found all three patterns:");
                trace!("  A: {}", DisplaySequence(first_pattern));
                trace!("  B: {}", DisplaySequence(second_pattern));
                trace!("  C: {}", DisplaySequence(third_pattern));

                let patterns = [
                    (Pattern::First, first_pattern),
                    (Pattern::Second, second_pattern),
                    (Pattern::Third, third_pattern),
                ];
                let mut pat_seq = Vec::new();
                let mut i = 0;

                trace!(
                    "Relocating patterns in sequence: {}",
                    DisplaySequence(sequence)
                );
                'outer: while i < sequence.len() {
                    for (key, pat) in patterns {
                        if i + pat.len() <= sequence.len() && &sequence[i..i + pat.len()] == pat {
                            trace!(
                                "Identified pattern {key} at {i} : {}",
                                DisplaySequence(
                                    &sequence[i.saturating_sub(2)
                                        ..sequence.len().min(i + pat.len() + 2)]
                                )
                            );
                            pat_seq.push(key);
                            i += pat.len();
                            continue 'outer;
                        }
                    }

                    trace!(
                        "Found sequence mismatch: {}",
                        DisplaySequence(&sequence[i..])
                    );

                    panic!("Couldn't find pattern in sequence, this shouln't be possible");
                }

                trace!("Final sequence: {}", DisplayPatterns(&pat_seq));

                return Some(([first_pattern, second_pattern, third_pattern], pat_seq));
            }
        }

        // We somehow haven't found any working first pattern
        None
    }

    fn from_program(mut program: Intcode<171>) -> Result<Self> {
        let mut scaffolding = [[false; WIDTH]; HEIGHT];
        let mut facing = Direction::Up;
        let mut robot = (0, 0);
        let mut intersections = Vec::new();

        program
            .run(&[])?
            .into_iter()
            .fold((0usize, 0usize), |(x, y), c| {
                let c = char::from_u32(c as u32).expect("Invalid ascii");
                match c {
                    '.' => {}
                    '\n' => return (0, y + 1),
                    '#' | '>' | '<' | '^' | 'v' => {
                        if c != '#' {
                            facing = Direction::from(c);
                            robot = (x, y);
                        }

                        scaffolding[y][x] = true;

                        if y >= 2
                            && x > 0
                            && x < WIDTH - 1
                            && scaffolding[y - 1][x - 1]
                            && scaffolding[y - 1][x]
                            && scaffolding[y - 1][x + 1]
                            && scaffolding[y - 2][x]
                        {
                            intersections.push((x, y - 1));
                        }
                    }
                    _ => panic!("Unexpected output: '{c}'"),
                };

                (x + 1, y)
            });

        Ok(Self {
            scaffolding,
            facing,
            robot,
            intersections,
        })
    }
}

pub async fn day17(input: String) -> Result<(String, String)> {
    let mut program: Intcode<171> = input.parse()?;
    let grid = Grid::from_program(program.clone())?;

    let part1 = grid
        .intersections
        .iter()
        .map(|&(x, y)| x * y)
        .sum::<usize>();

    let part2 = {
        program.program[0] = 2;

        let seq = grid.get_sequence();
        let (pats, seq) = Grid::compress(&seq).expect("Couldn't compress");

        let mut input = Vec::new();

        // Main movement function
        for pat in seq {
            pat.write_into(&mut input);
            input.push(b',' as i64);
        }

        input.pop();
        input.push(b'\n' as i64);

        // Movement functions
        for pat in pats {
            for &mov in pat {
                mov.write_into(&mut input);
                input.push(b',' as i64);
            }

            input.pop();
            input.push(b'\n' as i64);
        }

        // No video feed
        input.push(b'n' as i64);
        input.push(b'\n' as i64);

        program
            .run(&input)?
            .last()
            .copied()
            .ok_or_else(|| anyhow!("No output"))?
    };

    Ok((part1.to_string(), part2.to_string()))
}

