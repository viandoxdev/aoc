use std::{collections::HashSet, str::FromStr};

use anyhow::Result;

const MASK_LEFT_COL: u32 = 0b10000_10000_10000_10000_10000;
const MASK_RIGHT_COL: u32 = 0b00001_00001_00001_00001_00001;
const MASK_TOP_ROW: u32 = 0b11111_00000_00000_00000_00000;
const MASK_BOTTOM_ROW: u32 = 0b00000_00000_00000_00000_11111;
const MASK_INSIDE_NEIGHBOR: u32 = 0b00000_00100_01110_00100_00000;
const MASK_GRID: u32 = 0b11111_11111_11111_11111_11111;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Grid(u32);

impl FromStr for Grid {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Ok(Grid(s.chars().filter(|c| !c.is_whitespace()).fold(
            0u32,
            |acc, c| match c {
                '#' => acc << 1 | 1,
                _ => acc << 1,
            },
        )))
    }
}

impl Grid {
    pub fn step(self) -> Self {
        let state = self.0;

        let left = (state >> 1) & !MASK_LEFT_COL;
        let right = (state << 1) & !MASK_RIGHT_COL;
        let up = (state >> 5) & !MASK_TOP_ROW;
        let down = (state << 5) & !MASK_BOTTOM_ROW;

        let horz = left ^ right;
        let vert = up ^ down;
        let carh = left & right;
        let carv = up & down;

        let s0 = horz ^ vert;
        let s1 = (carh ^ carv) ^ (horz & vert);

        Grid((s0 ^ s1) & !(state & s1))
    }

    pub fn step_rec(self, around: Self, inside: Self) -> Self {
        let state = self.0;
        let around = around.0;
        let inside = inside.0;

        let outer_left = (around >> 13 & 1) * MASK_LEFT_COL;
        let outer_right = (around >> 11 & 1) * MASK_RIGHT_COL;
        let outer_up = (around >> 17 & 1) * MASK_TOP_ROW;
        let outer_down = (around >> 7 & 1) * MASK_BOTTOM_ROW;

        // Inside as a the left hand neighbor, so right col
        let inside_left = (inside & MASK_RIGHT_COL).count_ones();
        let inside_right = (inside & MASK_LEFT_COL).count_ones();
        let inside_up = (inside & MASK_BOTTOM_ROW).count_ones();
        let inside_down = (inside & MASK_TOP_ROW).count_ones();

        let left = (state >> 1) & !MASK_LEFT_COL | outer_left;
        let right = (state << 1) & !MASK_RIGHT_COL | outer_right;
        let up = (state >> 5) & !MASK_TOP_ROW | outer_up;
        let down = (state << 5) & !MASK_BOTTOM_ROW | outer_down;

        let horz = left ^ right;
        let vert = up ^ down;
        let carh = left & right;
        let carv = up & down;

        let s0 = horz ^ vert;
        let s1 = (carh ^ carv) ^ (horz & vert);
        let s2 = carh & carv;

        let inside_left = (inside_left + (s2 >> 9 & 0b100 | s1 >> 10 & 0b10 | s0 >> 11 & 1)).min(3);
        let inside_right =
            (inside_right + (s2 >> 11 & 0b100 | s1 >> 12 & 0b10 | s0 >> 13 & 1)).min(3);
        let inside_up = (inside_up + (s2 >> 5 & 0b100 | s1 >> 6 & 0b10 | s0 >> 7 & 1)).min(3);
        let inside_down =
            (inside_down + (s2 >> 15 & 0b100 | s1 >> 16 & 0b10 | s0 >> 17 & 1)).min(3);

        let s0 = s0 & !MASK_INSIDE_NEIGHBOR
            | ((inside_left & 1) << 11
                | (inside_right & 1) << 13
                | (inside_up & 1) << 7
                | (inside_down & 1) << 17);
        let s1 = s1 & !MASK_INSIDE_NEIGHBOR
            | ((inside_left & 0b10) << 10
                | (inside_right & 0b10) << 12
                | (inside_up & 0b10) << 6
                | (inside_down & 0b10) << 16);

        Grid((s0 ^ s1) & !(state & s1) & MASK_GRID)
    }

    pub fn bug_count(self) -> u32 {
        self.0.count_ones()
    }
}

fn part1(mut state: Grid) -> u32 {
    let mut seen_set = HashSet::new();

    while !seen_set.contains(&state) {
        seen_set.insert(state);

        state = state.step();
    }

    state.0.reverse_bits() >> 7
}

fn part2(state: Grid) -> u32 {
    const RUNTIME: usize = 200;
    const LEVELS: usize = RUNTIME * 2;

    let mut states = vec![Grid(0); LEVELS];
    let mut states_next = states.clone();

    states[RUNTIME] = state;

    for _ in 0..RUNTIME {
        for i in 0..LEVELS {
            let around = if i == 0 { Grid(0) } else { states[i - 1] };
            let inside = if i == LEVELS - 1 {
                Grid(0)
            } else {
                states[i + 1]
            };

            states_next[i] = states[i].step_rec(around, inside);
        }

        std::mem::swap(&mut states, &mut states_next);
    }

    states.iter().map(|s| s.bug_count()).sum()
}

pub async fn day24(input: String) -> Result<(String, String)> {
    let state: Grid = input.parse()?;

    Ok((part1(state).to_string(), part2(state).to_string()))
}
