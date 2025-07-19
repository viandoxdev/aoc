use std::{collections::HashMap, fmt::Display, str::FromStr};

use anyhow::{Context, Error, Result};
use itertools::Itertools;
use num::Integer;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tile {
    Empty,
    Block,
    Rock,
}

impl TryFrom<char> for Tile {
    type Error = Error;
    fn try_from(value: char) -> std::result::Result<Self, Self::Error> {
        match value {
            '#' => Ok(Self::Block),
            '.' => Ok(Self::Empty),
            'O' => Ok(Self::Rock),
            _ => Err(anyhow::anyhow!("Invalid tile")),
        }
    }
}

impl Display for Tile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Rock => write!(f, "O"),
            Self::Empty => write!(f, "."),
            Self::Block => write!(f, "#"),
        }
    }
}

type Bits = u128;

#[derive(Debug, Clone)]
struct State {
    width: usize,
    height: usize,
    grid: Vec<Vec<Tile>>,
    rocks: Vec<(usize, usize)>,
}

impl FromStr for State {
    type Err = Error;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let grid: Vec<Vec<Tile>> = s
            .lines()
            .map(|l| {
                l.chars()
                    .map(|c| c.try_into())
                    .collect::<Result<Vec<Tile>>>()
            })
            .try_collect()?;

        let width = grid.first().context("empty grid")?.len();
        let height = grid.len();

        let rocks = grid
            .iter()
            .enumerate()
            .flat_map(|(y, r)| r.iter().enumerate().map(move |(x, &t)| (x, y, t)))
            .filter_map(|(x, y, t)| (t == Tile::Rock).then_some((x, y)))
            .collect_vec();

        Ok(Self {
            grid,
            width,
            height,
            rocks,
        })
    }
}

impl State {
    fn key(&self) -> Vec<Bits> {
        const BITS: usize = Bits::BITS as usize;
        let len = (self.width * self.height).div_ceil(BITS);
        let mut key: Vec<Bits> = vec![0; len];

        for &(x, y) in &self.rocks {
            let rock = y * self.width + x;
            let (chunk, bit) = rock.div_rem(&BITS);
            key[chunk] |= 1 << bit;
        }

        key
    }

    fn tilt_north(&mut self) {
        self.rocks.clear();

        let mut farthest = vec![0; self.width];

        for y in 0..self.height {
            for (x, far) in farthest.iter_mut().enumerate() {
                match self.grid[y][x] {
                    Tile::Block => {
                        *far = y + 1;
                    }
                    Tile::Rock => {
                        self.rocks.push((x, *far));
                        self.grid[y][x] = Tile::Empty;
                        self.grid[*far][x] = Tile::Rock;
                        *far += 1;
                    }
                    Tile::Empty => {}
                }
            }
        }
    }

    fn tilt_cycle(&mut self) {
        self.rocks.clear();
        let mut farthest = [0; 100];

        for y in 0..self.height {
            for (x, far) in farthest.iter_mut().enumerate().take(self.width) {
                match self.grid[y][x] {
                    Tile::Block => {
                        *far = y + 1;
                    }
                    Tile::Rock => {
                        self.grid[y][x] = Tile::Empty;
                        self.grid[*far][x] = Tile::Rock;
                        *far += 1;
                    }
                    Tile::Empty => {}
                }
            }
        }

        farthest.fill(0);

        for x in 0..self.width {
            for (y, far) in farthest.iter_mut().enumerate().take(self.height) {
                match self.grid[y][x] {
                    Tile::Block => {
                        *far = x + 1;
                    }
                    Tile::Rock => {
                        self.grid[y][x] = Tile::Empty;
                        self.grid[y][*far] = Tile::Rock;
                        *far += 1;
                    }
                    Tile::Empty => {}
                }
            }
        }

        farthest.fill(self.height - 1);

        for y in (0..self.height).rev() {
            for (x, far) in farthest.iter_mut().enumerate().take(self.width) {
                match self.grid[y][x] {
                    Tile::Block => {
                        *far = y.saturating_sub(1);
                    }
                    Tile::Rock => {
                        self.grid[y][x] = Tile::Empty;
                        self.grid[*far][x] = Tile::Rock;
                        *far = (*far).saturating_sub(1);
                    }
                    Tile::Empty => {}
                }
            }
        }

        farthest.fill(self.width - 1);

        for x in (0..self.width).rev() {
            for (y, far) in farthest.iter_mut().enumerate().take(self.height) {
                match self.grid[y][x] {
                    Tile::Block => {
                        *far = x.saturating_sub(1);
                    }
                    Tile::Rock => {
                        self.rocks.push((*far, y));
                        self.grid[y][x] = Tile::Empty;
                        self.grid[y][*far] = Tile::Rock;
                        *far = (*far).saturating_sub(1);
                    }
                    Tile::Empty => {}
                }
            }
        }
    }

    fn load(&self) -> usize {
        self.rocks.iter().map(|&(_, y)| self.height - y).sum()
    }
}

pub async fn day14(input: String) -> Result<(String, String)> {
    let state: State = input.parse()?;

    let part1 = {
        let mut state = state.clone();
        state.tilt_north();
        state.load()
    };
    let part2 = {
        let mut state = state;
        let mut viewed_states = HashMap::<Vec<Bits>, u64>::new();
        let mut skipped = false;

        let mut i = 0;
        let max = 1_000_000_000;
        while i < max {
            state.tilt_cycle();

            if !skipped {
                let key = state.key();
                if let Some(start) = viewed_states.get(&key) {
                    skipped = true;
                    let cycle_len = i - start;
                    let cycles = (max - i) / cycle_len;
                    i += cycles * cycle_len;
                }
                viewed_states.insert(key, i);
            }

            i += 1;
        }

        state.load()
    };

    Ok((part1.to_string(), part2.to_string()))
}
