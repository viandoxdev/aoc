use std::{
    collections::{HashMap, VecDeque},
    hash::Hash,
    str::FromStr,
};

use anyhow::{Result, anyhow};
use fixedbitset::FixedBitSet;
use rustc_hash::FxHashSet;

#[derive(Debug, Clone, Copy)]
enum Tile {
    Empty,
    Wall,
    Door(u8),
    Key(u8),
}

struct Grid {
    width: usize,
    height: usize,
    tiles: Vec<Tile>,
    num_keys: usize,
    entrance: (usize, usize),
}

impl Grid {
    fn neighbors(
        &self,
        (x, y): (usize, usize),
        grabbed_keys: &FixedBitSet,
    ) -> impl Iterator<Item = ((usize, usize), Tile)> {
        [
            (x > 0).then_some((x.wrapping_sub(1), y)),
            (x < self.width - 1).then_some((x.wrapping_add(1), y)),
            (y > 0).then_some((x, y.wrapping_sub(1))),
            (y < self.height - 1).then_some((x, y.wrapping_add(1))),
        ]
        .into_iter()
        .flatten()
        .map(|(x, y)| ((x, y), self.tiles[y * self.width + x]))
        .filter(|(_, tile)| match tile {
            Tile::Wall => false,
            Tile::Door(k) if !grabbed_keys.contains(*k as usize) => false,
            _ => true,
        })
    }
}

impl FromStr for Grid {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self> {
        let mut tiles = Vec::new();
        let mut num_keys = 0;
        let mut entrance = (0, 0);
        let (mut width, mut height) = (0, 1);
        let (mut x, mut y) = (0, 0);

        for line in s.trim().lines() {
            for c in line.chars() {
                width = width.max(x + 1);
                match c {
                    '#' => tiles.push(Tile::Wall),
                    '.' => tiles.push(Tile::Empty),
                    '@' => {
                        tiles.push(Tile::Empty);
                        entrance = (x, y);
                    }
                    'a'..='z' => {
                        let k = c as u8 - b'a';
                        tiles.push(Tile::Key(k));
                        num_keys += 1;
                    }
                    'A'..='Z' => {
                        let k = c as u8 - b'A';
                        tiles.push(Tile::Door(k));
                    }
                    unk => return Err(anyhow!("Bad input '{unk}'")),
                };

                x += 1;
            }
            y += 1;
            height += 1;
            x = 0;
        }

        Ok(Self {
            width,
            height,
            tiles,
            num_keys,
            entrance,
        })
    }
}

#[derive(Debug, Clone, Eq)]
struct State {
    pos: (usize, usize),
    grabbed_keys: FixedBitSet,
    steps: usize,
}

impl PartialEq for State {
    fn eq(&self, other: &Self) -> bool {
        self.pos == other.pos && self.grabbed_keys == other.grabbed_keys
    }
}

impl Hash for State {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.pos.hash(state);
        self.grabbed_keys.hash(state);
    }
}

impl State {
    fn new(grid: &Grid) -> Self {
        Self {
            pos: grid.entrance,
            grabbed_keys: FixedBitSet::with_capacity(grid.num_keys),
            steps: 0,
        }
    }

    fn step(&self, new_pos: (usize, usize), tile: Tile) -> Self {
        let mut grabbed_keys = self.grabbed_keys.clone();
        match tile {
            Tile::Wall => panic!("Went into wall"),
            Tile::Door(k) if !self.grabbed_keys.contains(k as usize) => {
                panic!("Went through locked door")
            }
            Tile::Key(k) => grabbed_keys.insert(k as usize),
            _ => {}
        };

        Self {
            pos: new_pos,
            grabbed_keys,
            steps: self.steps + 1,
        }
    }
}

fn explore(grid: &Grid) -> Result<usize> {
    let mut visited = FxHashSet::default();
    let mut queue = VecDeque::new();

    let start_state = State::new(grid);
    visited.insert(start_state.clone());
    queue.push_back(start_state);

    while let Some(state) = queue.pop_front() {
        for (new_pos, tile) in grid.neighbors(state.pos, &state.grabbed_keys) {
            let new_state = state.step(new_pos, tile);

            if visited.contains(&new_state) {
                continue;
            }

            if new_state.grabbed_keys.is_full() {
                return Ok(new_state.steps);
            }

            visited.insert(new_state.clone());
            queue.push_back(new_state);
        }
    }

    Err(anyhow!("Couldn't grab all the keys"))
}

pub async fn day18(input: String) -> Result<(String, String)> {
    let grid: Grid = input.parse()?;

    let part1 = explore(&grid)?;
    let part2 = 0;

    Ok((part1.to_string(), part2.to_string()))
}

