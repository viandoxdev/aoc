use std::str::FromStr;

use anyhow::{anyhow, Result};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct GameSet {
    red: u32,
    green: u32,
    blue: u32,
}

impl GameSet {
    const EMPTY: Self = GameSet {
        red: 0,
        green: 0,
        blue: 0,
    };

    fn new(red: u32, green: u32, blue: u32) -> Self {
        Self { red, green, blue }
    }

    fn fits(&self, other: &Self) -> bool {
        self.red <= other.red && self.green <= other.green && self.blue <= other.blue
    }

    fn max(&self, other: &Self) -> Self {
        Self {
            red: self.red.max(other.red),
            green: self.green.max(other.green),
            blue: self.blue.max(other.blue),
        }
    }
}

impl FromStr for GameSet {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut res = GameSet::EMPTY;
        for field in s.split(", ") {
            let num: u32 = field
                .split_once(" ")
                .ok_or(anyhow!("Malformed game set"))?
                .0 // Get the sub string before the space
                .parse()?;
            // Set values accordingly
            if field.contains("red") {
                res.red = num;
            } else if field.contains("green") {
                res.green = num;
            } else if field.contains("blue") {
                res.blue = num;
            }
        }
        Ok(res)
    }
}

#[derive(Debug)]
struct Game {
    id: u32,
    sets: Vec<GameSet>,
}

impl Game {
    /// Minimum set such that all sets fit into it
    fn minimum_set(&self) -> Option<GameSet> {
        self.sets.iter().copied().reduce(|a, b| a.max(&b))
    }

    fn power(&self) -> u32 {
        self.minimum_set()
            .map(|s| s.red * s.green * s.blue)
            .unwrap_or_default()
    }
}

impl FromStr for Game {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let (id_str, sets_str) = s.split_once(": ").ok_or(anyhow!("Malformed game"))?;

        // Parse each sets
        let sets = sets_str
            .split("; ")
            .map(GameSet::from_str)
            // See https://doc.rust-lang.org/std/result/index.html#collecting-into-result
            .collect::<Result<Vec<GameSet>>>()?;
        // Parse the id, skipping over "Game "
        let id: u32 = id_str[5..].parse()?;
        Ok(Self { sets, id })
    }
}

pub async fn day2(input: String) -> Result<(String, String)> {
    // Parse games
    let games = input
        .lines()
        .map(Game::from_str)
        .collect::<Result<Vec<Game>>>()?;

    let part1_max = GameSet::new(12, 13, 14);
    // Sum of all the ids of the games for which all sets fit into part1_max
    let part1 = games
        .iter()
        // filter_map is equivalent to flat_map when the item is Option<T>, but can be very
        // slightly faster because the size of the iterator can be better known
        .filter_map(|g| g.sets.iter().all(|s| s.fits(&part1_max)).then_some(g.id))
        .sum::<u32>();
    // Sum of the power of each game
    let part2 = games.iter().map(Game::power).sum::<u32>();

    Ok((part1.to_string(), part2.to_string()))
}
