use std::{error::Error, str::FromStr};

use anyhow::Context;
use reqwest::header::COOKIE;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Vec2 {
    x: i32,
    y: i32
}

impl Vec2 {
    pub fn new(x: i32, y: i32) -> Self {
        Self {x, y}
    }
}

impl FromStr for Vec2 {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut iter = s.split(",").map(|s| s.parse()).flatten();
        Ok(Self::new(iter.next().context("missing x")?, iter.next().context("missing y")?))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Tile {
    Empty,
    Sand,
    Wall,
}

struct State<const W: usize, const H: usize> {
    grid: [[Tile; W]; H]
}

impl<const W: usize, const H: usize> State<W, H> {
    pub fn new() -> Self {
        Self {
            grid: [[Tile::Empty; W]; H]
        }
    }

    pub fn line(&mut self, from: Vec2, to: Vec2) {
        let xmin = from.x.min(to.x);
        let ymin = from.y.min(to.y);
        let xmax = from.x.max(to.x);
        let ymax = from.y.max(to.y);

        for x in xmin..=xmax {
            for y in ymin..=ymax {
                self.grid[y as usize][x as usize] = Tile::Wall;
            }
        }
    }

    pub fn parse(input: String) -> Self {
        let mut base = Self::new();
        let mut max_y = 0;
        for line in input.split_terminator("\n") {
            let v: Vec<Vec2> = line.split(" -> ").map(|vec| vec.parse().unwrap()).collect::<Vec<_>>();
            for v in v.windows(2) {
                base.line(v[0], v[1]);
                max_y = max_y.max(v[0].y).max(v[1].y);
            }
        }

        base.line(Vec2::new(0, max_y + 2), Vec2::new(W as i32 - 1, max_y + 2));

        base
    }

    fn open(&self, x: i32, y: i32) -> bool {
        self.grid[y as usize][x as usize] == Tile::Empty
    }

    pub fn run(&mut self) -> usize {
        let mut count = 0;
        let mut last_rest = Vec2::new(0, 0);
        while last_rest != Vec2::new(500, 0) {
            let mut cur = Vec2::new(500, 0);
            loop {
                if self.open(cur.x, cur.y + 1) {
                    cur.y += 1;
                } else if self.open(cur.x - 1, cur.y + 1) {
                    cur.x -= 1;
                    cur.y += 1;
                } else if self.open(cur.x + 1, cur.y + 1) {
                    cur.x += 1;
                    cur.y += 1;
                } else {
                    last_rest = cur;
                    self.grid[cur.y as usize][cur.x as usize] = Tile::Sand;
                    count += 1;
                    break;
                }
            }
        }

        count
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut session = std::fs::read_to_string("../../session")?;
    session.pop(); // remove new line

    let client = reqwest::blocking::Client::new();
    let resp = client.get("https://adventofcode.com/2022/day/14/input")
        .header(COOKIE, format!("session={session}"))
        .send()?.text()?;

    let res = State::<2048, 256>::parse(resp).run();
    println!("res: {res}");

    Ok(())
}
