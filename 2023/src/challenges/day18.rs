use std::hash::Hash;
use std::ops::{Add, Mul};

use rustc_hash::FxHashSet as HashSet;

use anyhow::Result;

struct Vec2<T> {
    x: T,
    y: T,
}

impl<T: Add> Add for Vec2<T> {
    type Output = Vec2<<T as Add>::Output>;
    fn add(self, rhs: Self) -> Self::Output {
        Vec2 {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl<T: Mul + Copy> Mul<T> for Vec2<T> {
    type Output = Vec2<<T as Mul<T>>::Output>;
    fn mul(self, rhs: T) -> Self::Output {
        Vec2 {
            x: self.x * rhs,
            y: self.y * rhs,
        }
    }
}

impl<T: Clone> Clone for Vec2<T> {
    fn clone(&self) -> Self {
        Self {
            x: self.x.clone(),
            y: self.y.clone(),
        }
    }
}

impl<T: Copy> Copy for Vec2<T> {}

impl<T: Hash> Hash for Vec2<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.x.hash(state);
        self.y.hash(state);
    }
}

impl<T: PartialEq> PartialEq for Vec2<T> {
    fn eq(&self, other: &Self) -> bool {
        self.x == other.x && self.y == other.y
    }
}

impl<T: Eq> Eq for Vec2<T> {}

fn vec2<T>(x: T, y: T) -> Vec2<T> {
    Vec2 { x, y }
}

use itertools::Itertools;
use Tile::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tile {
    Edge,
    Empty,
}

pub async fn day18(input: String) -> Result<(String, String)> {
    let edges = input.lines().fold(vec![vec2(0, 0)], |mut points, l| {
        let (num, _) = l[2..].split_once(' ').unwrap();
        let num: i32 = num.parse().unwrap();
        let dir = match &l[0..1] {
            "U" => vec2(0, -1),
            "L" => vec2(-1, 0),
            "D" => vec2(0, 1),
            "R" => vec2(1, 0),
            _ => unreachable!(),
        };

        let &last = points.last().unwrap();
        points.extend((1..=num).map(|n| last + dir * n));
        points
    });

    let (grid, width, height) = {
        let (x0, y0, x1, y1) = edges.iter().copied().fold(
            (i32::MAX, i32::MAX, i32::MIN, i32::MIN),
            |(minx, miny, maxx, maxy), Vec2 { x, y }| {
                (minx.min(x), miny.min(y), maxx.max(x), maxy.max(y))
            },
        );
        let width = (x1 - x0) as usize + 3;
        let height = (y1 - y0) as usize + 3;

        let mut grid = std::iter::repeat_with(|| vec![Empty; width])
            .take(height)
            .collect_vec();
        for Vec2 { x, y } in edges {
            grid[(y - y0 + 1) as usize][(x - x0 + 1) as usize] = Edge;
        }

        let mut str = String::new();

        for row in &grid {
            for &tile in row {
                match tile {
                    Edge => str.push_str("[]"),
                    Empty => str.push_str(" ."),
                }
            }
            str.push_str("\n");
        }

        println!("state: \n{str}");

        (grid, width, height)
    };

    let part1 = {
        let mut open = HashSet::<Vec2<usize>>::default();
        let mut next_open = HashSet::<Vec2<usize>>::default();
        let mut closed = HashSet::<Vec2<usize>>::default();

        open.insert(vec2(0, 0));

        while !open.is_empty() {
            for p @ Vec2 { x, y } in open.drain() {
                closed.insert(p);

                let nexts = [
                    (y > 0).then(|| vec2(x, y - 1)),
                    (x > 0).then(|| vec2(x - 1, y)),
                    (y < height - 1).then(|| vec2(x, y + 1)),
                    (x < width - 1).then(|| vec2(x + 1, y)),
                ]
                .into_iter()
                .flatten()
                .filter(|p @ Vec2 { x, y }| !closed.contains(p) && grid[*y][*x] != Edge);

                next_open.extend(nexts);
            }

            (next_open, open) = (open, next_open)
        }

        let undigged_count = closed.len();
        println!("{undigged_count}");

        (width * height) - undigged_count
    };
    let part2 = 0;

    Ok((part1.to_string(), part2.to_string()))
}
