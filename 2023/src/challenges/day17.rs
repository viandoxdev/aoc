use std::collections::BinaryHeap;

use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

use anyhow::Result;
use itertools::Itertools;

use Direction::*;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
enum Direction {
    North,
    West,
    South,
    East,
    NoDirection,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct OpenNode {
    x: u8,
    y: u8,
    dir: Direction,
    score: u32,
}
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
struct NodeKey(u8, u8, Direction);
impl OpenNode {
    fn key(&self) -> NodeKey {
        NodeKey(self.x, self.y, self.dir)
    }
    fn new(x: u8, y: u8, dir: Direction, score: u32) -> Self {
        Self { x, y, dir, score }
    }
}
impl PartialOrd for OpenNode {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(other.score.cmp(&self.score))
    }
}
impl Ord for OpenNode {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.score.cmp(&self.score)
    }
}

pub async fn day17(input: String) -> Result<(String, String)> {
    let tiles = input
        .chars()
        .filter_map(|c| c.to_digit(10))
        .map(|x| x as u8)
        .collect_vec();
    let width = input.find('\n').unwrap() as u8;
    let height = input.lines().count() as u8;

    let tile = |x: u8, y: u8| tiles[x as usize + y as usize * width as usize] as u32;
    let heuristics = |x: u8, y: u8| {
        (x as u32).abs_diff(width as u32 - 1) + (y as u32).abs_diff(height as u32 - 1)
    };

    let solve = |min, max| {
        let mut open = BinaryHeap::<OpenNode>::new();
        let mut open_set = HashSet::<NodeKey>::default();
        let mut gscore = HashMap::<NodeKey, u32>::default();

        let start = OpenNode::new(0, 0, NoDirection, heuristics(0, 0));

        gscore.insert(start.key(), 0);
        open.push(start);
        open_set.insert(start.key());

        let mut neighbours = Vec::with_capacity(6);

        while let Some(cur) = open.pop() {
            let OpenNode { x, y, dir, .. } = cur;
            let key = cur.key();
            let score = gscore[&key];

            open_set.remove(&key);

            if x == width - 1 && y == height - 1 {
                return score;
            }

            if !matches!(dir, South | North) && y >= min {
                let min_cost = (1..min).map(|dy| tile(x, y - dy)).sum::<u32>();
                (min..=y.min(max)).map(|dy| NodeKey(x, y - dy, North)).fold(
                    min_cost,
                    |mut c, n @ NodeKey(x, y, _)| {
                        c += tile(x, y);
                        neighbours.push((n, c));
                        c
                    },
                );
            }
            if !matches!(dir, East | West) && x >= min {
                let min_cost = (1..min).map(|dx| tile(x - dx, y)).sum::<u32>();
                (min..=x.min(max)).map(|dx| NodeKey(x - dx, y, West)).fold(
                    min_cost,
                    |mut c, n @ NodeKey(x, y, _)| {
                        c += tile(x, y);
                        neighbours.push((n, c));
                        c
                    },
                );
            }
            if !matches!(dir, North | South) && y < height - min {
                let min_cost = (1..min).map(|dy| tile(x, y + dy)).sum::<u32>();
                (min..=(height - 1 - y).min(max))
                    .map(|dy| NodeKey(x, y + dy, South))
                    .fold(min_cost, |mut c, n @ NodeKey(x, y, _)| {
                        c += tile(x, y);
                        neighbours.push((n, c));
                        c
                    });
            }
            if !matches!(dir, East | West) && x < width - min {
                let min_cost = (1..min).map(|dx| tile(x + dx, y)).sum::<u32>();
                (min..=(width - 1 - x).min(max))
                    .map(|dx| NodeKey(x + dx, y, East))
                    .fold(min_cost, |mut c, n @ NodeKey(x, y, _)| {
                        c += tile(x, y);
                        neighbours.push((n, c));
                        c
                    });
            }

            for (nkey @ NodeKey(nx, ny, dir), cost) in neighbours.drain(..) {
                let new_gscore = score + cost;
                let entry = gscore.entry(nkey);
                let cur_score = entry.or_insert(u32::MAX);
                if new_gscore < *cur_score {
                    *cur_score = new_gscore;
                    if !open_set.contains(&nkey) {
                        open.push(OpenNode {
                            x: nx,
                            y: ny,
                            dir,
                            score: new_gscore + heuristics(nx, ny),
                        });
                        open_set.insert(nkey);
                    }
                }
            }
        }

        unreachable!()
    };

    let part1 = solve(1, 3);
    let part2 = solve(4, 10);

    Ok((part1.to_string(), part2.to_string()))
}
