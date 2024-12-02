use std::collections::{BinaryHeap, HashMap, HashSet, VecDeque};

use anyhow::{anyhow, Result};
use enum_map::{Enum, EnumMap};
use itertools::Itertools;

use num::Num;
use parking_lot::Mutex;
use rustc_hash::FxHashMap;
use smallvec::{smallvec, SmallVec};
use Side::*;
use Tile::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tile {
    Plot,
    Rock,
}

const SIDES: [Side; 4] = [North, South, East, West];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Enum)]
enum Side {
    North,
    South,
    East,
    West,
}

impl Side {
    fn offset<N: Num>(self, (x, y): (N, N)) -> (N, N) {
        match self {
            North => (x, y + N::one()),
            South => (x, y - N::one()),
            East => (x + N::one(), y),
            West => (x - N::one(), y),
        }
    }

    /// What side of a is b on
    fn relative_to((ax, ay): (i32, i32), (bx, by): (i32, i32)) -> Self {
        if bx > ax {
            East
        } else if bx < ax {
            West
        } else if by > bx {
            North
        } else {
            South
        }
    }

    fn opposite(self) -> Self {
        match self {
            North => South,
            South => North,
            East => West,
            West => East,
        }
    }
}

struct GridMap {
    inner: Vec<u64>,
    width: u64,
    height: u64,
}

impl GridMap {
    fn merge(mut self, other: &Self, offset: u64) -> Self {
        for (i, d) in self.inner.iter_mut().enumerate() {
            *d = (other.inner[i].saturating_add(offset)).min(*d);
        }
        self
    }

    fn dijkstra(grid: &Vec<Vec<Tile>>, (sx, sy): (u64, u64)) -> Self {
        #[derive(PartialEq)]
        struct Item {
            dist: u64,
            tile: (u64, u64),
        }

        impl PartialOrd for Item {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(match self.dist.cmp(&other.dist) {
                    std::cmp::Ordering::Less => std::cmp::Ordering::Greater,
                    std::cmp::Ordering::Greater => std::cmp::Ordering::Less,
                    std::cmp::Ordering::Equal => std::cmp::Ordering::Equal,
                })
            }
        }

        impl Ord for Item {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                match self.dist.cmp(&other.dist) {
                    std::cmp::Ordering::Less => std::cmp::Ordering::Greater,
                    std::cmp::Ordering::Greater => std::cmp::Ordering::Less,
                    std::cmp::Ordering::Equal => std::cmp::Ordering::Equal,
                }
            }
        }
        impl Eq for Item {}

        let height = grid.len() as u64;
        let width = grid[0].len() as u64;
        let mut dist = GridMap {
            inner: vec![u64::MAX; (width * height) as usize],
            height,
            width,
        };
        let mut in_queue = vec![true; (width * height) as usize];
        let mut queue = BinaryHeap::<Item>::new();

        queue.push(Item {
            dist: 0,
            tile: (sx, sy),
        });

        dist.set((sx, sy), 0);

        while let Some(Item { tile: (x, y), .. }) = queue.pop() {
            in_queue[(y * width + x) as usize] = false;

            let neighbors = [
                (x > 0).then(|| (x - 1, y)),
                (x < width - 1).then(|| (x + 1, y)),
                (y > 0).then(|| (x, y - 1)),
                (y < height - 1).then(|| (x, y + 1)),
            ]
            .into_iter()
            .flatten()
            .filter(|&(x, y)| {
                grid[y as usize][x as usize] != Tile::Rock && in_queue[(y * width + x) as usize]
            });

            for (nx, ny) in neighbors {
                let alt = dist.get((x, y)).saturating_add(1);
                if alt < dist.get((nx, ny)) {
                    dist.set((nx, ny), alt);
                    queue.push(Item {
                        tile: (nx, ny),
                        dist: alt,
                    })
                }
            }
        }

        dist
    }
    fn set(&mut self, (x, y): (u64, u64), value: u64) {
        self.inner[(y * self.width + x) as usize] = value
    }
    fn get(&self, (x, y): (u64, u64)) -> u64 {
        self.inner[(y * self.width + x) as usize]
    }
}

struct DistanceMap {
    maps: Mutex<HashMap<(u64, u64), GridMap>>,
    grid: Vec<Vec<Tile>>,
    width: u64,
    height: u64,
}

impl DistanceMap {
    fn new(grid: &Vec<Vec<Tile>>) -> Self {
        Self {
            maps: Mutex::new(HashMap::new()),
            grid: grid.clone(),
            width: grid[0].len() as u64,
            height: grid.len() as u64,
        }
    }

    fn get_dist(&self, from: (u64, u64), to: (u64, u64)) -> u64 {
        let mut maps = self.maps.lock();
        if let Some(map) = maps.get(&from) {
            map.get(to)
        } else {
            let map = GridMap::dijkstra(&self.grid, from);
            let res = map.get(to);
            maps.insert(from, map);
            res
        }
    }

    fn get_combined_maps(&self, from: impl IntoIterator<Item = EntryPoint>) -> GridMap {
        let mut maps = self.maps.lock();
        let base_map = GridMap {
            width: self.width,
            height: self.height,
            inner: vec![u64::MAX; (self.width * self.height) as usize],
        };

        from.into_iter().fold(base_map, |acc, ep| {
            let map = maps
                .entry(ep.tile)
                .or_insert_with(|| GridMap::dijkstra(&self.grid, ep.tile));
            acc.merge(map, ep.local_offset)
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct EntryPoint {
    tile: (u64, u64),
    local_offset: u64,
}

#[derive(Debug)]
struct OpenGrid {
    entry_points: SmallVec<[EntryPoint; 4]>,
    ancestors: SmallVec<[Side; 2]>,
    offset: u64,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct GridKey {
    entry_points: SmallVec<[EntryPoint; 4]>,
    ancestors: SmallVec<[Side; 2]>,
}

struct ClosedGrid {
    map: GridMap,
    max_count: (u64, u64),
    max_step: u64,
    exit_points: EnumMap<Side, SmallVec<[EntryPoint; 4]>>,
}

impl Default for OpenGrid {
    fn default() -> Self {
        Self {
            entry_points: smallvec![],
            ancestors: smallvec![],
            offset: 0,
        }
    }
}

impl OpenGrid {
    fn update(&mut self, entry_points: &[EntryPoint], from: Side) {
        if let Some(min_offset) = self
            .entry_points
            .iter()
            .map(|e| e.local_offset + self.offset)
            .chain(entry_points.iter().map(|e| e.local_offset))
            .min()
        {
            for ep in &mut self.entry_points {
                ep.local_offset = ep.local_offset + self.offset - min_offset;
            }

            self.entry_points
                .extend(entry_points.iter().map(|&e| EntryPoint {
                    local_offset: e.local_offset - min_offset,
                    ..e
                }));

            self.offset = min_offset;
        }

        self.ancestors.push(from);
    }

    fn key(&self) -> GridKey {
        GridKey {
            entry_points: self.entry_points.clone(),
            ancestors: self.ancestors.clone(),
        }
    }

    fn close(self, dist: &DistanceMap) -> ClosedGrid {
        let map = dist.get_combined_maps(self.entry_points.iter().copied());
        let (width, height) = (dist.width, dist.height);

        let dist_iter = || {
            (0..width).cartesian_product(0..height).filter_map(|t| {
                let dist = map.get(t);
                (dist != u64::MAX).then_some(dist)
            })
        };

        let max_count_even = dist_iter().filter(|d| d % 2 == 0).count() as u64;
        let max_count_odd = dist_iter().filter(|d| d % 2 == 1).count() as u64;
        let max_step = dist_iter().max().unwrap_or_default();

        let mut exit_points = EnumMap::default();

        for side in SIDES
            .into_iter()
            .filter(|side| !self.ancestors.contains(side))
        {
            let iter: Box<dyn Iterator<Item = (u64, u64)>> = match side {
                North => Box::new((0..width).map(|x| (x, 0))),
                South => Box::new((0..width).map(|x| (x, height - 1))),
                East => Box::new((0..height).map(|y| (width - 1, y))),
                West => Box::new((0..height).map(|y| (0, y))),
            };

            let mut entry_points: SmallVec<[EntryPoint; 4]> = smallvec![];

            const DUMMY: (u64, u64) = (u64::MAX, u64::MAX);

            for (prev, current, next) in std::iter::once(DUMMY)
                .chain(iter)
                .chain(std::iter::once(DUMMY))
                .tuple_windows()
            {
                let pdist = if prev == DUMMY {
                    u64::MAX
                } else {
                    map.get(prev)
                };
                let cdist = map.get(current);
                let sdist = if next == DUMMY {
                    u64::MAX
                } else {
                    map.get(next)
                };

                if pdist >= cdist && cdist <= sdist {
                    let (width, height) = (width as i32, height as i32);
                    let ep = side.offset((current.0 as i32, height - 1 - (current.1 as i32)));
                    let ep = (
                        ((ep.0 + width) % width) as u64,
                        (height - 1 - ((ep.1 + height) % height)) as u64,
                    );

                    entry_points.push(EntryPoint {
                        tile: ep,
                        local_offset: cdist + 1,
                    });
                }
            }

            exit_points[side] = entry_points;
        }

        ClosedGrid {
            map,
            exit_points,
            max_step,
            max_count: (max_count_even, max_count_odd),
        }
    }
}

pub async fn day21(input: String) -> Result<(String, String)> {
    let mut start = (0, 0);
    let grid: Vec<Vec<Tile>> = input
        .lines()
        .enumerate()
        .map(|(y, l)| -> Result<Vec<Tile>> {
            l.chars()
                .enumerate()
                .map(|(x, c)| match c {
                    'S' => {
                        start = (x as u64, y as u64);
                        Ok(Plot)
                    }
                    '.' => Ok(Plot),
                    '#' => Ok(Rock),
                    _ => Err(anyhow!("Malformed input")),
                })
                .try_collect()
        })
        .try_collect()?;

    let width = grid[0].len() as u64;
    let height = grid.len() as u64;

    let dist = DistanceMap::new(&grid);
    let vertice_iter = || (0..width).cartesian_product(0..height);

    let part1 = {
        let steps = 64;

        vertice_iter()
            .filter(|&tile| {
                let d = dist.get_dist(start, tile);
                d <= steps && d % 2 == steps % 2
            })
            .count()
    };

    let part2 = {
        let steps = 100;

        let mut open = VecDeque::from([(0i32, 0i32)]);
        let mut closed = FxHashMap::<GridKey, ClosedGrid>::default();
        let mut open_data = FxHashMap::<(i32, i32), OpenGrid>::default();

        open_data.insert(
            (0, 0),
            OpenGrid {
                offset: 0,
                entry_points: smallvec![EntryPoint {
                    tile: start,
                    local_offset: 0,
                }],
                ancestors: smallvec![],
            },
        );

        let mut count = 0;

        while let Some(grid_coords) = open.pop_front() {
            let (grid, offset) = {
                let open_grid = open_data
                    .remove(&grid_coords)
                    .expect("Open grid has no data");
                let key = open_grid.key();
                let offset = open_grid.offset;

                if open_grid.entry_points.is_empty() {
                    continue;
                }

                if !closed.contains_key(&key) {
                    closed.insert(key.clone(), open_grid.close(&dist));
                }

                (&closed[&key], offset)
            };

            let added = if offset + grid.max_step <= steps {
                if offset % 2 == 0 {
                    grid.max_count.0
                } else {
                    grid.max_count.1
                }
            } else {
                vertice_iter()
                    .filter(|&t| {
                        let dist = grid.map.get(t).saturating_add(offset);
                        dist <= steps && dist % 2 == steps % 2
                    })
                    .count() as u64
            };

            count += added;

            for (side, exit_points) in &grid.exit_points {
                let next_grid = side.offset(grid_coords);
                let entry_points: SmallVec<[EntryPoint; 4]> = exit_points
                    .iter()
                    .filter_map(|&exit| {
                        let local_offset = exit.local_offset + offset;
                        (local_offset <= steps).then_some(EntryPoint {
                            local_offset,
                            ..exit
                        })
                    })
                    .collect();

                if !open_data.contains_key(&next_grid) {
                    open.push_back(next_grid);
                }

                open_data
                    .entry(next_grid)
                    .or_default()
                    .update(&entry_points, side.opposite());
            }
        }
        count
    };

    let part2 = {
        0
    };

    Ok((part1.to_string(), part2.to_string()))
}
