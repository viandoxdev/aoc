use std::{
    collections::{BinaryHeap, VecDeque},
    hash::Hash,
    ops::{Bound, RangeBounds},
    str::FromStr,
};

use anyhow::{Result, anyhow};
use itertools::Itertools;
use rustc_hash::FxHashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct BitSet {
    inner: u32,
}

impl BitSet {
    fn from_range(range: impl RangeBounds<usize>) -> Self {
        fn to_included(b: Bound<&usize>, start: bool) -> usize {
            match (start, b) {
                (_, Bound::Included(&b)) => b,
                (true, Bound::Excluded(&b)) => b + 1,
                (false, Bound::Excluded(&b)) => b - 1,
                (true, Bound::Unbounded) => 0,
                (false, Bound::Unbounded) => 32,
            }
        }
        let (start, end) = (
            to_included(range.start_bound(), true),
            to_included(range.end_bound(), false),
        );
        let len = end - start + 1;
        Self {
            inner: (1u32 << len).wrapping_sub(1) >> start,
        }
    }
    #[inline(always)]
    fn intersection(&self, other: &BitSet) -> Self {
        Self {
            inner: self.inner & other.inner,
        }
    }
    #[inline(always)]
    fn difference(&self, other: &BitSet) -> Self {
        Self {
            inner: self.inner & (!other.inner)
        }
    }
    #[inline(always)]
    fn empty() -> Self {
        Self { inner: 0 }
    }
    #[inline(always)]
    fn contains(&self, key: usize) -> bool {
        self.inner & (1 << key) > 0
    }
    #[inline(always)]
    fn insert(&mut self, key: usize) {
        self.inner |= 1 << key;
    }
    #[inline(always)]
    fn contains_set(&self, other: &BitSet) -> bool {
        self.inner & other.inner == other.inner
    }
    #[inline(always)]
    fn concat(&mut self, other: BitSet) {
        self.inner |= other.inner
    }
}

impl FromIterator<usize> for BitSet {
    fn from_iter<T: IntoIterator<Item = usize>>(iter: T) -> Self {
        let mut res = Self::empty();
        for i in iter {
            res.insert(i);
        }
        res
    }
}

impl IntoIterator for BitSet {
    type Item = usize;
    type IntoIter = OnesIterator;

    fn into_iter(self) -> Self::IntoIter {
        OnesIterator { set: self.inner }
    }
}

struct OnesIterator {
    set: u32,
}

impl Iterator for OnesIterator {
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        let res = self.set.trailing_zeros();
        self.set ^= 1 << res;
        (res != 32).then_some(res as usize)
    }
}

#[derive(Debug, Clone, Copy)]
enum Tile {
    Empty,
    Wall,
    Door(u8),
    Key(u8),
}

#[derive(Clone)]
struct Grid {
    width: usize,
    height: usize,
    tiles: Vec<Tile>,
    num_keys: usize,
    entrance: (usize, usize),
}

impl Grid {
    fn neighbors(&self, (x, y): (usize, usize)) -> impl Iterator<Item = ((usize, usize), Tile)> {
        [
            (x > 0).then_some((x.wrapping_sub(1), y)),
            (x < self.width - 1).then_some((x.wrapping_add(1), y)),
            (y > 0).then_some((x, y.wrapping_sub(1))),
            (y < self.height - 1).then_some((x, y.wrapping_add(1))),
        ]
        .into_iter()
        .flatten()
        .map(|(x, y)| ((x, y), self.tiles[y * self.width + x]))
        .filter(move |(_, tile)| !matches!(tile, Tile::Wall))
    }

    fn transform(&self) -> Self {
        let mut new = self.clone();
        let (x, y) = new.entrance;
        new.tiles[x + y * new.width] = Tile::Wall;
        new.tiles[(x + 1) + y * new.width] = Tile::Wall;
        new.tiles[(x - 1) + y * new.width] = Tile::Wall;
        new.tiles[x + (y - 1) * new.width] = Tile::Wall;
        new.tiles[x + (y + 1) * new.width] = Tile::Wall;
        new
    }

    fn split_entrances(&self) -> [(usize, usize); 4] {
        let (x, y) = self.entrance;
        [
            (x + 1, y + 1),
            (x - 1, y + 1),
            (x + 1, y - 1),
            (x - 1, y - 1),
        ]
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

#[derive(Debug, Clone, Copy, Eq)]
struct State<const R: usize> {
    poses: [usize; R],
    keys: BitSet,
    dist: usize,
}

impl<const R: usize> PartialEq for State<R> {
    fn eq(&self, other: &Self) -> bool {
        self.poses == other.poses && self.keys == other.keys
    }
}

impl<const R: usize> Hash for State<R> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.poses.hash(state);
        self.keys.hash(state);
    }
}

impl<const R: usize> Ord for State<R> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Flip the order because a shorter distance is better than a longer one
        other.dist.cmp(&self.dist)
    }
}

impl<const R: usize> PartialOrd for State<R> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

struct KeyGraph<const R: usize> {
    robot_keys: [BitSet; R],
    distances: Vec<Vec<usize>>,
    dependencies: Vec<BitSet>,
    count: usize,
}

impl<const R: usize> KeyGraph<R> {
    fn new(grid: &Grid, robots: [(usize, usize); R]) -> Self {
        // Index grid.num_keys is the distance from the robot's entrances.
        let mut keys_distances = vec![vec![usize::MAX; grid.num_keys + 1]; grid.num_keys + 1];

        let mut keys_robot = vec![0; grid.num_keys];
        let mut keys_locations = vec![(0, 0); grid.num_keys];
        let mut gate_requirements = vec![None; grid.num_keys];
        let mut keys_requirements = vec![None; grid.num_keys];

        let mut queue = VecDeque::new();
        let mut visited = vec![false; grid.width * grid.height];

        // Scan map from each entrances for key to entrances distances and what door needs to be
        // unlocked to go where
        for (robot, entrance) in robots.into_iter().enumerate() {
            queue.push_back((entrance, None, 0));
            visited[entrance.0 + entrance.1 * grid.width] = true;

            while let Some((tile, req, dist)) = queue.pop_front() {
                for ((x, y), tile) in grid.neighbors(tile) {
                    if visited[x + grid.width * y] {
                        continue;
                    }

                    if let Tile::Key(k) = tile {
                        keys_requirements[k as usize] = req;
                        keys_locations[k as usize] = (x, y);
                        keys_robot[k as usize] = robot;
                        keys_distances[k as usize][k as usize] = 0;
                        keys_distances[k as usize][grid.num_keys] = dist + 1;
                        keys_distances[grid.num_keys][k as usize] = dist + 1;
                    }

                    let next_req = if let Tile::Door(k) = tile {
                        gate_requirements[k as usize] = req;
                        Some(k)
                    } else {
                        req
                    };
                    queue.push_back(((x, y), next_req, dist + 1));
                    visited[x + grid.width * y] = true;
                }
            }
        }

        // Do a few more bfs to firgure out the distances between each keys
        let mut queue = VecDeque::new();
        for k in 0..grid.num_keys {
            visited.fill(false);
            let (kx, ky) = keys_locations[k];
            queue.push_back(((kx, ky), 0));
            visited[kx + ky * grid.height] = true;

            while let Some((tile, d)) = queue.pop_front() {
                for ((x, y), tile) in grid.neighbors(tile) {
                    if visited[x + y * grid.height] {
                        continue;
                    }

                    visited[x + y * grid.height] = true;
                    queue.push_back(((x, y), d + 1));

                    if let Tile::Key(n) = tile {
                        keys_distances[k][n as usize] = d + 1;
                    }
                }
            }
        }

        // Resolve key dependencies : remove gates altogether by expanding the keys_requirements into a
        // list of keys that all need to be collected before.
        let mut key_dependencies = vec![None; grid.num_keys];

        // Ugly recursive auxiliary function
        fn get_key_dependencies(
            memo: &mut [Option<BitSet>],
            key: u8,
            kreqs: &[Option<u8>],
            greqs: &[Option<u8>],
            cap: usize,
        ) -> BitSet {
            if let Some(set) = memo[key as usize].as_ref() {
                return set.clone();
            }

            let mut dependencies = BitSet::empty();
            let mut gate = kreqs[key as usize];
            while let Some(g) = gate {
                dependencies.insert(g as usize);
                dependencies.concat(get_key_dependencies(memo, g, kreqs, greqs, cap));
                gate = greqs[g as usize];
            }

            memo[key as usize] = Some(dependencies.clone());
            dependencies
        }

        for k in 0..grid.num_keys {
            let _ = get_key_dependencies(
                &mut key_dependencies,
                k as u8,
                &keys_requirements,
                &gate_requirements,
                grid.num_keys,
            );
        }

        let dependencies = key_dependencies
            .into_iter()
            .map(|o| o.unwrap_or(BitSet::empty()))
            .collect_vec();

        let mut robot_keys = [BitSet::empty(); R];

        for k in 0..grid.num_keys {
            robot_keys[keys_robot[k]].insert(k);
        }

        Self {
            distances: keys_distances,
            count: grid.num_keys,
            dependencies,
            robot_keys,
        }
    }

    fn explore(&self) -> usize {
        // Dijkstra on the simplified graph
        let mut queue = BinaryHeap::new();
        let mut dist = FxHashMap::default();
        let start = State {
            dist: 0,
            poses: [self.count; R],
            keys: BitSet::empty(),
        };
        dist.insert(start, 0);
        queue.push(start);

        let goal = BitSet::from_range(0..self.count);

        while let Some(state) = queue.pop() {
            if state.keys.contains_set(&goal) {
                return state.dist;
            }

            if state.dist > dist[&state] {
                continue;
            }

            for robot in 0..R {
                for n in self.robot_keys[robot].difference(&state.keys).into_iter().filter(|&k| {
                    state.keys.contains_set(&self.dependencies[k])
                }) {
                    let weight = self.distances[state.poses[robot]][n];
                    let n_state = {
                        let mut poses = state.poses;
                        poses[robot] = n;
                        let mut keys = state.keys;
                        if n != self.count {
                            keys.insert(n);
                        }

                        State {
                            keys,
                            poses,
                            dist: dist[&state] + weight,
                        }
                    };

                    if n_state.dist < dist.get(&n_state).copied().unwrap_or(usize::MAX) {
                        queue.push(n_state);
                        dist.insert(n_state, n_state.dist);
                    }
                }
            }
        }

        panic!("Couldn't find shortest path");
    }
}

pub async fn day18(input: String) -> Result<(String, String)> {
    let grid: Grid = input.parse()?;
    let split_grid = grid.transform();

    let (graph, split_graph) = (
        KeyGraph::new(&grid, [grid.entrance]),
        KeyGraph::new(&split_grid, split_grid.split_entrances()),
    );

    let part1 = graph.explore();
    let part2 = split_graph.explore();

    Ok((part1.to_string(), part2.to_string()))
}
