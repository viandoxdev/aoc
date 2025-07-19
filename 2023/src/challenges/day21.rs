use std::{
    cmp::Ordering,
    collections::{BinaryHeap, HashMap},
};

use anyhow::{anyhow, Result};
use enum_map::Enum;

/// Size of the garden
const SIZE: usize = 131;
//const SIZE: usize = 3;
/// Maximum number of steps to explore every plot in a garden
const MAX_STEPS: usize = 512;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tile {
    Plot,
    Rock,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Enum)]
enum Side {
    North = 0,
    South = 1,
    East = 2,
    West = 3,
}

#[derive(Clone, Copy, Debug)]
struct EntryPoint {
    /// How many plots can be reached with a given number of steps starting from this entry
    plots: [u32; MAX_STEPS],
    even_plot_count: u32,
    odd_plot_count: u32,
}

impl Default for EntryPoint {
    fn default() -> Self {
        Self {
            plots: [0; MAX_STEPS],
            even_plot_count: 0,
            odd_plot_count: 0,
        }
    }
}

impl EntryPoint {
    fn new(grid: &[[Tile; SIZE]; SIZE], (ex, ey): (u32, u32)) -> Self {
        struct Entry {
            dist: u32,
            x: u32,
            y: u32,
        }

        impl PartialEq for Entry {
            fn eq(&self, other: &Self) -> bool {
                self.dist == other.dist
            }
        }

        impl Eq for Entry {}

        impl PartialOrd for Entry {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                Some(self.cmp(other))
            }
        }

        impl Ord for Entry {
            fn cmp(&self, other: &Self) -> Ordering {
                other.dist.cmp(&self.dist)
            }
        }

        let mut dist = [[u32::MAX; SIZE]; SIZE];
        let mut visited = [[false; SIZE]; SIZE];
        let mut queue = BinaryHeap::new();

        dist[ey as usize][ex as usize] = 0;
        visited[ey as usize][ex as usize] = true;
        queue.push(Entry {
            dist: 0,
            x: ex,
            y: ey,
        });

        while let Some(tile) = queue.pop() {
            // Dijkstra
            for (dx, dy) in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
                let (nx, ny) = (tile.x as i32 + dx, tile.y as i32 + dy);
                if nx < SIZE as i32
                    && ny < SIZE as i32
                    && nx >= 0
                    && ny >= 0
                    && grid[ny as usize][nx as usize] != Tile::Rock
                    && !visited[ny as usize][nx as usize]
                {
                    let new_dist = tile.dist + 1;
                    if new_dist < dist[ny as usize][nx as usize] {
                        dist[ny as usize][nx as usize] = new_dist;
                        visited[ny as usize][nx as usize] = true;
                        queue.push(Entry {
                            dist: new_dist,
                            x: nx as u32,
                            y: ny as u32,
                        });
                    }
                }
            }
        }

        let mut plots = [0u32; MAX_STEPS];
        let mut even_plot_count = 0;
        let mut odd_plot_count = 0;

        for row in &dist {
            for &dist in row {
                if dist == u32::MAX {
                    continue;
                }

                if dist % 2 == 0 {
                    even_plot_count += 1;
                } else {
                    odd_plot_count += 1;
                }

                for d in ((dist as usize)..MAX_STEPS).step_by(2) {
                    plots[d] += 1;
                }
            }
        }

        Self {
            plots,
            even_plot_count,
            odd_plot_count,
        }
    }
}

struct Grid {
    start: (u32, u32),
    tiles: [[Tile; SIZE]; SIZE],
    entry_points: HashMap<(u32, u32), EntryPoint>,
}

impl Grid {
    fn new(input: &str) -> Result<Self> {
        let mut tiles = [[Tile::Rock; SIZE]; SIZE];
        let mut start = (0, 0);
        for (y, line) in input.lines().enumerate() {
            for (x, c) in line.chars().enumerate() {
                tiles[y][x] = match c {
                    'S' => {
                        start = (x as u32, y as u32);
                        Tile::Plot
                    }
                    '.' => Tile::Plot,
                    '#' => Tile::Rock,
                    _ => Err(anyhow!("Malformed input"))?,
                };
            }
        }

        Ok(Self {
            start,
            tiles,
            entry_points: HashMap::new(),
        })
    }

    fn entry_point(&mut self, (x, y): (u32, u32)) -> &mut EntryPoint {
        self.entry_points
            .entry((x, y))
            .or_insert_with(|| EntryPoint::new(&self.tiles, (x, y)))
    }
}

pub async fn day21(input: String) -> Result<(String, String)> {
    let mut grid = Grid::new(&input)?;

    assert!(grid.tiles.len() == SIZE);
    assert!(grid.tiles[0].len() == SIZE);

    let part1 = grid.entry_point(grid.start).plots[64];
    let part2 = {
        let step_count = 26_501_365u32;

        // Minimum manhattan distance where all gardens are trivially explored
        let mut p = ((step_count + 1) / SIZE as u32 - 1) as u64;
        // Number of gardens at a manhattan distance less than or equal to p
        let full_garden_count = 1 + 2 * p * (p + 1);
        // Number of gardens at an odd manhattan distance less than or equal to p
        let odd_garden_count = (p + p % 2) * (p + p % 2);
        // Number of gardens at an even manhattan distance less than or equal to p
        let even_garden_count = full_garden_count - odd_garden_count;
        let even_plot_count = grid.entry_point(grid.start).even_plot_count as u64;
        let odd_plot_count = grid.entry_point(grid.start).odd_plot_count as u64;
        let mut total = if step_count % 2 == 0 {
            odd_garden_count * odd_plot_count + even_garden_count * even_plot_count
        } else {
            odd_garden_count * even_plot_count + even_garden_count * odd_plot_count
        };

        let mut plots_added = true;
        while plots_added {
            p += 1;
            plots_added = false;

            let edges = [
                ((SIZE as u32 / 2, 0), 1, 0),
                ((SIZE as u32 / 2, SIZE as u32 - 1), 1, 0),
                ((0, SIZE as u32 / 2), 1, 0),
                ((SIZE as u32 - 1, SIZE as u32 / 2), 1, 0),
                ((0, 0), p - 1, SIZE / 2),
                ((SIZE as u32 - 1, 0), p - 1, SIZE / 2),
                ((0, SIZE as u32 - 1), p - 1, SIZE / 2),
                ((SIZE as u32 - 1, SIZE as u32 - 1), p - 1, SIZE / 2),
            ];

            for &(entry_point, count, adjust) in &edges {
                let dist_to_edge =
                    SIZE as u32 / 2 + 1 + (p as u32 - 1) * SIZE as u32 - adjust as u32;
                if dist_to_edge > step_count {
                    continue;
                }

                let steps_left = step_count - dist_to_edge;
                let plots = grid.entry_point(entry_point).plots[steps_left as usize] as u64;

                plots_added = true;
                total += count * plots;
            }
        }

        total
    };

    Ok((part1.to_string(), part2.to_string()))
}
