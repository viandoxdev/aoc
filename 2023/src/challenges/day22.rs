use std::str::FromStr;

use anyhow::{Context, Error, Result};
use itertools::Itertools;

/// Arbitrary vec3 (because I don't know yet what type I'll need)
struct Vec3<T> {
    x: T,
    y: T,
    z: T,
}

impl<T: FromStr> FromStr for Vec3<T> {
    type Err = Error;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let (x, y, z) = s
            .split(',')
            .flat_map(T::from_str)
            .collect_tuple()
            .context("Bad Vec3")?;
        Ok(Self { x, y, z })
    }
}

impl<T: Clone> Clone for Vec3<T> {
    fn clone(&self) -> Self {
        Self {
            x: self.x.clone(),
            y: self.y.clone(),
            z: self.y.clone(),
        }
    }
}

impl<T: Copy> Copy for Vec3<T> {}

pub async fn day22(input: String) -> Result<(String, String)> {
    // Parse the bricks
    let mut bricks: Vec<(Vec3<usize>, Vec3<usize>)> = input
        .lines()
        .map(|l| -> Result<(Vec3<usize>, Vec3<usize>)> {
            let (a, b) = l.split_once('~').context("Bad input")?;
            Ok((a.parse()?, b.parse()?))
        })
        .try_collect()?;

    // For some weird reason the bricks aren't sorted by z by default, that is a later brick can
    // support an earlier one in the list, this fixes that since my code needs them in order
    bricks.sort_by_key(|(a, _)| a.z);

    // Compute maxes for the grid size
    let max_x = bricks
        .iter()
        .map(|(_, Vec3 { x, .. })| *x)
        .max()
        .unwrap_or_default();
    let max_y = bricks
        .iter()
        .map(|(_, Vec3 { y, .. })| *y)
        .max()
        .unwrap_or_default();

    // The grid, a vertical view of the stack, just has the highest point of each (x, y) and the
    // corresponding brick it belongs to (for dependency analysis)
    let mut grid = vec![vec![(0, usize::MAX); max_x + 1]; max_y + 1];
    // What brick each brick depends on
    let mut dependencies = vec![vec![]; bricks.len()];

    // Figure out dependencies
    for (i, &(a, b)) in bricks.iter().enumerate() {
        // All the (x, y) in the brick
        let affected = (a.x..=b.x).flat_map(|x| (a.y..=b.y).map(move |y| (x, y)));
        // Get the z at which the brick will settle at
        let z = affected
            .clone()
            .map(|(x, y)| grid[y][x].0)
            .max()
            .unwrap_or_default();
        // Update dependencies
        dependencies[i].extend(
            // Find all the bricks this one is resting onto
            affected
                .clone()
                .filter_map(|(x, y)| {
                    // Keep the bricks whose z is the same as the highest we found
                    // (The ones directly touching the current brick)
                    (grid[y][x].0 == z && grid[y][x].1 != usize::MAX).then_some(grid[y][x].1)
                })
                // Kinda slow, but fast enough and leads to problems otherwise
                .unique(),
        );
        let height = b.z - a.z + 1;
        for (x, y) in affected {
            grid[y][x] = (z + height, i);
        }
    }

    // What bricks depends on each brick (the opposite relation as dependencies)
    let mut supports = vec![vec![]; bricks.len()];
    for (i, deps) in dependencies.iter().enumerate() {
        for &dep in deps {
            supports[dep].push(i)
        }
    }

    let part1 = supports
        .iter()
        // Count all the brick such that the bricks they support all are supported by more than one
        // brick (they are safely disintegratable)
        .filter(|s| s.iter().all(|&b| dependencies[b].len() > 1))
        .count();

    let part2 = {
        let mut count = 0;
        // Set of which bricks are disintegrated
        let mut disintegrated = vec![false; bricks.len()];
        let mut to_disintegrate = Vec::new();
        let mut next = Vec::new();
        for brick in 0..bricks.len() {
            disintegrated.fill(false);
            to_disintegrate.push(brick);

            while !to_disintegrate.is_empty() {
                for b in to_disintegrate.drain(..) {
                    count += 1;
                    disintegrated[b] = true;
                    next.extend(
                        // Get all the bricks depending on b, lets call these x
                        supports[b]
                            .iter()
                            // Keep only the bricks whose dependencies (the one they depend on) all
                            // have been disintegrated
                            .filter(|&&x| dependencies[x].iter().all(|&x| disintegrated[x])),
                    )
                }

                std::mem::swap(&mut to_disintegrate, &mut next);
            }
            // We want to count the number of bricks that would fall, excluding the one we disintegrated,
            // so we need to remove one
            count -= 1;
        }
        count
    };

    Ok((part1.to_string(), part2.to_string()))
}
