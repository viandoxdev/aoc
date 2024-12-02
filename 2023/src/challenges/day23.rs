use anyhow::{anyhow, Context, Result};
use itertools::Itertools;

use Tile::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tile {
    Wall,
    Floor,
    DownSlope,
    RightSlope,
}

pub async fn day23(input: String) -> Result<(String, String)> {
    let tiles: Vec<Tile> = input
        .chars()
        .map(|c| match c {
            '#' => Ok(Wall),
            '.' => Ok(Floor),
            '>' => Ok(RightSlope),
            'v' => Ok(DownSlope),
            '\n' => Ok(Wall),
            _ => Err(anyhow!("Bad character in input")),
        })
        .try_collect()?;

    let width = input.find('\n').context("Bad input")?;
    let height = input.lines().count();
    let start_index = tiles
        .iter()
        .position(|&t| t == Floor)
        .context("Bad input")?;
    let end_index = tiles
        .iter()
        .rposition(|&t| t == Floor)
        .context("Bad input")?;

    // Have to use a fn because I can't specify lifetimes with a closures
    fn neighbors<'a>(
        tiles: &'a [Tile],
        visited: &'a [bool],
        cur: usize,
        width: usize,
    ) -> impl Iterator<Item = usize> + 'a {
        [
            cur.saturating_sub(width + 1),
            cur - 1,
            cur + 1,
            cur + width + 1,
        ]
        .into_iter()
        .filter_map(move |i| {
            let t = tiles.get(i).copied().unwrap_or(Wall);
            let valid = if i < cur {
                // Can't take down and right slopes if they are above or on the left of us
                t == Floor
            } else {
                matches!(t, Floor | DownSlope | RightSlope)
            };
            (valid && !visited[i]).then_some(i)
        })
    }

    let part1 = {
        let mut init_vis = vec![false; width * height + width];
        init_vis[start_index] = true;
        let mut states = vec![(init_vis, start_index, 0)];
        let mut next_states = Vec::new();
        let mut options = Vec::with_capacity(2);
        let mut longest = 0;

        while !states.is_empty() {
            for (mut taken, cur, mut steps) in states.drain(..) {
                options.extend(neighbors(&tiles, &taken, cur, width));

                steps += 1;

                if options.len() > 1 {
                    // Add all the other branches (technically at most one)
                    // This clones
                    for next in options.drain(1..) {
                        // Not a fan of the code duplication, but both chunks are actually quite
                        // different in subtles ways (the continues don't point to the same loop, a
                        // clone becomes a move and next_taken doesn't exist in the second)
                        if next == end_index {
                            longest = longest.max(steps);
                            continue;
                        }
                        let mut next_taken = taken.clone();
                        next_taken[next] = true;
                        next_states.push((next_taken, next, steps))
                    }
                }

                // Then add the first "branch", this moves
                if let Some(next) = options.pop() {
                    // The second chunk being this one
                    if next == end_index {
                        longest = longest.max(steps);
                        continue;
                    }
                    taken[next] = true;
                    next_states.push((taken, next, steps))
                }
            }

            std::mem::swap(&mut states, &mut next_states);
        }

        longest
    };

    let part2 = {
        // Part 2 has us ignore slopes
        let mut tiles = tiles;
        for t in &mut tiles {
            if matches!(*t, RightSlope | DownSlope) {
                *t = Floor;
            }
        }

        // Pre proccess graph to collapse a number of nodes
        let (edges, vertex_count, start_vertex, end_vertex) = {
            const NONE: usize = usize::MAX;

            let mut cur = start_index + width + 1;
            let mut queue = Vec::new();
            let mut visited = vec![false; width * height + width];
            let mut options = Vec::with_capacity(3);
            let mut edges = Vec::new();
            let mut last_vertex = 0;

            let mut vertices_map = vec![NONE; width * height + width];
            let mut edge_len = 1;
            let mut vertex_count = 1;

            let start_vertex = 0;
            let mut end_vertex = NONE;

            visited[cur] = true;
            vertices_map[start_index] = 0;

            'outer: loop {
                options.clear();
                options.extend(neighbors(&tiles, &visited, cur, width));

                if options.is_empty() {
                    // We've reached the end vertex

                    let new_vertex = vertex_count;
                    vertex_count += 1;
                    // Make sure no vertex is marked as visited
                    visited[cur] = false;
                    vertices_map[cur] = new_vertex;

                    edges.push((last_vertex, new_vertex, edge_len));

                    end_vertex = new_vertex;

                    // No obvious continuation, so we'll take from the queue
                } else if options.iter().filter(|&&x| vertices_map[x] == NONE).count() > 1 {
                    // We have more than one unvisited non vertex tile, we're at a never visited branch
                    let new_vertex = vertex_count;
                    vertex_count += 1;

                    // Make sure no vertex is marked as visited
                    visited[cur] = false;
                    vertices_map[cur] = new_vertex;

                    edges.push((last_vertex, new_vertex, edge_len));
                    last_vertex = new_vertex;
                    edge_len = 1;

                    // Take an arbitrary branch
                    let next = options.pop().unwrap();
                    cur = next;
                    visited[cur] = true;
                    // Enqueue the other branches to visit them later
                    queue.extend(options.drain(..).map(|i| (i, new_vertex)));
                    // We can just continue since we choose a branch
                    continue;
                } else {
                    // Handle the case where we have more than one options, but we have only one non
                    // vertex option by removing all the vertex options so we get the normal one on pop
                    // This keeps the case where we have only one option that is a vertex the same
                    if options.len() > 1 {
                        while let Some(&i) = options.last() {
                            if vertices_map[i] != NONE {
                                options.pop();
                            } else {
                                break;
                            }
                        }
                    }

                    if let Some(only) = options.pop() {
                        let to = vertices_map[only];
                        if to != NONE {
                            // We only have one option and it is a vertex, we've reached a vertex from an
                            // outside edge
                            edge_len += 1;
                            edges.push((last_vertex, to, edge_len));
                            // We've reached a node and know no obvious continuation from here, so
                            // we'll take from the queue
                        } else {
                            // We only have one option and it isn't a vertex, so we're still in the middle of
                            // the edge, just extend it
                            edge_len += 1;
                            visited[only] = true;
                            cur = only;
                            // We can just continue on this edge
                            continue;
                        }
                    }
                }

                // We don't know where to continue: take on the queue
                while let Some((i, vertex)) = queue.pop() {
                    if visited[i] {
                        // This location was already visited in since it was enqueued, skip it
                        continue;
                    }

                    // We found an unvisited enqueued location, use it
                    last_vertex = vertex;
                    cur = i;
                    visited[cur] = true;
                    edge_len = 1;
                    continue 'outer;
                }

                // If we got here, that means we couldn't find anywhere to continue even in the queue,
                // so either something went wrong or we're done.
                break;
            }

            (edges, vertex_count, start_vertex, end_vertex)
        };

        let mut edges_from = std::iter::repeat_with(Vec::new)
            .take(vertex_count)
            .collect_vec();
        for &(a, b, w) in &edges {
            edges_from[a].push((b, w));
            edges_from[b].push((a, w));
        }

        let mut queue = Vec::<(usize, usize, u64)>::new();
        let mut max_len = 0;

        queue.push((start_vertex, 0, 1));

        while let Some((cur, len, visited)) = queue.pop() {
            if cur == end_vertex {
                max_len = max_len.max(len);
                continue;
            }

            queue.extend(edges_from[cur].iter().filter_map(|&(to, w)| {
                if visited >> to & 1 > 0 {
                    None
                } else {
                    let mut visited = visited;
                    visited |= 1 << to;
                    Some((to, w + len, visited))
                }
            }));
        }

        max_len
    };

    Ok((part1.to_string(), part2.to_string()))
}
