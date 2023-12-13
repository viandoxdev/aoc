use std::{collections::HashMap, str::FromStr};

use anyhow::{anyhow, Error, Result};
use itertools::{FoldWhile::*, Itertools};
use num::Integer;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Direction {
    Left,
    Right,
}

impl FromStr for Direction {
    type Err = Error;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "L" => Ok(Self::Left),
            "R" => Ok(Self::Right),
            _ => Err(anyhow!("Not a direction")),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq)]
struct Node {
    is_start: bool,
    is_end: bool,
    id: usize,
    edges: (usize, usize),
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Node {
    fn get_next(&self, dir: Direction) -> usize {
        match dir {
            Direction::Left => self.edges.0,
            Direction::Right => self.edges.1,
        }
    }
    fn is_start(&self) -> bool {
        self.is_start
    }
    fn is_end(&self) -> bool {
        self.is_end
    }
}

pub async fn day8(input: String) -> Result<(String, String)> {
    // Parsing, retrieve directions and edges
    let (dir_str, edges_str) = input.split_once("\n\n").ok_or(anyhow!("Malformed input"))?;

    let dirs: Vec<Direction> = dir_str.split("").flat_map(|x| x.parse().ok()).collect();
    // All the nodes
    let mut nodes = Vec::new();
    // Mapping from names to ids (indices into the nodes array)
    let mut nodes_name: HashMap<String, usize> = HashMap::new();

    // Resolve names and start building nodes list
    for s in edges_str.lines() {
        // Only get the start of the line since we can't work with the rest yet
        let (from, _) = s.split_once(" = ").ok_or(anyhow!("Bad line"))?;

        // The id is just the index of the node in the nodes array
        let id = nodes.len();
        // Add mapping
        nodes_name.insert(from.to_string(), id);
        // Add node
        nodes.push(Node {
            is_end: from.ends_with('Z'),
            is_start: from.ends_with('A'),
            id,
            edges: (0, 0), // dummy value
        })
    }

    // Add edges once all names are resolved
    for s in edges_str.lines() {
        // Actually parse the whole line
        let (from, rest) = s.split_once(" = ").ok_or(anyhow!("Bad line"))?;
        let rest = &rest[1..(rest.len() - 1)];
        let (left, right) = rest.split_once(", ").ok_or(anyhow!("Bad line"))?;

        // Get the ids
        let id = nodes_name[from];
        let left_id = nodes_name[left];
        let right_id = nodes_name[right];
        // Add edges
        nodes[id].edges = (left_id, right_id)
    }

    // Get ids of start and end nodes for part 1
    let start = &nodes[nodes_name["AAA"]];
    let end = &nodes[nodes_name["ZZZ"]];

    // We take box because generic closures don't exist (the indirection is pretty marginal and
    // this may also just get optimized, maybe)
    let steps_from_to = |from: &Node, is_end: Box<dyn Fn(&Node) -> bool>| {
        dirs.iter()
            .cycle()
            .fold_while((from, 0u64), |(cur, count), &dir| {
                if is_end(cur) {
                    Done((cur, count))
                } else {
                    Continue((&nodes[cur.get_next(dir)], count + 1))
                }
            })
            .into_inner()
            .1
    };

    let part1 = steps_from_to(start, Box::new(|n| n == end));

    // Part 2 is *heavily* based on tricks: assumptions about the input generators allowing me to
    // take shortcuts that don't hold for the more general problem explained on day 8.
    // These assumptions are:
    //  - All starting nodes will eventually lead to a unique cycle
    //  - That cycle only contains one end node
    //  - We can reach the end node from the start in the same ammount of steps as there are nodes
    //  in the cycle. This means that even if the cycle doesn't contain the starting node and the
    //  end node isn't the last node, we can just act as if that was the case (which it is NOT) and
    //  the results will be the same.
    //  - Since the number of steps to go from a start node to an end node is the same as to go
    //  from the end node to itself (the cycle length), no cycle analysis is even necessary, we can
    //  just count the steps to go the end node
    //
    //  These reduce the problem to finding the length of each cycle and finding the point at which
    //  all of them align. This is just the lowest common multiple of their lengths.
    let part2 = nodes
        .iter()
        .copied()
        .filter(Node::is_start)
        .map(|s| steps_from_to(&s, Box::new(Node::is_end)))
        // Find lowest common multiple of the lengths of all the cycles
        .reduce(|a, b| a.lcm(&b))
        .unwrap_or_default();

    Ok((part1.to_string(), part2.to_string()))
}
