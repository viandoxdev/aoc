use std::collections::{HashMap, VecDeque};

use anyhow::{anyhow, Result};
use itertools::Itertools;

fn sort_3((a, b, c): (usize, usize, usize)) -> (usize, usize, usize) {
    if a <= b && a <= c {
        if b <= c {
            (a, b, c)
        } else {
            (a, c, b)
        }
    } else if b <= a && b <= c {
        if a <= c {
            (b, a, c)
        } else {
            (b, c, a)
        }
    } else if a <= b {
        (c, a, b)
    } else {
        (c, b, a)
    }
}

fn size_of_component(
    visited: &mut [bool],
    start_node: usize,
    edges: &[Vec<(usize, usize)>],
) -> usize {
    let mut count = 0usize;
    let mut queue = VecDeque::new();

    visited[start_node] = true;
    queue.push_back(start_node);

    while let Some(node) = queue.pop_front() {
        count += 1;
        for &(to, _) in &edges[node] {
            if !visited[to] {
                visited[to] = true;
                queue.push_back(to);
            }
        }
    }

    count
}

pub async fn day25(input: String) -> Result<(String, String)> {
    let mut edges_count = 0usize;
    let mut nodes_map = HashMap::new();
    let mut edges_map = HashMap::new();
    let mut edges = Vec::new();

    macro_rules! node_id {
        ($str:expr) => {
            *nodes_map.entry($str.to_string()).or_insert_with(|| {
                let id = edges.len();
                edges.push(vec![]);
                id
            })
        };
    }

    for line in input.lines().filter(|l| !l.is_empty()) {
        let (from, rhs) = line.split_once(": ").ok_or_else(|| anyhow!("Bad line"))?;
        let from_id = node_id!(from);
        for to in rhs.split(" ") {
            let to_id = node_id!(to);
            let edge_id = *edges_map.entry((from, to)).or_insert_with(|| {
                let id = edges_count;
                edges_count += 1;
                id
            });
            edges[from_id].push((to_id, edge_id));
            edges[to_id].push((from_id, edge_id));
        }
    }

    let mut edge_throughput = vec![0; edges_count];
    let mut visited = vec![false; edges.len()];
    let mut queue = VecDeque::new();

    for start_node in 0..edges.len() {
        visited.fill(false);
        queue.clear();

        visited[start_node] = true;
        queue.push_back(start_node);

        while let Some(node) = queue.pop_front() {
            for &(to, id) in &edges[node] {
                if !visited[to] {
                    edge_throughput[id] += 1;
                    visited[to] = true;
                    queue.push_back(to);
                }
            }
        }
    }

    let sorted_edges = (0..edges_count)
        .sorted_by_key(|&edge| -edge_throughput[edge])
        .collect_vec();

    let part1 = (3..edges_count)
        .flat_map(|n| {
            sorted_edges[0..n]
                .iter()
                .copied()
                .tuple_combinations()
                .map(sort_3)
        })
        .unique()
        .find_map(|cut_edges| {
            let mut edges = edges.clone();
            for edges in &mut edges {
                edges
                    .retain(|&(_, id)| cut_edges.0 != id && cut_edges.1 != id && cut_edges.2 != id);
            }

            visited.fill(false);
            let a = size_of_component(&mut visited, 0, &edges);
            let non_visited = (0..edges.len()).find(|&node| !visited[node])?;
            let b = size_of_component(&mut visited, non_visited, &edges);

            Some(a * b)
        })
        .unwrap();

    Ok((part1.to_string(), "".to_string()))
}
