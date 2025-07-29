use std::collections::{HashMap, VecDeque};

use anyhow::{Result, anyhow};

fn orbit_number(node: usize, dist: &mut [Option<u32>], parent: &[usize]) -> u32 {
    if let Some(d) = dist[node] {
        return d;
    }

    let d = orbit_number(parent[node], dist, parent) + 1;
    dist[node] = Some(d);
    d
}

fn distance_between(from: usize, to: usize, parent: &[usize], children: &[Vec<usize>]) -> Result<u32> {
    let mut visited = vec![false; parent.len()];
    let mut queue = VecDeque::new();

    visited[from] = true;
    queue.push_back((from, 0));

    while let Some((node, dist)) = queue.pop_front() {
        if node == to {
            return Ok(dist);
        }

        for next in children[node]
            .iter()
            .copied()
            .chain(std::iter::once(parent[node]))
        {
            if !visited[next] {
                visited[next] = true;
                queue.push_back((next, dist + 1));
            }
        }
    }

    Err(anyhow!("Can't reach {to} from {from}"))
}

pub async fn day06(input: String) -> Result<(String, String)> {
    let mut nodes_id = HashMap::new();
    let mut parent = vec![];
    let mut children = vec![];

    macro_rules! id_of {
        ($name:expr) => {{
            *nodes_id.entry($name).or_insert_with(|| {
                let id = parent.len();
                parent.push(id);
                children.push(vec![]);
                id
            })
        }};
    }

    let com = id_of!("COM");
    let you = id_of!("YOU");
    let san = id_of!("SAN");

    for line in input.trim().lines() {
        let (orbited, orbiter) = line.split_once(")").ok_or_else(|| anyhow!("Bad input"))?;
        let (orbited, orbiter) = (id_of!(orbited), id_of!(orbiter));
        parent[orbiter] = orbited;
        children[orbited].push(orbiter);
    }

    let mut distance = vec![None; parent.len()];
    distance[com] = Some(0u32);

    let part1 = (0..parent.len())
        .map(|node| orbit_number(node, &mut distance, &parent))
        .sum::<u32>();

    let part2 = distance_between(parent[you], parent[san], &parent, &children)?;

    Ok((part1.to_string(), part2.to_string()))
}

