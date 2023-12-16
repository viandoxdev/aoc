use anyhow::{Context, Result};
use itertools::Itertools;

fn hash(s: &str) -> u32 {
    s.chars().fold(0, |v, c| (v + c as u32) * 17 % 256)
}

pub async fn day15(input: String) -> Result<(String, String)> {
    let input = input.lines().next().unwrap();
    let part1 = input.split(',').map(hash).sum::<u32>();
    let part2 = {
        let mut boxes: Vec<Vec<(&str, u32)>> = std::iter::repeat_with(|| Vec::new())
            .take(256)
            .collect_vec();

        for step in input.split(',') {
            if step.ends_with('-') {
                let label = &step[..(step.len() - 1)];
                boxes[hash(label) as usize].retain(|&(l, _)| l != label);
            } else {
                let (label, focal) = step.split_once('=').context("Malformed input")?;
                let focal = focal.parse::<u32>()?;
                let r#box = &mut boxes[hash(label) as usize];
                if let Some((_, f)) = r#box.iter_mut().find(|(l, _)| *l == label) {
                    *f = focal;
                } else {
                    r#box.push((label, focal));
                }
            }
        }

        boxes
            .into_iter()
            .enumerate()
            .map(|(box_index, b)| {
                b.into_iter()
                    .enumerate()
                    .map(|(slot_index, (_, focal))| (slot_index as u32 + 1) * focal)
                    .sum::<u32>()
                    * (box_index as u32 + 1)
            })
            .sum::<u32>()
    };

    Ok((part1.to_string(), part2.to_string()))
}
