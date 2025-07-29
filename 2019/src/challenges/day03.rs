use anyhow::{Result, anyhow};
use itertools::Itertools;

#[derive(Debug, Clone, Copy)]
enum Segment {
    Vert {
        x: i32,
        origin_y: i32,
        start_y: i32,
        end_y: i32,
        steps: u32,
    },
    Horz {
        y: i32,
        origin_x: i32,
        start_x: i32,
        end_x: i32,
        steps: u32,
    },
}

impl Segment {
    fn vert(x: i32, mut y1: i32, mut y2: i32, steps: u32) -> Self {
        let origin_y = y1;
        if y1 > y2 {
            std::mem::swap(&mut y1, &mut y2);
        }

        Self::Vert {
            x,
            origin_y,
            start_y: y1,
            end_y: y2,
            steps,
        }
    }
    fn horz(y: i32, mut x1: i32, mut x2: i32, steps: u32) -> Self {
        let origin_x = x1;
        if x1 > x2 {
            std::mem::swap(&mut x1, &mut x2);
        }

        Self::Horz {
            y,
            origin_x,
            start_x: x1,
            end_x: x2,
            steps,
        }
    }
    fn intersects_once(self, other: Self) -> Option<(i32, i32, u32)> {
        match self {
            Self::Horz { .. } => match other {
                Self::Horz { .. } => None,
                Self::Vert { .. } => other.intersects_once(self),
            },
            Self::Vert {
                x,
                origin_y,
                start_y,
                end_y,
                steps: steps1,
            } => match other {
                Self::Vert { .. } => None,
                Self::Horz {
                    y,
                    origin_x,
                    start_x,
                    end_x,
                    steps: steps2,
                } => (start_x <= x && x <= end_x && start_y <= y && y <= end_y).then_some((
                    x,
                    y,
                    steps1 + steps2 + (x - origin_x).unsigned_abs() + (y - origin_y).unsigned_abs(),
                )),
            },
        }
    }
}

fn wire_to_segments(str: &str) -> Result<Vec<Segment>> {
    let mut last_pos = (0, 0);
    let mut steps = 0u32;

    macro_rules! seg {
        (ver: $d:expr) => {{
            let ((sx, sy), last_steps) = (last_pos, steps);
            last_pos = (last_pos.0, last_pos.1 + $d);
            steps += $d.unsigned_abs();
            Segment::vert(sx, sy, last_pos.1, last_steps)
        }};
        (hor: $d:expr) => {{
            let ((sx, sy), last_steps) = (last_pos, steps);
            last_pos = (last_pos.0 + $d, last_pos.1);
            steps += $d.unsigned_abs();
            Segment::horz(sy, sx, last_pos.0, last_steps)
        }};
    }

    str.split(",")
        .map(|m| {
            let n = m[1..].parse::<i32>()?;
            match &m[0..1] {
                "U" => Ok(seg!(ver: -n)),
                "D" => Ok(seg!(ver: n)),
                "L" => Ok(seg!(hor: -n)),
                "R" => Ok(seg!(hor: n)),
                _ => Err(anyhow!("Bad movement")),
            }
        })
        .collect::<Result<Vec<Segment>>>()
}

pub async fn day03(input: String) -> Result<(String, String)> {
    let (wire1_str, wire2_str) = input
        .trim()
        .split_once("\n")
        .ok_or_else(|| anyhow!("Bad input"))?;
    let (segs1, segs2) = (wire_to_segments(wire1_str)?, wire_to_segments(wire2_str)?);

    let intersections = segs1
        .iter()
        .copied()
        .cartesian_product(segs2.iter().copied())
        .filter_map(|(s1, s2)| s1.intersects_once(s2))
        .filter(|&(x, y, _)| x != 0 || y != 0)
        .collect_vec();

    let part1 = intersections
        .iter()
        .map(|&(x, y, _)| x.abs() + y.abs())
        .min()
        .unwrap_or_default();

    let part2 = intersections
        .iter()
        .map(|&(_, _, steps)| steps)
        .min()
        .unwrap_or_default();

    Ok((part1.to_string(), part2.to_string()))
}
