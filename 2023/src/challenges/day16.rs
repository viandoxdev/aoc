use anyhow::Result;
use itertools::Itertools;

use rayon::prelude::{IntoParallelRefIterator, ParallelIterator};
use Alignment::*;
use Direction::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Alignment {
    Vertical,
    Horizontal,
    Both,
}

impl Alignment {
    fn add_to(&self, into: &mut Option<Alignment>) {
        if let Some(prev) = into {
            if prev != self {
                *prev = Both;
            }
        } else {
            *into = Some(*self);
        }
    }

    fn is_in(self, inside: Option<Alignment>) -> bool {
        if let Some(al) = inside {
            al == Both || al == self
        } else {
            false
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Direction {
    Up,
    Left,
    Right,
    Down,
}

impl Direction {
    fn step(&self, x: usize, y: usize, width: usize, height: usize) -> Option<(usize, usize)> {
        match self {
            Up => Some((x, y.checked_sub(1)?)),
            Left => Some((x.checked_sub(1)?, y)),
            Right => (x < width - 1).then_some((x + 1, y)),
            Down => (y < height - 1).then_some((x, y + 1)),
        }
    }

    fn reflect(&self) -> Self {
        match self {
            Up => Right,
            Left => Down,
            Right => Up,
            Down => Left,
        }
    }

    fn reflect_reverse(&self) -> Self {
        match self {
            Up => Left,
            Left => Up,
            Right => Down,
            Down => Right,
        }
    }

    fn alignment(&self) -> Alignment {
        match self {
            Up | Down => Vertical,
            Left | Right => Horizontal,
        }
    }
}

pub async fn day16(input: String) -> Result<(String, String)> {
    let chars = input.chars().collect_vec();
    let width = input.find('\n').unwrap();
    let height = input.lines().count();

    let solve = |start: (usize, usize, Direction)| {
        let mut alignments = vec![None; chars.len()];
        let mut next_alignments = alignments.clone();

        let mut beams = vec![start];
        let mut next_beams = Vec::new();
        while !beams.is_empty() {
            'beams: for (mut x, mut y, mut dir) in beams.drain(..) {
                loop {
                    let index = y * (width + 1) + x;

                    let char = chars[index];
                    if dir.alignment().is_in(alignments[index]) && char != '/' && char != '\\' {
                        continue 'beams;
                    }

                    dir.alignment().add_to(&mut next_alignments[index]);

                    match (char, dir.alignment()) {
                        ('-', Vertical) => {
                            next_beams.push((x, y, Left));
                            next_beams.push((x, y, Right));
                            continue 'beams;
                        }
                        ('|', Horizontal) => {
                            next_beams.push((x, y, Up));
                            next_beams.push((x, y, Down));
                            continue 'beams;
                        }
                        _ => {
                            dir = match char {
                                '/' => dir.reflect(),
                                '\\' => dir.reflect_reverse(),
                                _ => dir,
                            };

                            let Some(next) = dir.step(x, y, width, height) else {
                                continue 'beams;
                            };

                            (x, y) = next;
                        }
                    }
                }
            }

            std::mem::swap(&mut beams, &mut next_beams);
            alignments.copy_from_slice(&next_alignments[..]);
        }

        alignments.into_iter().flatten().count()
    };

    let starts = (0..height)
        .flat_map(|y| [(0, y, Right), (width - 1, y, Left)])
        .chain((0..width).flat_map(|x| [(x, 0, Down), (x, height - 1, Up)]))
        .collect_vec();
    let results = starts.par_iter().copied().map(solve).collect::<Vec<_>>();

    let part1 = results[0];
    let part2 = results.into_iter().max().unwrap();

    Ok((part1.to_string(), part2.to_string()))
}
