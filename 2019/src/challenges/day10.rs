use std::{cmp::Ordering, str::FromStr};

use anyhow::Result;
use itertools::Itertools;

include!(concat!(env!("OUT_DIR"), "/day10_data.rs"));

const HIT_ASTEROIDS_SIZE: usize = MASKS.len().div_ceil(64);

#[derive(Debug)]
struct HitAsteroids {
    data: [u64; HIT_ASTEROIDS_SIZE],
    count: usize,
}

impl Default for HitAsteroids {
    fn default() -> Self {
        Self {
            data: [0; HIT_ASTEROIDS_SIZE],
            count: 0,
        }
    }
}

impl HitAsteroids {
    pub fn set(&mut self, i: usize) {
        let bit = 1 << (i % 64);
        self.count += (self.data[i / 64] & bit == 0) as usize;
        self.data[i / 64] |= bit;
    }
    pub fn into_vec(mut self) -> Vec<usize> {
        let mut vec = Vec::new();
        while let Some(index) = self.pop() {
            vec.push(index);
        }
        vec
    }
    pub fn pop(&mut self) -> Option<usize> {
        let (word_index, word) = self
            .data
            .iter()
            .copied()
            .enumerate()
            .find(|&(_, x)| x != 0)?;
        let bit_index = word.trailing_zeros() as usize;
        self.data[word_index] ^= 1 << bit_index;
        self.count -= 1;
        Some(bit_index + word_index * 64)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Mask {
    // We need at least SIZE * 2 - 1 (= 71) bits
    data: [u128; MASK_SIZE],
    dx: i32,
    dy: i32,
}

#[derive(Debug, Clone, Copy)]
struct Bitmap {
    data: [u64; SIZE],
}

impl Bitmap {
    fn and(&self, mask: &Mask, dx: i32, dy: i32) -> Self {
        let mut data = [0; SIZE];

        for (y, line) in self.data.iter().enumerate() {
            let mask_line = mask.data[(y as i32 + dy) as usize];
            let mask_line = (if dx > 0 {
                mask_line >> dx as usize
            } else {
                mask_line << (-dx) as usize
            } & ((1 << SIZE) - 1)) as u64;

            data[y] = line & mask_line
        }

        Self { data }
    }

    fn is_null(&self) -> bool {
        self.data.iter().all(|&x| x == 0)
    }

    fn asteroid_hits(&self, x: usize, y: usize) -> HitAsteroids {
        let mut hit = HitAsteroids::default();
        let dx = SIZE as i32 - 1 - x as i32;
        let dy = SIZE as i32 - 1 - y as i32;
        for (i, mask) in MASKS.iter().enumerate() {
            let (x, y) = (
                SIZE as i32 - 1 + mask.dx - dx,
                SIZE as i32 - 1 + mask.dy - dy,
            );
            if x < 0 // Bounds check
                || x >= SIZE as i32
                || y < 0
                || y >= SIZE as i32
                || self.and(mask, dx, dy).is_null() // Sees asteroid
            {
                continue;
            }

            hit.set(i);
        }

        hit
    }

    fn vaporise(&mut self, x: usize, y: usize, dx: i32, dy: i32) -> Option<u32> {
        let (mut x, mut y) = (x as i32 + dx, y as i32 + dy);
        while 0 <= x && x < MASK_SIZE as i32 && 0 <= y && y < MASK_SIZE as i32 {
            let bit = 1 << (x as usize);
            if self.data[y as usize] & bit != 0 {
                self.data[y as usize] ^= bit;
                return Some(x as u32 * 100 + y as u32)
            }

            x += dx;
            y += dy;
        }

        None
    }
}

impl PartialOrd for Mask {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Mask {
    fn cmp(&self, other: &Self) -> Ordering {
        let (a, b) = (self, other);
        if a.dx == b.dx && a.dy == b.dy {
            return Ordering::Equal;
        }

        if (a.dx != 0
            && b.dx != 0
            && a.dy * b.dx.abs() * a.dx.signum() > b.dy * a.dx.abs() * b.dx.signum())
            || (a.dx == 0 && a.dy > 0)
            || (b.dx == 0 && b.dy < 0)
        {
            Ordering::Greater
        } else {
            Ordering::Less
        }
    }
}

impl FromStr for Bitmap {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        let mut data = [0; SIZE];

        for (y, line) in s.trim().lines().enumerate() {
            data[y] = line
                .chars()
                .rev()
                .fold(0, |a, v| a << 1 | ((v == '#') as u64));
        }

        Ok(Self { data })
    }
}

pub async fn day10(input: String) -> Result<(String, String)> {
    let mut bitmap: Bitmap = input.parse()?;

    let (mut max_x, mut max_y) = (0, 0);
    let mut max_hit = HitAsteroids::default();

    for (x, y) in (0..SIZE)
        .cartesian_product(0..SIZE)
        .filter(|&(x, y)| bitmap.data[y] & (1 << x) != 0)
    {
        let hit = bitmap.asteroid_hits(x, y);
        if hit.count > max_hit.count {
            max_hit = hit;
            max_x = x;
            max_y = y;
        }
    }

    let part1 = max_hit.count;
    let indices = max_hit.into_vec();

    let mut vaporised = 0;
    let mut part2 = 0;

    for index in indices.iter().copied().cycle() {
        let mask = MASKS[index];
        if let Some(coords) = bitmap.vaporise(max_x, max_y, mask.dx, mask.dy) {
            vaporised += 1;
            part2 = coords;

            if vaporised == 200 {
                break;
            }
        }
    }

    Ok((part1.to_string(), part2.to_string()))
}
