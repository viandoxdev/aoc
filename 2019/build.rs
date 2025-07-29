// Day 10 stuff

use std::{cmp::Ordering, env, fs, path::Path};

use itertools::Itertools;

const fn coprime(mut a: i32, mut b: i32) -> bool {
    (a, b) = (a.abs(), b.abs());
    while b != 0 {
        (a, b) = (b, a % b);
    }

    a == 1
}

const SIZE: usize = 36;
const MASK_SIZE: usize = SIZE * 2 - 1;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Mask {
    // We need at least SIZE * 2 - 1 (= 71) bits
    data: [u128; MASK_SIZE],
    dx: i32,
    dy: i32,
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

        // Handle the zero cases
        if a.dx == 0 {
            if a.dy < 0 || b.dx < 0 {
                Ordering::Less
            } else {
                Ordering::Greater
            }
        } else if b.dx == 0 {
            if b.dy < 0 || a.dx < 0 {
                Ordering::Greater
            } else {
                Ordering::Less
            }
        } else if a.dx.signum() != b.dx.signum() {
            b.dx.cmp(&a.dx)
        } else if a.dy * b.dx.abs() * a.dx.signum() > b.dy * a.dx.abs() * b.dx.signum() {
            Ordering::Greater
        } else {
            Ordering::Less
        }
    }
}

fn main() {
    let min = 1 - SIZE as i32;
    let max = SIZE as i32 - 1;
    let masks = (min..=max)
        .cartesian_product(min..=max)
        .filter(|&(dx, dy)| coprime(dx, dy))
        .map(|(dx, dy)| {
            let mut data = [0; MASK_SIZE];

            let (mut x, mut y) = (SIZE as i32 - 1 + dx, SIZE as i32 - 1 + dy);
            while 0 <= x && x < MASK_SIZE as i32 && 0 <= y && y < MASK_SIZE as i32 {
                data[y as usize] |= 1 << (x as usize);

                x += dx;
                y += dy;
            }

            Mask { data, dx, dy }
        })
        .sorted()
        .collect_vec();

    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("day10_data.rs");

    fs::write(
        &dest_path,
        format!(
            "\
            const SIZE: usize = {SIZE};\n\
            const MASK_SIZE: usize = {MASK_SIZE};\n\
            static MASKS: [Mask; {}] = {masks:?};\n\
        ",
            masks.len()
        ),
    )
    .unwrap();

    println!("cargo:rerun-if-changed=build.rs");
}
