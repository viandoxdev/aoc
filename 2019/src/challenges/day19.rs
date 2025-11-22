use anyhow::Result;

use crate::intcode::Intcode;

struct Beam {
    program: Intcode<191>,
    is_down: bool,
}

impl Beam {
    fn new(program: Intcode<191>) -> Self {
        let mut res = Self {
            program,
            is_down: false,
        };
        res.is_down = res.find_beam_orientation();
        res
    }
    fn find_beam_orientation(&self) -> bool {
        for d in 1.. {
            for x in 0..=d {
                if self.contains((x, d - x)) {
                    return 2 * x <= d;
                }
            }
        }

        unreachable!()
    }
    fn contains(&self, (x, y): (i64, i64)) -> bool {
        self.program
            .clone()
            .run_until_output(&[x, y])
            .expect("Error when running tractor beam program")
            .expect("Tractor beam program returned with no output")
            == 1
    }
    fn step((x, y): (i64, i64), down: bool) -> (i64, i64) {
        if down { (x, y + 1) } else { (x + 1, y) }
    }
    fn unstep((x, y): (i64, i64), down: bool) -> (i64, i64) {
        if down { (x, y - 1) } else { (x - 1, y) }
    }
}

struct BeamBoundIter<'a> {
    beam: &'a Beam,
    pos: (i64, i64),
    is_lost: bool,
    is_top: bool,
}

impl<'a> BeamBoundIter<'a> {
    fn new(beam: &'a Beam, top: bool) -> Self {
        Self {
            beam,
            pos: (0, 0),
            is_lost: false,
            is_top: top,
        }
    }
    fn step(&mut self, reverse: bool) {
        self.pos = Beam::step(self.pos, self.beam.is_down != reverse)
    }
    fn is_on(&self) -> bool {
        self.beam.contains(self.pos)
    }
}

impl<'a> Iterator for BeamBoundIter<'a> {
    type Item = (i64, i64);
    fn next(&mut self) -> Option<Self::Item> {
        if self.is_top == !self.beam.is_down {
            self.step(false);
            if self.is_on() {
                self.is_lost = false;
            } else if !self.is_lost {
                self.step(true);
                self.is_lost = true;
            }
            Some(self.pos)
        } else {
            self.step(!self.is_lost);
            if self.is_on() {
                self.is_lost = false;
                Some(self.pos)
            } else {
                if !self.is_lost {
                    self.step(false);
                    if self.is_on() {
                        return Some(self.pos);
                    }
                    self.is_lost = true;
                }
                Some(Beam::unstep(self.pos, !self.beam.is_down))
            }
        }
    }
}

fn part1(beam: &Beam) -> i64 {
    let mut total = 0i64;
    for y in 0..50 {
        for x in 0..50 {
            let is_in = beam.contains((x, y));
            total += is_in as i64;
        }
    }

    total
}

fn part2(beam: &Beam) -> i64 {
    let box_size = 100;

    let ((tx, ty), (bx, by)) = BeamBoundIter::new(beam, true)
        .skip(if beam.is_down { 0 } else { box_size - 1 })
        .zip(BeamBoundIter::new(beam, false).skip(if beam.is_down { box_size - 1 } else { 0 }))
        .find(|&((tx, ty), (bx, by))| {
            if beam.is_down {
                tx - bx + 1 >= box_size as i64
            } else {
                by - ty + 1 >= box_size as i64
            }
        })
        .unwrap();

    if beam.is_down {
        bx * 10000 + ty
    } else {
        tx * 10000 + by
    }
}

pub async fn day19(input: String) -> Result<(String, String)> {
    let beam = Beam::new(input.parse()?);

    let part1 = part1(&beam);
    let part2 = part2(&beam);

    Ok((part1.to_string(), part2.to_string()))
}

