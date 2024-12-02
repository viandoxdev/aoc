use itertools::Itertools;

use anyhow::{anyhow, Context, Result};

use Direction::*;
use Winding::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Winding {
    Clockwise,
    CounterClockwise,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Direction {
    Up,
    Left,
    Down,
    Right,
}

impl Direction {
    fn winding(self, rhs: Self) -> Option<Winding> {
        match (self, rhs) {
            (Up, Right) | (Right, Down) | (Down, Left) | (Left, Up) => Some(Clockwise),
            (Up, Left) | (Left, Down) | (Down, Right) | (Right, Up) => Some(CounterClockwise),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Instruction {
    dir: Direction,
    mag: i64,
}

impl Instruction {
    fn delta(self) -> (i64, i64) {
        match self.dir {
            Up => (0, -self.mag),
            Left => (-self.mag, 0),
            Down => (0, self.mag),
            Right => (self.mag, 0),
        }
    }
}

// Workaround for Chain not implementing ExactSizeIterator (for good reasons but these don't apply
// to us here)
/// A chain that implements ExactSizeIterator
struct ExactSizeChain<A, B> {
    iter1: A,
    iter2: B,
    size: usize,
}

impl<A: Clone, B: Clone> Clone for ExactSizeChain<A, B> {
    fn clone(&self) -> Self {
        Self {
            iter1: self.iter1.clone(),
            iter2: self.iter2.clone(),
            size: self.size,
        }
    }
}

impl<I, A: Iterator<Item = I> + ExactSizeIterator, B: Iterator<Item = I> + ExactSizeIterator>
    Iterator for ExactSizeChain<A, B>
{
    type Item = I;
    fn next(&mut self) -> Option<Self::Item> {
        self.iter1.next().or_else(|| self.iter2.next())
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.size, Some(self.size))
    }
}

impl<I, A: Iterator<Item = I> + ExactSizeIterator, B: Iterator<Item = I> + ExactSizeIterator>
    ExactSizeIterator for ExactSizeChain<A, B>
{
    fn len(&self) -> usize {
        self.size
    }
}

trait IterExt {
    fn exact_chain<T: ExactSizeIterator>(self, other: T) -> ExactSizeChain<Self, T>
    where
        Self: Sized;
}

impl<I: Iterator + ExactSizeIterator> IterExt for I {
    fn exact_chain<T: ExactSizeIterator>(self, other: T) -> ExactSizeChain<Self, T>
    where
        Self: Sized,
    {
        let size = self.len() + other.len();
        ExactSizeChain {
            iter1: self,
            iter2: other,
            size,
        }
    }
}

/// Compute the area enclosed bu the instructions.
fn area(instructions: &[Instruction]) -> i64 {
    // Find the index of the instruction getting us to the highest, leftmost point.
    let (_, _, min_ins, _, _) = instructions.iter().enumerate().fold(
        (0, 0, 0, 0, 0),
        |(pos_x, pos_y, min_index, min_x, min_y), (i, ins)| {
            // Vector of the movement we do with this instruction
            let (dx, dy) = ins.delta();
            // New position after the instruction
            let (pos_x, pos_y) = (pos_x + dx, pos_y + dy);
            if pos_y < min_y || (pos_y == min_y && pos_x < min_x) {
                // If we found a new highest point, or a point with the same height but that is more on
                // the left, update to this one
                (pos_x, pos_y, i, pos_x, pos_y)
            } else {
                // Otherwise, just update the position and continue
                (pos_x, pos_y, min_index, min_x, min_y)
            }
        },
    );

    // Get the next instruction (the one starting from the highest leftmost point)
    let start_ins = (min_ins + 1) % instructions.len();

    // Winding of the whole polygon
    let global_winding = instructions[min_ins]
        .dir
        .winding(instructions[start_ins].dir)
        .unwrap();

    // Iterator over the instructions, from the one that starts from the highest leftmost point.
    let insts = instructions[start_ins..]
        .iter()
        .copied()
        .exact_chain(instructions[..start_ins].iter().copied());

    // Since we're starting from a leftmost highpoint, we know that above this point is outside and
    // left of this point is too.

    // We compute the area using a varient of the shoelace formula
    let mut area_sum = 0;
    // The current y we're at, to compute the area of the parallelograms (axis aligned rectangles
    // here)
    let mut y = 0;

    for (prev, cur, next) in insts.circular_tuple_windows() {
        // Number of matching windings minus one: -1, 0 or 1.
        // For a Clockwise global_winding, with all horizontal instructions of magnitude 9:
        //               len: 10
        //    #        <-------->
        //    #      (CW)      (CW)
        //    #        ########## len: 9
        //    # len: 8 #   ^1   #<------->
        //    #<------>#        #       (CW)
        //    ##########        ##########
        // (CCW)  ^-1 (CCW)  (CCW)  ^0   #
        let matching_windings = (prev.dir.winding(cur.dir) == Some(global_winding)) as i64
            + (cur.dir.winding(next.dir) == Some(global_winding)) as i64
            - 1;
        // Length of the oustide edge created by this block
        let outside_len = cur.mag + matching_windings;

        if matches!(cur.dir, Left | Right) {
            // We have an horizontal instruction, will count in area.
            // Area of the rectange formed between the edge and the x axis
            let area = y * outside_len; 
            // Change the sign depending on the direction
            area_sum += if cur.dir == Right { area } else { -area };
        } else {
            // We have a vertical instruction, update y
            y += if cur.dir == Up {
                outside_len
            } else {
                -outside_len
            };
        }
    }

    area_sum
}

pub async fn day18(input: String) -> Result<(String, String)> {
    // Parse the instructions for part 1
    let instructions_part1: Vec<Instruction> = input
        .lines()
        .map(|l| -> Result<Instruction> {
            let dir = match &l[0..1] {
                "U" => Ok(Up),
                "L" => Ok(Left),
                "D" => Ok(Down),
                "R" => Ok(Right),
                _ => Err(anyhow!("Bad direction")),
            }?;
            let mag: i64 = l[2..].split_once(' ').unwrap().0.parse()?;

            Ok(Instruction { dir, mag })
        })
        .try_collect()?;

    // Parse the instructions from the hex code for part 2
    let instructions_part2: Vec<Instruction> = input
        .lines()
        .map(|l| -> Result<Instruction> {
            let (_, hex) = l.split_once('#').context("Malfored input")?;
            let hex = &hex[..hex.len() - 1];
            let mag = i64::from_str_radix(&hex[..5], 16)?;
            let dir = match &hex[5..6] {
                "0" => Ok(Right),
                "1" => Ok(Down),
                "2" => Ok(Left),
                "3" => Ok(Up),
                _ => Err(anyhow!("Bad hex direction")),
            }?;
            Ok(Instruction { dir, mag })
        })
        .try_collect()?;

    let part1 = area(&instructions_part1);
    let part2 = area(&instructions_part2);

    Ok((part1.to_string(), part2.to_string()))
}
