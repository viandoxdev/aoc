use std::collections::HashMap;

use anyhow::Result;

use crate::{bigletters, intcode::{Intcode, ProgramState}};

#[derive(Debug, Clone, Copy)]
enum Color {
    White = 1,
    Black = 0,
}

impl From<i64> for Color {
    fn from(value: i64) -> Self {
        match value {
            0 => Self::Black,
            1 => Self::White,
            c => panic!("Invalid color {c}"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Direction {
    Up,
    Left,
    Right,
    Down,
}

impl Direction {
    fn left(self) -> Self {
        match self {
            Self::Up => Self::Left,
            Self::Left => Self::Down,
            Self::Down => Self::Right,
            Self::Right => Self::Up,
        }
    }
    fn right(self) -> Self {
        match self {
            Self::Left => Self::Up,
            Self::Down => Self::Left,
            Self::Right => Self::Down,
            Self::Up => Self::Right,
        }
    }
    fn forward(self, (x, y): (i32, i32)) -> (i32, i32) {
        match self {
            Self::Left => (x - 1, y),
            Self::Down => (x, y + 1),
            Self::Right => (x + 1, y),
            Self::Up => (x, y - 1),
        }
    }
    fn turn(self, turn: i64) -> Self {
        match turn {
            0 => self.left(),
            1 => self.right(),
            c => panic!("Invalid turn {c}"),
        }
    }
}

fn run_robot(mut program: Intcode<111>, origin_color: Color) -> Result<HashMap<(i32, i32), Color>> {
    let mut tiles = HashMap::new();
    let mut input = None;
    let mut position = (0, 0);
    let mut painted = false;
    let mut direction = Direction::Up;

    tiles.insert(position, origin_color);

    loop {
        match program.step(input)? {
            ProgramState::Running => {}
            ProgramState::Finished => break,
            ProgramState::AwaitingInput => {
                input = Some(tiles.get(&position).copied().unwrap_or(Color::Black) as i64);
            }
            ProgramState::ConsumedInput => input = None,
            ProgramState::PendingOutput(c) if !painted => {
                tiles.insert(position, Color::from(c));
                painted = true;
            }
            ProgramState::PendingOutput(t) => {
                direction = direction.turn(t);
                position = direction.forward(position);
                painted = false;
            }
        }
    }

    Ok(tiles)
}

pub async fn day11(input: String) -> Result<(String, String)> {
    let program: Intcode<111> = input.parse()?;

    let part1 = run_robot(program.clone(), Color::Black)?.len();
    let part2 = {
        let tiles = run_robot(program, Color::White)?;

        let (min_x, min_y, max_x, max_y) = tiles.keys().copied().fold(
            (i32::MAX, i32::MAX, i32::MIN, i32::MIN),
            |(min_x, min_y, max_x, max_y), (x, y)| {
                (min_x.min(x), min_y.min(y), max_x.max(x), max_y.max(y))
            },
        );

        let (width, height) = ((max_x - min_x + 1) as usize, (max_y - min_y + 1) as usize);
        let mut bitmap = vec![false; width * height];

        for ((x, y), color) in tiles {
            bitmap[(y - min_y) as usize * width + (x - min_x) as usize] = matches!(color, Color::White);
        }

        bigletters::read_bitmap_string(&bitmap, width)
    };

    Ok((part1.to_string(), part2))
}

