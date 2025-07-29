use std::{
    collections::VecDeque,
    rc::Rc,
};

use anyhow::{Result, anyhow};
use rustc_hash::FxHashSet;

use crate::intcode::Intcode;

#[derive(Debug, Clone, Copy)]
enum Status {
    Wall = 0,
    Moved = 1,
    OxygenSystem = 2,
}

#[derive(Debug, Clone, Copy)]
enum Movement {
    North = 1,
    South = 2,
    East = 3,
    West = 4,
}

const MOVEMENTS: [Movement; 4] = [
    Movement::North,
    Movement::South,
    Movement::East,
    Movement::West,
];

impl From<i64> for Status {
    fn from(value: i64) -> Self {
        match value {
            0 => Self::Wall,
            1 => Self::Moved,
            2 => Self::OxygenSystem,
            unk => panic!("Invalid status {unk}"),
        }
    }
}

impl From<i64> for Movement {
    fn from(value: i64) -> Self {
        match value {
            1 => Self::North,
            2 => Self::South,
            3 => Self::East,
            4 => Self::West,
            unk => panic!("Invalid movement {unk}"),
        }
    }
}

impl Movement {
    fn dx(self) -> i32 {
        match self {
            Self::East => 1,
            Self::West => -1,
            _ => 0,
        }
    }
    fn dy(self) -> i32 {
        match self {
            Self::North => -1,
            Self::South => 1,
            _ => 0,
        }
    }
}

struct State {
    program: Rc<Intcode<151>>,
    x: i32,
    y: i32,
    distance: u32,
}

impl State {
    fn new(program: Intcode<151>) -> Self {
        Self {
            program: Rc::new(program),
            x: 0,
            y: 0,
            distance: 0,
        }
    }

    fn do_move(&self, movement: Movement) -> Result<(Self, Status)> {
        let mut next = Self {
            program: Rc::clone(&self.program),
            x: self.x + movement.dx(),
            y: self.y + movement.dy(),
            distance: self.distance + 1,
        };

        let status = Status::from(
            Rc::make_mut(&mut next.program)
                .run_until_output(&[movement as i64])?
                .ok_or_else(|| anyhow!("Expected output, program halted"))?,
        );

        Ok((next, status))
    }
}

pub async fn day15(input: String) -> Result<(String, String)> {
    let program: Intcode<151> = input.parse()?;

    let mut walls = FxHashSet::default();
    let mut oxygen_start = (0, 0);

    let part1 = {
        let mut distance = 0;
        let mut queue = VecDeque::new();
        let mut visited = FxHashSet::default();

        queue.push_back(State::new(program.clone()));
        visited.insert((0, 0));

        while let Some(state) = queue.pop_front() {
            for movement in MOVEMENTS {
                let (nx, ny) = (state.x + movement.dx(), state.y + movement.dy());
                if !visited.insert((nx, ny)) {
                    continue;
                }

                let (next, status) = state.do_move(movement)?;
                match status {
                    Status::Wall => {
                        walls.insert((nx, ny));
                    }
                    Status::Moved => queue.push_back(next),
                    Status::OxygenSystem => {
                        oxygen_start = (nx, ny);
                        distance = next.distance;
                        queue.push_back(next);
                    }
                };
            }
        }

        distance
    };

    let part2 = {
        let mut queue = VecDeque::new();
        let mut visited = FxHashSet::default();
        let mut distance = 0;

        queue.push_back((oxygen_start.0, oxygen_start.1, 0));
        visited.insert(oxygen_start);

        while let Some((x, y, dist)) = queue.pop_front() {
            distance = distance.max(dist);

            for movement in MOVEMENTS {
                let (nx, ny) = (x + movement.dx(), y + movement.dy());

                if walls.contains(&(nx, ny)) || !visited.insert((nx, ny)) {
                    continue;
                }

                queue.push_back((nx, ny, distance + 1));
            }
        }

        distance
    };

    Ok((part1.to_string(), part2.to_string()))
}
