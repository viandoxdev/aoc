use std::{
    cmp::Ordering,
    collections::{HashMap, VecDeque},
    fmt::Display,
};

use anyhow::{Context, Result, anyhow};
use itertools::Itertools;

use crate::intcode::{Intcode, ProgramState};

const OUTPUT_LEN_CAP: usize = 5000;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Direction {
    West = 0,
    North = 1,
    East = 2,
    South = 3,
}

impl Direction {
    fn opposite(self) -> Self {
        match self {
            Direction::West => Direction::East,
            Direction::East => Direction::West,
            Direction::North => Direction::South,
            Direction::South => Direction::North,
        }
    }
}

impl TryFrom<usize> for Direction {
    type Error = anyhow::Error;
    fn try_from(value: usize) -> Result<Self> {
        match value {
            0 => Ok(Direction::West),
            1 => Ok(Direction::North),
            2 => Ok(Direction::East),
            3 => Ok(Direction::South),
            d => Err(anyhow!("Invalid direction '{d}'")),
        }
    }
}

impl Display for Direction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Direction::West => write!(f, "west"),
            Direction::East => write!(f, "east"),
            Direction::North => write!(f, "north"),
            Direction::South => write!(f, "south"),
        }
    }
}

#[derive(Debug, Clone)]
enum Command {
    Take(String),
    Move(Direction),
}

impl Display for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Command::Take(s) => write!(f, "take {s}"),
            Command::Move(s) => write!(f, "{s}"),
        }
    }
}

#[derive(Debug)]
struct Room {
    neighbors: [Option<String>; 4],
}

#[derive(Debug)]
struct Spaceship {
    rooms: HashMap<String, Room>,
    item_start_room: HashMap<String, String>,
    goal: String,
    gateway: String,
    start: String,
    droid_template: Droid,
}

#[derive(Debug, Clone)]
struct Droid {
    program: Intcode<251>,
}

#[derive(Debug, Clone)]
enum CommandFeedback {
    Halted,
    ItemPickedUp,
    Other,
    RoomInformation(CurrentRoomInformation),
    Finished(String),
}

impl CommandFeedback {
    pub fn room_information(self) -> Option<CurrentRoomInformation> {
        match self {
            Self::RoomInformation(inf) => Some(inf),
            _ => None,
        }
    }
}

#[derive(Debug, Default, Clone)]
struct CurrentRoomInformation {
    name: String,
    items: Vec<String>,
    exits: [bool; 4],
    pressure_diag: Option<Ordering>,
    kicked_back: bool,
}

impl Droid {
    fn parse_program_output(output: String) -> Result<CommandFeedback> {
        let mut inf = CurrentRoomInformation::default();
        let mut res = CommandFeedback::Other;

        #[derive(Debug, Clone, Copy)]
        enum State {
            None,
            Exits,
            Items,
        }

        let mut state = State::None;

        for line in output.lines() {
            if line.starts_with("== ") {
                if inf.name == "" {
                    inf.name = line[3..(line.len() - 3)].to_string();
                } else {
                    inf.kicked_back = true;
                }

                state = State::None;
            } else if line == "Doors here lead:" {
                state = State::Exits;
            } else if line == "Items here:" {
                state = State::Items;
            } else if line.contains("Alert!") {
                if line.contains("lighter") {
                    inf.pressure_diag = Some(Ordering::Less)
                } else {
                    inf.pressure_diag = Some(Ordering::Greater)
                }
            } else if line.contains("get in by typing") {
                res = CommandFeedback::Finished(
                    line.chars().filter(|c| c.is_ascii_digit()).collect(),
                );
                inf.name = "".to_string();
            } else if line.starts_with("- ") {
                match state {
                    State::None => return Err(anyhow!("List item outside of item or exits scope")),
                    State::Exits => match line {
                        "- west" => inf.exits[Direction::West as usize] = true,
                        "- east" => inf.exits[Direction::East as usize] = true,
                        "- north" => inf.exits[Direction::North as usize] = true,
                        "- south" => inf.exits[Direction::South as usize] = true,
                        _ => return Err(anyhow!("Unrecognized exit door '{line}'")),
                    },
                    State::Items => {
                        inf.items.push(line[2..].to_string());
                    }
                }
            } else if line.starts_with("You take") {
                res = CommandFeedback::ItemPickedUp
            }
        }

        if inf.name != "" {
            Ok(CommandFeedback::RoomInformation(inf))
        } else {
            Ok(res)
        }
    }

    fn step(&mut self, command: Option<Command>) -> Result<CommandFeedback> {
        let input = match command {
            Some(c) => c
                .to_string()
                .bytes()
                .map(|b| b as i64)
                .chain(std::iter::once(10))
                .collect_vec(),
            None => Vec::new(),
        };

        let mut input = &input[..];
        let mut output = Vec::new();

        let finished = loop {
            if output.len() > OUTPUT_LEN_CAP {
                // Ran into infinite loop
                return Ok(CommandFeedback::Halted);
            }

            match self.program.step(input.first().copied())? {
                ProgramState::Running => {}
                ProgramState::Finished => break true,
                ProgramState::AwaitingInput => break false,
                ProgramState::ConsumedInput => input = &input[1..],
                ProgramState::PendingOutput(out) => output.push(out),
            };
        };

        let output: String = output
            .iter()
            .filter_map(|&c| char::from_u32(c as u32))
            .collect();

        let res = Self::parse_program_output(output)?;
        if finished && !matches!(res, CommandFeedback::Finished(_)) {
            return Ok(CommandFeedback::Halted);
        }

        Ok(res)
    }
}

#[derive(Debug, Clone)]
struct OnesIterator(u16);

impl Iterator for OnesIterator {
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        let res = self.0.trailing_zeros();
        if res < 16 {
            self.0 ^= 1 << res;
            Some(res as usize)
        } else {
            None
        }
    }
}

impl Spaceship {
    pub fn explore(mut droid: Droid) -> Result<Self> {
        let mut queue = VecDeque::new();

        let mut rooms = HashMap::new();
        let mut goal = String::new();
        let mut gateway = String::new();
        let mut item_start_room = HashMap::new();

        let start = droid
            .step(None)?
            .room_information()
            .context("No start room information")?;

        for item in &start.items {
            item_start_room.insert(item.clone(), start.name.clone());
        }

        rooms.insert(
            start.name.clone(),
            Room {
                neighbors: [None, None, None, None],
            },
        );
        queue.push_back((start.name.clone(), start.exits, droid.clone()));

        let start_room = start.name;

        while let Some((name, exits, droid)) = queue.pop_front() {
            for (i, _) in exits.into_iter().enumerate().filter(|(_, b)| *b) {
                let d = Direction::try_from(i)?;

                let mut next_droid = droid.clone();
                let next_pos = next_droid
                    .step(Some(Command::Move(d)))?
                    .room_information()
                    .context("Move failed")?;

                rooms.get_mut(&name).unwrap().neighbors[i] = Some(next_pos.name.clone());

                if next_pos.kicked_back {
                    goal = next_pos.name.clone();
                    gateway = name.clone();
                }

                if !rooms.contains_key(&next_pos.name) {
                    for item in &next_pos.items {
                        item_start_room.insert(item.clone(), next_pos.name.clone());
                    }
                    rooms.insert(
                        next_pos.name.clone(),
                        Room {
                            neighbors: [None, None, None, None],
                        },
                    );

                    if !next_pos.kicked_back {
                        // Don't explore further if we can't
                        queue.push_back((next_pos.name, next_pos.exits, next_droid));
                    }
                }
            }
        }

        Ok(Self {
            rooms,
            item_start_room,
            goal,
            gateway,
            start: start_room,
            droid_template: droid,
        })
    }

    fn path_to(&self, droid: &mut Droid, from: String, mut to: String) -> Result<()> {
        let mut prev: HashMap<&String, Option<Direction>> = HashMap::new();
        let mut queue = VecDeque::new();

        prev.insert(&from, None);
        queue.push_back(&from);

        while let Some(pos) = queue.pop_front() {
            if pos == &to {
                break;
            }

            for (i, r) in self.rooms[pos]
                .neighbors
                .iter()
                .enumerate()
                .filter_map(|(i, r)| Some((i, r.as_ref()?)))
            {
                let dir = Direction::try_from(i).unwrap();

                if !prev.contains_key(r) {
                    prev.insert(r, Some(dir));
                    queue.push_back(r);
                }
            }
        }

        let mut path = Vec::new();

        while let Some(&Some(dir)) = prev.get(&to) {
            path.push(dir);
            to = self.rooms[&to].neighbors[dir.opposite() as usize]
                .as_ref()
                .unwrap()
                .clone();
        }

        path.reverse();

        for dir in path {
            droid.step(Some(Command::Move(dir)))?;
        }

        Ok(())
    }

    fn identify_trap_items(&self) -> Result<Vec<String>> {
        let mut trapped = Vec::new();

        for (item, room) in &self.item_start_room {
            let mut droid = self.droid_template.clone();
            self.path_to(&mut droid, self.start.clone(), room.clone())?;

            if !matches!(
                droid.step(Some(Command::Take(item.clone())))?,
                CommandFeedback::ItemPickedUp
            ) {
                trapped.push(item.clone());
                continue;
            };

            let exit = Direction::try_from(
                self.rooms[room]
                    .neighbors
                    .iter()
                    .position(|o| o.is_some())
                    .context("Room doesn't have any exit")?,
            )
            .unwrap();

            if let Some(state) = droid.step(Some(Command::Move(exit)))?.room_information() {
                if &state.name != room {
                    continue;
                }
            }

            trapped.push(item.clone());
        }

        Ok(trapped)
    }

    fn solve(&self) -> Result<String> {
        let trapped = self.identify_trap_items()?;
        let safe_items = self
            .item_start_room
            .keys()
            .filter(|k| !trapped.contains(k))
            .cloned()
            .collect_vec();

        let count = safe_items.len();

        let mut heavy: Vec<u16> = Vec::new();
        let mut light: Vec<u16> = Vec::new();
        let mut subsets = (0u16..(1u16 << count)).collect_vec();
        subsets.sort_by_key(|s| s.count_ones());

        for set in subsets {
            if heavy.iter().any(|&s| set & s == s) {
                continue;
            }
            if light.iter().any(|&s| set & s == set) {
                continue;
            }

            let selected = OnesIterator(set).map(|i| &safe_items[i]);

            let mut droid = self.droid_template.clone();

            let mut pos = self.start.clone();
            for item in selected {
                let room = &self.item_start_room[item];
                self.path_to(&mut droid, pos, room.clone())?;
                pos = room.clone();
                droid.step(Some(Command::Take(item.clone())))?;
            }
            self.path_to(&mut droid, pos, self.gateway.clone())?;

            let dir = Direction::try_from(
                self.rooms[&self.gateway]
                    .neighbors
                    .iter()
                    .position(|r| r.as_ref() == Some(&self.goal))
                    .unwrap(),
            )
            .unwrap();

            match droid.step(Some(Command::Move(dir)))? {
                CommandFeedback::Finished(pass) => return Ok(pass),
                CommandFeedback::RoomInformation(CurrentRoomInformation {
                    pressure_diag: Some(diag),
                    ..
                }) => match diag {
                    Ordering::Less => heavy.push(set),
                    Ordering::Greater => light.push(set),
                    _ => {}
                },
                _ => {}
            }
        }

        Err(anyhow!("Failed to find the correct item combination"))
    }
}

pub async fn day25(input: String) -> Result<(String, String)> {
    let program: Intcode<251> = input.parse()?;

    let spaceship = Spaceship::explore(Droid { program })?;

    Ok((spaceship.solve()?.to_string(), "".to_string()))
}
