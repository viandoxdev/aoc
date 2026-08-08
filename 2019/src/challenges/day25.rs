use std::{
    cmp::Ordering,
    collections::{HashMap, VecDeque},
    fmt::Display,
};

use anyhow::{Context, Result, anyhow};
use itertools::Itertools;

use crate::intcode::{Intcode, ProgramState};

const OUTPUT_LEN_CAP: usize = 1000;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Direction {
    West = 0,
    North = 1,
    East = 2,
    South = 3,
}

impl Direction {
    const ALL: [Direction; 4] = [
        Direction::West,
        Direction::North,
        Direction::East,
        Direction::South,
    ];

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

impl TryFrom<&str> for Direction {
    type Error = anyhow::Error;
    fn try_from(value: &str) -> Result<Self> {
        match value {
            "west" => Ok(Direction::West),
            "north" => Ok(Direction::North),
            "east" => Ok(Direction::East),
            "south" => Ok(Direction::South),
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
    Drop(String),
    Move(Direction),
}

impl Display for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Command::Take(s) => write!(f, "take {s}"),
            Command::Drop(s) => write!(f, "drop {s}"),
            Command::Move(s) => write!(f, "{s}"),
        }
    }
}

#[derive(Debug, Default)]
struct Room {
    neighbors: [Option<String>; 4],
}

impl Room {
    fn neighbor(&self, dir: Direction) -> Option<&str> {
        self.neighbors[dir as usize].as_deref()
    }

    fn exits(&self) -> impl Iterator<Item = (Direction, &str)> {
        Direction::ALL
            .into_iter()
            .filter_map(|d| Some((d, self.neighbor(d)?)))
    }

    fn exit_toward(&self, dest: &str) -> Option<Direction> {
        self.exits().find(|(_, n)| *n == dest).map(|(d, _)| d)
    }

    fn any_exit(&self) -> Option<Direction> {
        self.exits().next().map(|(d, _)| d)
    }
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

#[derive(Debug)]
enum CommandFeedback {
    Halted,
    ItemPickedUp,
    ItemDropped,
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

#[derive(Debug, Default)]
struct CurrentRoomInformation {
    name: String,
    items: Vec<String>,
    exits: [bool; 4],
    pressure_diag: Option<Ordering>,
    kicked_back: bool,
}

impl Droid {
    fn take(&mut self, item: &str) -> Result<CommandFeedback> {
        self.step(Some(Command::Take(item.to_string())))
    }

    fn drop_item(&mut self, item: &str) -> Result<CommandFeedback> {
        self.step(Some(Command::Drop(item.to_string())))
    }

    fn go(&mut self, dir: Direction) -> Result<CommandFeedback> {
        self.step(Some(Command::Move(dir)))
    }

    /// Pumps the Intcode VM until it awaits more input or finishes.
    /// Returns None if the output cap is exceeded (signals an infinite loop).
    fn run_to_input(&mut self, mut input: &[i64]) -> Result<Option<(bool, Vec<i64>)>> {
        let mut output = Vec::new();
        let finished = loop {
            if output.len() > OUTPUT_LEN_CAP {
                return Ok(None);
            }
            match self.program.step(input.first().copied())? {
                ProgramState::Running => {}
                ProgramState::Finished => break true,
                ProgramState::AwaitingInput => break false,
                ProgramState::ConsumedInput => input = &input[1..],
                ProgramState::PendingOutput(out) => output.push(out),
            }
        };
        Ok(Some((finished, output)))
    }

    fn parse_program_output(output: String) -> Result<CommandFeedback> {
        let mut inf = CurrentRoomInformation::default();
        let mut res = CommandFeedback::Other;

        #[derive(Clone, Copy)]
        enum State {
            None,
            Exits,
            Items,
        }
        let mut state = State::None;

        for line in output.lines() {
            if line.starts_with("== ") {
                if inf.name.is_empty() {
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
                inf.pressure_diag = Some(if line.contains("lighter") {
                    Ordering::Less
                } else {
                    Ordering::Greater
                });
            } else if line.contains("get in by typing") {
                res = CommandFeedback::Finished(
                    line.chars().filter(|c| c.is_ascii_digit()).collect(),
                );
                inf.name = String::new();
            } else if let Some(stripped) = line.strip_prefix("- ") {
                match state {
                    State::None => return Err(anyhow!("List item outside of item or exits scope")),
                    State::Exits => {
                        let dir = Direction::try_from(stripped)?;
                        inf.exits[dir as usize] = true;
                    }
                    State::Items => inf.items.push(stripped.to_string()),
                }
            } else if line.starts_with("You take") {
                res = CommandFeedback::ItemPickedUp;
            } else if line.starts_with("You drop") {
                res = CommandFeedback::ItemDropped;
            }
        }

        if !inf.name.is_empty() {
            Ok(CommandFeedback::RoomInformation(inf))
        } else {
            Ok(res)
        }
    }

    fn step(&mut self, command: Option<Command>) -> Result<CommandFeedback> {
        let input: Vec<i64> = match command {
            Some(c) => c
                .to_string()
                .bytes()
                .map(|b| b as i64)
                .chain(std::iter::once(10))
                .collect_vec(),
            None => Vec::new(),
        };

        let Some((finished, raw)) = self.run_to_input(&input)? else {
            return Ok(CommandFeedback::Halted);
        };

        let output: String = raw
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
        let mut rooms: HashMap<String, Room> = HashMap::new();
        let mut goal = String::new();
        let mut gateway = String::new();
        let mut item_start_room: HashMap<String, String> = HashMap::new();

        let start = droid
            .step(None)?
            .room_information()
            .context("No start room information")?;

        for item in &start.items {
            item_start_room.insert(item.clone(), start.name.clone());
        }

        rooms.insert(start.name.clone(), Room::default());
        queue.push_back((start.name.clone(), start.exits, droid.clone()));

        let start_room = start.name;

        while let Some((name, exits, droid)) = queue.pop_front() {
            for d in Direction::ALL.into_iter().filter(|d| exits[*d as usize]) {
                let mut next_droid = droid.clone();
                let next_pos = next_droid
                    .go(d)?
                    .room_information()
                    .context("Move failed")?;

                rooms.get_mut(&name).unwrap().neighbors[d as usize] = Some(next_pos.name.clone());

                if next_pos.kicked_back {
                    goal = next_pos.name.clone();
                    gateway = name.clone();
                }

                if !rooms.contains_key(&next_pos.name) {
                    for item in &next_pos.items {
                        item_start_room.insert(item.clone(), next_pos.name.clone());
                    }
                    rooms.insert(next_pos.name.clone(), Room::default());
                    if !next_pos.kicked_back {
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

    fn find_path(&self, from: String, mut to: String) -> Vec<Direction> {
        let mut prev: HashMap<&str, Option<Direction>> = HashMap::new();
        let mut queue: VecDeque<&str> = VecDeque::new();

        prev.insert(&from, None);
        queue.push_back(&from);

        while let Some(pos) = queue.pop_front() {
            if pos == to {
                break;
            }

            for (dir, neighbor) in self.rooms[pos].exits() {
                if !prev.contains_key(neighbor) {
                    prev.insert(neighbor, Some(dir));
                    queue.push_back(neighbor);
                }
            }
        }

        let mut path = Vec::new();

        while let Some(&Some(dir)) = prev.get(to.as_str()) {
            path.push(dir);
            to = self.rooms[&to].neighbor(dir.opposite()).unwrap().to_owned();
        }

        path.reverse();
        path
    }

    fn path_to(&self, droid: &mut Droid, from: &str, to: &str) -> Result<()> {
        for dir in self.find_path(from.to_string(), to.to_string()) {
            droid.go(dir)?;
        }

        Ok(())
    }

    fn is_trap_item(&self, item: &str) -> Result<bool> {
        let room = &self.item_start_room[item];

        let mut droid = self.droid_template.clone();

        self.path_to(&mut droid, &self.start, room)?;

        if !matches!(droid.take(item)?, CommandFeedback::ItemPickedUp) {
            return Ok(true);
        }

        let exit = self.rooms[room]
            .any_exit()
            .context("Room doesn't have any exit")?;
        let safe = droid
            .go(exit)?
            .room_information()
            .is_some_and(|s| &s.name != room);
        Ok(!safe)
    }

    fn identify_trap_items(&self) -> Result<Vec<String>> {
        self.item_start_room
            .keys()
            .filter_map(|item| match self.is_trap_item(item) {
                Ok(true) => Some(Ok(item.clone())),
                Ok(false) => None,
                Err(e) => Some(Err(e)),
            })
            .collect()
    }

    /// Clones the template droid, collects every safe item, and positions it at the gateway.
    /// Returns the droid and the direction from the gateway into the goal room.
    fn load_droid_at_gateway(&self, safe_items: &[&String]) -> Result<(Droid, Direction)> {
        let mut droid = self.droid_template.clone();
        let mut pos = self.start.as_str();

        for item in safe_items {
            let room = &self.item_start_room[*item];
            self.path_to(&mut droid, pos, room)?;
            pos = room;
            droid.take(item)?;
        }

        self.path_to(&mut droid, pos, &self.gateway)?;
        let dir = self.rooms[&self.gateway].exit_toward(&self.goal).unwrap();
        Ok((droid, dir))
    }

    fn solve(&self) -> Result<String> {
        let trapped = self.identify_trap_items()?;
        let safe_items: Vec<&String> = self
            .item_start_room
            .keys()
            .filter(|k| !trapped.contains(k))
            .collect();

        let count = safe_items.len();
        let (mut droid, dir) = self.load_droid_at_gateway(&safe_items)?;

        let mut subsets: Vec<u16> = (0u16..(1u16 << count)).collect();
        subsets.sort_by_key(|s| s.count_ones());

        let full = (1u16 << count) - 1;
        let mut current_set = full;
        let mut heavy: Vec<u16> = Vec::new();

        for set in subsets {
            #[allow(clippy::manual_contains)]
            if heavy.iter().any(|&s| set & s == s) {
                continue;
            }

            for i in OnesIterator(current_set & !set) {
                droid.drop_item(safe_items[i])?;
            }
            for i in OnesIterator(set & !current_set) {
                droid.take(safe_items[i])?;
            }
            current_set = set;

            match droid.go(dir)? {
                CommandFeedback::Finished(pass) => return Ok(pass),
                CommandFeedback::RoomInformation(CurrentRoomInformation {
                    pressure_diag: Some(Ordering::Less),
                    ..
                }) => heavy.push(set),
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
