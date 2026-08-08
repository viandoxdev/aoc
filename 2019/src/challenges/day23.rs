use std::{collections::VecDeque, default, process::id};

use anyhow::Result;
use itertools::Itertools;

use crate::intcode::{Intcode, ProgramState};

#[derive(Debug, Clone, Copy, Default)]
struct Packet {
    address: usize,
    x: i64,
    y: i64,
}

const IDLE_DELAY: usize = 50;
const WAKEUP_DELAY: usize = 4;

struct Computer {
    address: usize,
    input_queue: VecDeque<i64>,
    program: Intcode<231>,
    finished: bool,
    idle: bool,
    empty_rec_streak: usize,
}

impl Computer {
    pub fn new(address: usize, program: Intcode<231>) -> Computer {
        Computer {
            address,
            input_queue: VecDeque::from(vec![address as i64]),
            program,
            finished: false,
            idle: false,
            empty_rec_streak: 0,
        }
    }

    pub fn push_packet(&mut self, packet: Packet) {
        self.input_queue.push_back(packet.x);
        self.input_queue.push_back(packet.y);
    }

    pub fn step(&mut self) -> Result<Option<Packet>> {
        if self.finished {
            return Ok(None);
        }

        let mut output = [0, 0, 0];
        let mut index = 0;
        loop {
            match self
                .program
                .step(Some(self.input_queue.front().copied().unwrap_or(-1)))?
            {
                ProgramState::Running => {}
                ProgramState::Finished => {
                    self.finished = true;
                    break;
                }
                ProgramState::AwaitingInput => unreachable!(),
                ProgramState::ConsumedInput => {
                    if self.input_queue.pop_front().is_none() {
                        self.empty_rec_streak += 1;

                        if self.empty_rec_streak > IDLE_DELAY {
                            self.idle = true;
                        }
                    } else {
                        self.empty_rec_streak = 0;
                        self.idle = false
                    }
                    break;
                }
                ProgramState::PendingOutput(out) => {
                    self.empty_rec_streak = 0;
                    self.idle = false;
                    output[index] = out;
                    index += 1;
                    if index == 3 {
                        return Ok(Some(Packet {
                            address: output[0] as usize,
                            x: output[1],
                            y: output[2],
                        }));
                    }
                }
            };
        }

        Ok(None)
    }
}

struct Network {
    computers: Vec<Computer>,
}

impl Network {
    pub fn new(computers: usize, nic: &Intcode<231>) -> Self {
        Self {
            computers: (0..computers)
                .map(|address| Computer::new(address, nic.clone()))
                .collect_vec(),
        }
    }

    pub fn run(&mut self) -> Result<(i64, i64)> {
        let mut part1 = 0;
        let mut packet_queue = Vec::new();
        let mut nat = Packet::default();
        let mut last_nat_y = 1234556;
        let mut wakeup = 0;

        loop {
            wakeup += 1;
            for computer in &mut self.computers {
                if let Some(packet) = computer.step()? {
                    packet_queue.push(packet);
                }
            }

            for packet in packet_queue.drain(..) {
                if packet.address == 255 {
                    if part1 == 0 {
                        part1 = packet.y;
                    }

                    nat = packet;

                    continue;
                }

                self.computers[packet.address].push_packet(packet);
            }

            if self.computers.iter().all(|c| c.idle) && wakeup > WAKEUP_DELAY {
                // Network is idle

                wakeup = 0;

                packet_queue.push(Packet { address: 0, ..nat });

                if last_nat_y == nat.y {
                    break;
                } else {
                    last_nat_y = nat.y
                }
            }
        }

        Ok((part1, last_nat_y))
    }
}

pub async fn day23(input: String) -> Result<(String, String)> {
    let nic: Intcode<231> = input.parse()?;

    let mut network = Network::new(50, &nic);

    let (part1, part2) = network.run()?;

    Ok((part1.to_string(), part2.to_string()))
}
