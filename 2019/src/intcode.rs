use anyhow::{Result, anyhow};
use std::{
    io::{BufWriter, Write},
    str::FromStr,
};

#[derive(Debug, Clone, Copy)]
pub enum ProgramState {
    Running,
    Finished,
    AwaitingInput,
    ConsumedInput,
    PendingOutput(i64),
}

// Version: <day number><day part> (i.e: Day 5 part 2 is 52)
#[derive(Debug, Clone)]
pub struct Intcode<const VERSION: u8> {
    pub program: Vec<i64>,
    instruction_pointer: i64,
    base_pointer: i64,
}

pub fn parse_int_list(s: &str) -> Result<Vec<i64>, std::num::ParseIntError> {
    s.trim()
        .split(",")
        .map(str::parse::<i64>)
        .collect::<Result<Vec<i64>, std::num::ParseIntError>>()
}

impl<const VERSION: u8> FromStr for Intcode<VERSION> {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        let ints = parse_int_list(s)?;
        Ok(Self::new(ints))
    }
}

impl<const VERSION: u8> Intcode<VERSION> {
    pub fn into_version<const NEW_VERSION: u8>(self) -> Intcode<NEW_VERSION> {
        Intcode {
            program: self.program,
            instruction_pointer: self.instruction_pointer,
            base_pointer: self.base_pointer,
        }
    }

    pub fn new(program: Vec<i64>) -> Self {
        Self {
            program,
            instruction_pointer: 0,
            base_pointer: 0,
        }
    }

    fn mode(ins: i64, arg: u8) -> u8 {
        if VERSION < 51 {
            0
        } else {
            (ins as u64 / 10u64.pow(arg as u32 + 1) % 10) as u8
        }
    }

    fn param_disassemble(&self, ip: usize, ins: i64, off: usize) -> String {
        match Self::mode(ins, off as u8) {
            // Position mode
            0 => format!("[{}]", self.program[ip + off]),
            // Immediate mode
            1 => format!("{}", self.program[ip + off]),
            // Relative mode
            2 if VERSION >= 91 => format!("[rb + {}]", self.program[ip + off]),
            unk => format!("[{unk}? {}]", self.program[ip + off]),
        }
    }

    fn param_read(&self, ins: i64, off: usize) -> Result<i64> {
        let ip = self.instruction_pointer as usize;
        let bp = self.base_pointer;

        if VERSION >= 91 {
            match Self::mode(ins, off as u8) {
                // Position mode
                0 => Ok(self
                    .program
                    .get(self.program.get(ip + off).copied().unwrap_or(0) as usize)
                    .copied()
                    .unwrap_or(0)),
                // Immediate mode
                1 => Ok(self.program.get(ip + off).copied().unwrap_or(0)),
                // Relative mode
                2 => Ok(self
                    .program
                    .get((bp + self.program.get(ip + off).copied().unwrap_or(0)) as usize)
                    .copied()
                    .unwrap_or(0)),
                unk => Err(anyhow!(
                    "Unknown parameter mode {unk} (VERSION = {VERSION})"
                )),
            }
        } else {
            // VERSION < 91
            match Self::mode(ins, off as u8) {
                // Position mode
                0 => Ok(self.program[self.program[ip + off] as usize]),
                // Immediate mode
                1 => Ok(self.program[ip + off]),
                unk => Err(anyhow!(
                    "Unknown parameter mode {unk} (VERSION = {VERSION})"
                )),
            }
        }
    }

    fn param_write(&mut self, ins: i64, off: usize, value: i64) -> Result<()> {
        let addr = self
            .program
            .get(self.instruction_pointer as usize + off)
            .copied()
            .unwrap_or(0);
        let index = match Self::mode(ins, off as u8) {
            0 => addr as usize,
            1 => return Err(anyhow!("Can't write to immediate")),
            2 if VERSION >= 91 => (self.base_pointer + addr) as usize,
            unk => {
                return Err(anyhow!(
                    "Unknown parameter mode {unk} (VERSION = {VERSION})"
                ));
            }
        };

        if index >= self.program.len() && VERSION >= 91 {
            self.program.resize(index + 1, 0);
        }
        self.program[index] = value;
        Ok(())
    }

    // Execute one instruction
    pub fn step(&mut self, mut input: Option<i64>) -> Result<ProgramState> {
        let has_input = input.is_some();
        let mut output = None;
        let mem = &mut self.program;
        let ins = mem[self.instruction_pointer as usize];
        let op = if VERSION < 51 { ins } else { ins % 100 };

        macro_rules! p {
            ([$off:expr] <- $val:expr) => {
                self.param_write(ins, $off, $val)?
            };
            ($off:expr) => {
                self.param_read(ins, $off)?
            };
        }

        let (ip_offset, bp_offset) = match op {
            // Add: [3] <- [1] + [2]
            1 => {
                p! { [3] <- p![1] + p![2] };
                (4, 0)
            }
            // Mul: [3] <- [1] * [2]
            2 => {
                p! { [3] <- p![1] * p![2] };
                (4, 0)
            }
            // Input: [1] <- (input)
            3 if VERSION >= 51 => {
                if let Some(input) = input.take() {
                    p! { [1] <- input };
                    (2, 0)
                } else {
                    return Ok(ProgramState::AwaitingInput);
                }
            }
            // Output: (output) <- [1]
            4 if VERSION >= 51 => {
                output = Some(p![1]);
                (2, 0)
            }
            // Jump if true:
            //   IF [1] != 0 THEN
            //     (ip) <- [2]
            //   END
            5 if VERSION >= 52 => {
                if p![1] != 0 {
                    (p![2] - self.instruction_pointer, 0)
                } else {
                    (3, 0)
                }
            }
            // Jump if false:
            //   IF [1] == 0 THEN
            //     (ip) <- [2]
            //   END
            6 if VERSION >= 52 => {
                if p![1] == 0 {
                    (p![2] - self.instruction_pointer, 0)
                } else {
                    (3, 0)
                }
            }
            // Less than: [3] <- IF [1] < [2] THEN 1 ELSE 0
            7 if VERSION >= 52 => {
                p! { [3] <-  (p![1] < p![2]) as i64 };
                (4, 0)
            }
            // Equals: [3] <- IF [1] = [2] THEN 1 ELSE 0
            8 if VERSION >= 52 => {
                p! { [3] <- (p![1] == p![2]) as i64 };
                (4, 0)
            }
            // Adjust relative base: (bp) <- (bp) + [1]
            9 if VERSION >= 91 => (2, p![1]),
            // Halt
            99 => return Ok(ProgramState::Finished),

            unk => {
                return Err(anyhow!("Unknown opcode {unk} (VERSION = {VERSION})"));
            }
        };

        self.instruction_pointer += ip_offset;
        self.base_pointer += bp_offset;

        if let Some(output) = output {
            Ok(ProgramState::PendingOutput(output))
        } else if let None = input
            && has_input
        {
            Ok(ProgramState::ConsumedInput)
        } else {
            Ok(ProgramState::Running)
        }
    }

    pub fn disassemble(&self, colors: bool) -> Result<String> {
        let mut writer = BufWriter::new(Vec::new());

        let (s_ins, s_param, s_addr, s_ints, s_reset) = if colors {
            ("\x1b[1m", "\x1b[32m", "\x1b[31m", "\x1b[0;2m", "\x1b[0m")
        } else {
            ("", "", "", "", "")
        };

        let mut ip = 0;
        while ip < self.program.len() {
            let ins = self.program[ip];
            let op = if VERSION < 51 { ins } else { ins % 100 };

            macro_rules! p {
                ($off:literal) => {
                    self.param_disassemble(ip, ins, $off)
                };
            }

            write!(writer, "{s_addr}{ip:04} ")?;

            let ip_offset = match op {
                1 => {
                    write!(
                        writer,
                        "{s_ins}add {s_param}{}{s_reset} <- {s_param}{}{s_reset} + {s_param}{}",
                        p![3],
                        p![1],
                        p![2]
                    )?;
                    4
                }
                2 => {
                    write!(
                        writer,
                        "{s_ins}mul {s_param}{}{s_reset} <- {s_param}{}{s_reset} * {s_param}{}",
                        p![3],
                        p![1],
                        p![2]
                    )?;
                    4
                }
                3 if VERSION >= 51 => {
                    write!(writer, "{s_ins}inp {s_param}{}", p![1])?;
                    2
                }
                4 if VERSION >= 51 => {
                    write!(writer, "{s_ins}out {s_param}{}", p![1])?;
                    2
                }
                5 if VERSION >= 52 => {
                    write!(
                        writer,
                        "{s_ins}jmp {s_reset}if {s_param}{}{s_reset} != 0 to {s_param}{}",
                        p![1],
                        p![2]
                    )?;
                    3
                }
                6 if VERSION >= 52 => {
                    write!(
                        writer,
                        "{s_ins}jmp {s_reset}if {s_param}{}{s_reset} == 0 to {s_param}{}",
                        p![1],
                        p![2]
                    )?;
                    3
                }
                7 if VERSION >= 52 => {
                    write!(
                        writer,
                        "{s_ins}cmp {s_param}{}{s_reset} <- {s_param}{}{s_reset} < {s_param}{}",
                        p![3],
                        p![1],
                        p![2]
                    )?;
                    4
                }
                8 if VERSION >= 52 => {
                    write!(
                        writer,
                        "{s_ins}cmp  {s_param}{}{s_reset} <- {s_param}{}{s_reset} == {s_param}{}",
                        p![3],
                        p![1],
                        p![2]
                    )?;
                    4
                }
                // Adjust relative base: (bp) <- (bp) + [1]
                9 if VERSION >= 91 => {
                    write!(writer, "{s_ins}arb {s_reset}(bp) <- {s_param}{}", p![1])?;
                    2
                }
                99 => {
                    write!(writer, "{s_ins}halt")?;
                    1
                }
                _ => 1,
            };

            write!(writer, " {s_ints}[")?;
            for off in 0..ip_offset {
                write!(writer, "{}", self.program[ip + off])?;
                if off < ip_offset - 1 {
                    write!(writer, ",")?;
                }
            }
            writeln!(writer, "]{s_reset}")?;

            ip += ip_offset;
        }

        Ok(String::from_utf8(writer.into_inner()?)?)
    }

    // Run the program until input is fully consumed (discards output)
    pub fn feed_input(&mut self, mut input: &[i64]) -> Result<bool> {
        while !input.is_empty() {
            match self.step(input.first().copied())? {
                ProgramState::Finished => return Ok(false),
                ProgramState::ConsumedInput => input = &input[1..],
                _ => {}
            }
        }

        Ok(true)
    }

    pub fn run_until_output(&mut self, mut input: &[i64]) -> Result<Option<i64>> {
        loop {
            match self.step(input.first().copied())? {
                ProgramState::Running => {}
                ProgramState::Finished => return Ok(None),
                ProgramState::AwaitingInput => return Err(anyhow!("Missing input")),
                ProgramState::ConsumedInput => input = &input[1..],
                ProgramState::PendingOutput(output) => return Ok(Some(output)),
            }
        }
    }

    pub fn run_until_array_output<const N: usize>(
        &mut self,
        mut input: &[i64],
    ) -> Result<Option<[i64; N]>> {
        let mut output = [0; N];
        let mut index = 0usize;
        loop {
            match self.step(input.first().copied())? {
                ProgramState::Running => {}
                ProgramState::Finished => return Ok(None),
                ProgramState::AwaitingInput => return Err(anyhow!("Missing input")),
                ProgramState::ConsumedInput => input = &input[1..],
                ProgramState::PendingOutput(out) => {
                    output[index] = out;
                    index += 1;
                    if index == N {
                        return Ok(Some(output));
                    }
                }
            }
        }
    }

    // Run program until halts
    pub fn run(&mut self, mut input: &[i64]) -> Result<Vec<i64>> {
        let mut output = Vec::new();

        loop {
            match self.step(input.first().copied())? {
                ProgramState::Running => {}
                ProgramState::Finished => return Ok(output),
                ProgramState::AwaitingInput => return Err(anyhow!("Missing input")),
                ProgramState::ConsumedInput => input = &input[1..],
                ProgramState::PendingOutput(out) => output.push(out),
            }
        }
    }
}
