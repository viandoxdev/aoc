use std::io::ErrorKind;

use std::{
    pin::Pin,
    time::{Duration, Instant},
};

use anyhow::Result;
use std::future::Future;

use reqwest::Client;

const BASE_URL: &str = "https://adventofcode.com";

pub struct Aoc {
    year: u32,
    session: String,
    client: Client,
}

impl Aoc {
    pub fn from_path(session_file_path: impl AsRef<str>, year: u32) -> Result<Self> {
        let session_file = std::fs::read_to_string(session_file_path.as_ref())?;
        let session = session_file.trim_end();
        Ok(Self::new(session, year))
    }
    pub fn new(session: impl AsRef<str>, year: u32) -> Self {
        if let Err(e) = std::fs::create_dir_all("./inputs") {
            println!("Warning: Couldn't create inputs directory, this means no caching will work and the api will be spammed, which is not good. Error: {e}");
        }
        Self {
            year,
            session: session.as_ref().into(),
            client: Client::new(),
        }
    }

    async fn fetch_input(&self, day: u32) -> Result<String, reqwest::Error> {
        self.client
            .get(format!("{BASE_URL}/{}/day/{day}/input", self.year))
            .header("Cookie", format!("session={}", self.session))
            .send()
            .await?
            .text()
            .await
    }

    async fn fetch_and_cache(&self, day: u32) -> Result<String> {
        let input = self.fetch_input(day).await?;
        // Technically failing to cache isn't a problem for the runtime, so no need to return an
        // error, simply warn the user since this isn't ideal.
        if let Err(e) = std::fs::write(format!("./inputs/{day}"), &input) {
            eprintln!("{e}");
            eprintln!("Warning: Couldn't save input file, this can lead to spamming of the api and should be avoided to not get banned.");
        }
        Ok(input)
    }

    pub async fn get_input(&self, day: u32) -> Result<String> {
        match std::fs::read_to_string(format!("./inputs/{day}")) {
            Ok(input) => Ok(input),
            Err(err) => {
                if err.kind() == ErrorKind::NotFound {
                    Ok(self.fetch_and_cache(day).await?)
                } else {
                    Err(err.into())
                }
            }
        }
    }

    pub async fn run(&self, days: &[Box<dyn AnyDay>]) {
        let mut results = Vec::with_capacity(days.len());

        println!("AOC {}\n", self.year);

        let start = Instant::now();

        for day in days {
            results.push(day.run(self).await);
        }

        let total_runtime = Instant::now() - start;

        for res in &results {
            let day = res.day();
            println!("[Day {day}]");
            match res {
                DayResult::Success { part1, part2, .. } => {
                    if part2.is_empty() {
                        println!("  Part1: {part1}");
                    } else {
                        println!("  Part1: {part1}\n  Part2: {part2}");
                    }
                }
                DayResult::SolveError { error, .. } => {
                    println!("\x1b[31m  Error when solving:\n  {error}\x1b[0m");
                }
                DayResult::InputError { error, .. } => {
                    println!("\x1b[31m  Error when getting input:\n  {error}\x1b[0m");
                }
            }
        }

        println!("\nTimings\n");

        for res in &results {
            match res {
                DayResult::Success { day, runtime, .. } => {
                    println!("  Day {day}: {}", format_dur(*runtime))
                }
                DayResult::SolveError { day, runtime, .. } => println!(
                    "  Day {day}: {} \x1b[31;1m(failed)\x1b[0m",
                    format_dur(*runtime)
                ),
                DayResult::InputError { day, .. } => {
                    println!("  Day {day}: didn't run \x1b[31;1m(failed)\x1b[0m")
                }
            }
        }

        let sum_runtime: Duration = results.iter().map(DayResult::runtime).sum();

        println!(
            "\nSum: {}ms (sum of individual runtime)",
            sum_runtime.as_millis()
        );
        println!(
            "Total: {}ms (total runtime, including IO)",
            total_runtime.as_millis()
        );
        println!("\nDone");
    }
}

#[derive(Clone, Copy)]
pub struct Day<F> {
    day: u32,
    solve: F,
}

// Type system magic with async function, don't look too much into it

impl<F, Fut> Day<F>
where
    F: Fn(String) -> Fut,
    Fut: Future<Output = Result<(String, String)>>,
{
    pub fn new(day: u32, solve: F) -> Box<Self> {
        Box::new(Self { day, solve })
    }
}

// Since each Day<T> type will match a single type, we need a dyn object to store them in a
// collection, so we do just that
pub trait AnyDay {
    // Solve for the day, can't be async yet so returns a Pin<Box<dyn Future>>
    fn run<'a>(&'a self, aoc: &'a Aoc) -> Pin<Box<dyn Future<Output = DayResult> + 'a>>;
}

impl<F, Fut> AnyDay for Day<F>
where
    F: Fn(String) -> Fut,
    Fut: Future<Output = Result<(String, String)>>,
{
    fn run<'a>(&'a self, aoc: &'a Aoc) -> Pin<Box<dyn Future<Output = DayResult> + 'a>> {
        Box::pin(async move {
            let input = match aoc.get_input(self.day).await {
                Ok(input) => input,
                Err(e) => {
                    return DayResult::InputError {
                        day: self.day,
                        error: e,
                    }
                }
            };

            let start = Instant::now();
            let res = (self.solve)(input).await;
            let runtime = Instant::now() - start;
            let (part1, part2) = match res {
                Ok(res) => res,
                Err(e) => {
                    return DayResult::SolveError {
                        day: self.day,
                        error: e,
                        runtime,
                    }
                }
            };

            DayResult::Success {
                day: self.day,
                part1,
                part2,
                runtime,
            }
        })
    }
}

pub enum DayResult {
    InputError {
        day: u32,
        error: anyhow::Error,
    },
    SolveError {
        day: u32,
        error: anyhow::Error,
        runtime: Duration,
    },
    Success {
        day: u32,
        part1: String,
        part2: String,
        runtime: Duration,
    },
}

impl DayResult {
    fn day(&self) -> u32 {
        match self {
            Self::Success { day, .. } => *day,
            Self::SolveError { day, .. } => *day,
            Self::InputError { day, .. } => *day,
        }
    }

    fn runtime(&self) -> Duration {
        match self {
            Self::Success { runtime, .. } => *runtime,
            Self::SolveError { runtime, .. } => *runtime,
            Self::InputError { .. } => Duration::ZERO,
        }
    }
}

pub fn format_dur(dur: Duration) -> String {
    let nanos = dur.as_nanos();
    let micros = dur.as_micros();
    let millis = dur.as_millis();
    match nanos {
        0..=999 => format!("\x1b[92m{nanos}ns\x1b[0m"),
        1_000..=999_999 => format!("\x1b[92m{micros}µs\x1b[0m"),
        1_000_000..=999_999_999 => match millis {
            0..=15 => format!("\x1b[92m{millis}ms\x1b[0m"),
            16..=99 => format!("\x1b[93m{millis}ms\x1b[0m"),
            100.. => format!("\x1b[91m{millis}ms\x1b[0m"),
        },
        1_000_000_000.. => format!("\x1b[91m{}s\x1b[0m", (millis / 100) as f64 / 10.0),
    }
}
