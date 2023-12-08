use std::{
    pin::Pin,
    time::{Duration, Instant},
};

use anyhow::Result;
use chrono::{FixedOffset, Utc, TimeZone, Datelike};
use std::future::Future;
use tracing_subscriber::prelude::*;

use aoc::Aoc;
use challenges::*;

mod utils;
mod aoc;
mod challenges;

#[derive(Clone, Copy)]
struct Day<F> {
    day: u32,
    solve: F,
}

// Type system magic with async function, don't look too much into it

impl<F, Fut> Day<F>
where
    F: Fn(String) -> Fut,
    Fut: Future<Output = Result<(String, String)>>,
{
    fn new(day: u32, solve: F) -> Box<Self> {
        Box::new(Self { day, solve })
    }
}

// Since each Day<T> type will match a single type, we need a dyn object to store them in a
// collection, so we do just that
trait AnyDay {
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

enum DayResult {
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

fn format_dur(dur: Duration) -> String {
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

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "aoc2023=debug".into()),
        )
        .with(tracing_subscriber::fmt::layer())
        .init();

    let days: &[Box<dyn AnyDay>] = &[
        Day::new(01, day1),
        Day::new(02, day2),
        Day::new(03, day3),
        Day::new(04, day4),
        Day::new(05, day5),
        Day::new(06, day6),
        Day::new(07, day7),
        Day::new(08, day8),
        Day::new(09, day9),
        Day::new(10, day10),
        Day::new(11, day11),
        Day::new(12, day12),
        Day::new(13, day13),
        Day::new(14, day14),
        Day::new(15, day15),
        Day::new(16, day16),
        Day::new(17, day17),
        Day::new(18, day18),
        Day::new(19, day19),
        Day::new(20, day20),
        Day::new(21, day21),
        Day::new(22, day22),
        Day::new(23, day23),
        Day::new(24, day24),
        Day::new(25, day25),
    ];
    
    let tz = FixedOffset::west_opt(5 * 3600).unwrap();
    let time = Utc::now().with_timezone(&tz);
    let start = tz.with_ymd_and_hms(2023, 12, 1, 0, 0, 0).unwrap();
    let end = tz.with_ymd_and_hms(2023, 12, 25, 0, 0, 0).unwrap();
    let released: usize = if time < start { 0 } else if time > end { 25 } else { time.day() as usize };

    let days = &days[0..released];

    let session_file = std::fs::read_to_string("../session")?;
    let session = session_file.trim_end();
    let aoc = Aoc::new(session);
    let mut results = Vec::with_capacity(days.len());

    println!("Aoc 2023\n");

    let start = Instant::now();

    for day in days {
        results.push(day.run(&aoc).await);
    }

    let total_runtime = Instant::now() - start;

    for res in &results {
        let day = res.day();
        println!("[Day {day}]");
        match res {
            DayResult::Success { part1, part2, .. } => {
                println!("  Part1: {part1}\n  Part2: {part2}");
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

    Ok(())
}
