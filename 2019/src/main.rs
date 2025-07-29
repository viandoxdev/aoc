use anyhow::Result;
use tracing_subscriber::prelude::*;

mod challenges;
mod intcode;
mod bigletters;

use challenges::*;
use rust_aoc::{AnyDay, Aoc, Day};

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "aoc2019=debug".into()),
        )
        .with(tracing_subscriber::fmt::layer())
        .init();

    let days: &[Box<dyn AnyDay>] = &[
        Day::new(1, day01),
        Day::new(2, day02),
        Day::new(3, day03),
        Day::new(4, day04),
        Day::new(5, day05),
        Day::new(6, day06),
        Day::new(7, day07),
        Day::new(8, day08),
        Day::new(9, day09),
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
    let days = &days[0..18];

    Aoc::from_path("../session", 2019)?.run(days).await;

    Ok(())
}
