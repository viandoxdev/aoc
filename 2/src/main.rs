use std::{error::Error, str::FromStr};
use reqwest::header::COOKIE;

// Requires in Cargo.toml
//
// [dependencies]
// reqwest = { version = "0.11.13", features = ["blocking", "cookies"]}

#[derive(Clone, Copy, Debug)]
enum Play {
    Rock,
    Paper,
    Scissors,
}

#[derive(Clone, Copy, Debug)]
enum Outcome {
    Win,
    Draw,
    Loss,
}

impl FromStr for Outcome {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "X" => Ok(Self::Loss),
            "Y" => Ok(Self::Draw),
            "Z" => Ok(Self::Win),
            _ => Err(()),
        }
    }
}

impl FromStr for Play {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "A" => Ok(Self::Rock),
            "B" => Ok(Self::Paper),
            "C" => Ok(Self::Scissors),
            "X" => Ok(Self::Rock),
            "Y" => Ok(Self::Paper),
            "Z" => Ok(Self::Scissors),
            _ => Err(())
        }
    }
}

impl Outcome {
    fn value(&self) -> u32 {
        match self {
            Self::Win => 6,
            Self::Draw => 3,
            Self::Loss => 0,
        }
    }
}

impl Play {
    fn value(&self) -> u32 {
        match self {
            Self::Rock => 1,
            Self::Paper => 2,
            Self::Scissors => 3,
        }
    }

    fn score(&self, opp: &Self) -> u32 {
        self.outcome(opp).value() + self.value()
    }

    fn outcome(&self, opp: &Self) -> Outcome {
        match self {
            Self::Rock => match opp {
                Self::Rock => Outcome::Draw,
                Self::Paper => Outcome::Loss,
                Self::Scissors => Outcome::Win,
            },
            Self::Paper => match opp {
                Self::Rock => Outcome::Win,
                Self::Paper => Outcome::Draw,
                Self::Scissors => Outcome::Loss,
            },
            Self::Scissors => match opp {
                Self::Rock => Outcome::Loss,
                Self::Paper => Outcome::Win,
                Self::Scissors => Outcome::Draw,
            }
        }
    }

    // Find the play needed to get a specific outcome from the opponent's play.
    fn play(opp: &Self, out: &Outcome) -> Play {
        match out {
            Outcome::Draw => *opp,
            Outcome::Win => match opp {
                Self::Rock => Self::Paper,
                Self::Paper => Self::Scissors,
                Self::Scissors => Self::Rock,
            }
            Outcome::Loss => match opp {
                Self::Rock => Self::Scissors,
                Self::Paper => Self::Rock,
                Self::Scissors => Self::Paper,
            }
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut session = std::fs::read_to_string("../session")?;
    session.pop(); // remove new line

    let client = reqwest::blocking::Client::new();
    let resp = client.get("https://adventofcode.com/2022/day/2/input")
        .header(COOKIE, format!("session={session}"))
        .send()?.text()?;

    // Part 1
    let mut score = 0;
    for line in resp.split("\n") {
        let mut iter = line.split_whitespace();
        let opp = iter.next();
        let per = iter.next();

        if let (Some(opp), Some(per)) = (opp, per) {
            let opp: Play = opp.parse().unwrap();
            let per: Play = per.parse().unwrap();

            score += per.score(&opp);
        }
    }

    println!("score (part1): {score}");

    // Part 2
    score = 0;
    for line in resp.split("\n") {
        let mut iter = line.split_whitespace();
        let opp = iter.next();
        let out = iter.next();

        if let (Some(opp), Some(out)) = (opp, out) {
            let opp: Play = opp.parse().unwrap();
            let out: Outcome = out.parse().unwrap();
            let per = Play::play(&opp, &out);

            score += per.value() + out.value();
        }
    }

    println!("score (part2): {score}");
    
    Ok(())
}

