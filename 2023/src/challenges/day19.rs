use std::{cmp::Ordering, collections::HashMap, convert::Infallible, ops::Range, str::FromStr};

use anyhow::{anyhow, Context, Error, Result};

use itertools::Itertools;
use Condition::*;
use Continuation::*;

/// A rating (x, m, a, or s)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Rating {
    X,
    M,
    A,
    S,
}

impl FromStr for Rating {
    type Err = Error;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "x" => Ok(Rating::X),
            "m" => Ok(Rating::M),
            "a" => Ok(Rating::A),
            "s" => Ok(Rating::S),
            _ => Err(anyhow!("Malformed rating")),
        }
    }
}

/// A condition, >, < or default
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Condition {
    Default,
    Comparison(Rating, u64, Ordering),
}

impl FromStr for Condition {
    type Err = Error;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let rating: Rating = s[0..1].parse()?;
        let val: u64 = s[2..].parse()?;
        let cmp: Ordering = match &s[1..2] {
            ">" => Ok(Ordering::Greater),
            "<" => Ok(Ordering::Less),
            _ => Err(anyhow!("Malformed ordering")),
        }?;

        Ok(Self::Comparison(rating, val, cmp))
    }
}

/// A continuation, what to do after a condition has been verrified
#[derive(Debug, Clone, PartialEq, Eq)]
enum Continuation {
    Accepted,
    Rejected,
    Next(String),
}

impl FromStr for Continuation {
    type Err = Infallible;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "A" => Ok(Accepted),
            "R" => Ok(Rejected),
            _ => Ok(Next(s.to_string())),
        }
    }
}

/// A rule, a condition and a continuation
#[derive(Debug, Clone, PartialEq, Eq)]
struct Rule {
    condition: Condition,
    next: Continuation,
}

impl FromStr for Rule {
    type Err = Error;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        if let Some((cond_str, next_str)) = s.split_once(':') {
            let condition = cond_str.parse()?;
            let next = next_str.parse()?;
            Ok(Self { condition, next })
        } else {
            Ok(Self {
                condition: Default,
                next: s.parse()?,
            })
        }
    }
}

impl Rule {
    /// Wether the part abides by the rule
    fn passes(&self, part: &Part) -> bool {
        match self.condition {
            Default => true,
            Comparison(rating, val, cmp) => part.get(rating).cmp(&val) == cmp,
        }
    }

    /// Take a PartRange and returns the contained range passing the rule, and the one failing
    fn run_range(&self, part: PartRange) -> (PartRange, PartRange) {
        match self.condition {
            Default => (part, PartRange::empty()),
            Comparison(rating, val, cmp) => {
                let range = part.get(rating);

                match cmp {
                    Ordering::Greater => {
                        if range.end - 1 <= val {
                            // We have a condition that fails for any value in the range:
                            // the rating must be greater than the greatest value the range can take
                            (PartRange::empty(), part)
                        } else if val < range.start {
                            // We have a condition that will pass for any value in the range:
                            // the rating must be greater than less than the smallest value the
                            // range can take
                            (part, PartRange::empty())
                        } else {
                            let mut fails = part.clone();
                            let mut passes = part;

                            passes.get_mut(rating).start = val + 1;
                            fails.get_mut(rating).end = val + 1;

                            (passes, fails)
                        }
                    }
                    Ordering::Less => {
                        if val <= range.start {
                            // We have a condition that fails for any value in the range:
                            // the rating must be less than the smallest value the range can take
                            (PartRange::empty(), part)
                        } else if val >= range.end {
                            // We have a condition that will pass for any value in the range:
                            // the rating must be less than more than the greatest value the
                            // range can take
                            (part, PartRange::empty())
                        } else {
                            let mut fails = part.clone();
                            let mut passes = part;

                            passes.get_mut(rating).end = val;
                            fails.get_mut(rating).start = val;

                            (passes, fails)
                        }
                    }
                    Ordering::Equal => unreachable!(),
                }
            }
        }
    }
}

/// A workflow, a named set of rules
#[derive(Debug, Clone)]
struct Workflow {
    name: String,
    rules: Vec<Rule>,
}

impl FromStr for Workflow {
    type Err = Error;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let (name, rules) = s.split_once('{').context("Malformed workflow")?;
        let name = name.to_string();
        let rules = &rules[..rules.len() - 1];
        let rules = rules.split(',').map(Rule::from_str).try_collect()?;

        Ok(Self { name, rules })
    }
}

impl Workflow {
    /// Run the workflow on a particular part
    fn run(&self, part: &Part) -> &Continuation {
        &self.rules.iter().find(|r| r.passes(part)).unwrap().next
    }

    /// Run the workflow on a part range, returning the different branches that could be taken from
    /// the values of the range
    fn run_range(&self, part: &PartRange) -> impl Iterator<Item = (PartRange, &Continuation)> {
        self.rules.iter().scan(part.clone(), |last_failed, r| {
            let (passed, failed) = r.run_range(last_failed.clone());
            *last_failed = failed;
            Some((passed, &r.next))
        })
    }
}

/// A part with its rating numbers
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Part {
    x: u64,
    m: u64,
    a: u64,
    s: u64,
}

impl Part {
    /// Get the value of the rating for the part
    fn get(self, rating: Rating) -> u64 {
        match rating {
            Rating::X => self.x,
            Rating::M => self.m,
            Rating::A => self.a,
            Rating::S => self.s,
        }
    }

    /// Get the sum of ratings
    fn sum(self) -> u64 {
        self.x + self.m + self.a + self.s
    }
}

impl FromStr for Part {
    type Err = Error;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut iter = s[1..s.len() - 1].split(',').map(|f| f[2..].parse::<u64>());
        let mut next = || match iter.next() {
            None => Result::<u64, Error>::Err(anyhow!("Expected more fields")),
            Some(r) => Ok(r?),
        };

        Ok(Self {
            x: next()?,
            m: next()?,
            a: next()?,
            s: next()?,
        })
    }
}

/// A part range, defining a set of parts
#[derive(Debug, Clone)]
struct PartRange {
    x: Range<u64>,
    m: Range<u64>,
    a: Range<u64>,
    s: Range<u64>,
}

impl PartRange {
    /// Number of different possible parts in the range
    fn combinations(&self) -> u64 {
        (self.x.end - self.x.start)
            * (self.m.end - self.m.start)
            * (self.a.end - self.a.start)
            * (self.s.end - self.s.start)
    }
    /// The full range: all parts
    fn full() -> Self {
        Self {
            x: 1..4001,
            m: 1..4001,
            a: 1..4001,
            s: 1..4001,
        }
    }
    /// The empty range: no parts
    fn empty() -> Self {
        Self {
            x: 0..0,
            m: 0..0,
            a: 0..0,
            s: 0..0,
        }
    }
    /// Get a specific rating range
    fn get(&self, rating: Rating) -> &Range<u64> {
        match rating {
            Rating::X => &self.x,
            Rating::M => &self.m,
            Rating::A => &self.a,
            Rating::S => &self.s,
        }
    }
    /// Get a specific rating range mutably
    fn get_mut(&mut self, rating: Rating) -> &mut Range<u64> {
        match rating {
            Rating::X => &mut self.x,
            Rating::M => &mut self.m,
            Rating::A => &mut self.a,
            Rating::S => &mut self.s,
        }
    }
}

pub async fn day19(input: String) -> Result<(String, String)> {
    // Parse
    let (workflows, parts) = input.split_once("\n\n").context("Malformed input")?;
    let workflows: HashMap<String, Workflow> = workflows
        .lines()
        .map(|s| Workflow::from_str(s).map(|w| (w.name.clone(), w)))
        .try_collect()?;
    let parts: Vec<Part> = parts.lines().map(Part::from_str).try_collect()?;

    let part1 = {
        // Run each parts through the workflows and figure out if they get accepted
        let mut total = 0;
        for part in &parts {
            let mut cur = "in";
            loop {
                match workflows[cur].run(part) {
                    Accepted => {
                        // If they do add their ratings to the sum
                        total += part.sum();
                        break;
                    }
                    Rejected => {
                        break;
                    }
                    Next(s) => {
                        cur = s;
                    }
                }
            }
        }
        total
    };
    let part2 = {
        // Get the ranges of the parts that are accepted
        // Start with all the parts in a single range
        let in_cont = Next("in".to_string());
        let mut parts = vec![(PartRange::full(), &in_cont)];
        let mut next_parts = Vec::new();

        let mut sum = 0;
        while !parts.is_empty() {
            for (part, cont) in parts.drain(..) {
                match cont {
                    Next(n) => {
                        // Most the range will get expended by the different branches that can be
                        // taken depending on the conditions.
                        next_parts.extend(workflows[n].run_range(&part));
                    }
                    Accepted => {
                        // If the range is accepted, we can add the number of possible parts to the
                        // sum
                        sum += part.combinations();
                    }
                    Rejected => {
                        // If the range is rejected we don't count it
                    }
                }
            }

            std::mem::swap(&mut parts, &mut next_parts);
        }
        sum
    };

    Ok((part1.to_string(), part2.to_string()))
}
