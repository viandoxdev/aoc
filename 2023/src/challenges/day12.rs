use std::{
    hash::Hash,
    sync::{Arc, OnceLock}, hint::unreachable_unchecked,
};

use rustc_hash::FxHashMap as HashMap;

use anyhow::{anyhow, Context, Result};

use itertools::Itertools;
use parking_lot::RwLock;
use Spring::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Spring {
    // '#'
    Damaged,
    // '.'
    Operational,
    // '?'
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Record {
    current_group: Option<u8>,
    // Groups in reverse order for efficient unshift
    groups: Vec<u8>,
    // Springs in reverse order for efficient unshift
    springs: Vec<(u8, Spring)>,
}

static RESOLVE_MEMO: OnceLock<Arc<RwLock<HashMap<Record, u64>>>> = OnceLock::new();
fn get_resolve_memo() -> Arc<RwLock<HashMap<Record, u64>>> {
    RESOLVE_MEMO
        .get_or_init(|| {
            let mut map = HashMap::default();
            map.reserve(2usize.pow(19));
            Arc::new(RwLock::new(map))
        })
        .clone()
}

impl Record {
    fn resolve(self) -> u64 {
        enum Iteration {
            Next {
                source: usize,
                value: u64,
                expanded: bool,
                key: Record,
            },
            Finished {
                source: usize,
                value: u64,
            },
        }

        impl Iteration {
            fn add_to_value(&mut self, v: u64) {
                match self {
                    Self::Next { value, .. } => *value += v,
                    Self::Finished { value, .. } => *value += v,
                }
            }
            fn set_expanded(&mut self) {
                if let Self::Next { expanded, .. } = self {
                    *expanded = true;
                }
            }
        }

        let memo = get_resolve_memo();

        if let Some(&value) = memo.read().get(&self) {
            return value;
        }

        let mut work = vec![Iteration::Next {
            source: usize::MAX,
            value: 0,
            expanded: false,
            key: self,
        }];

        while let Some(last) = work.last() {
            let index = work.len() - 1;
            let key = match last {
                Iteration::Next {
                    key,
                    expanded: false,
                    ..
                } => key,
                Iteration::Next { expanded: true, .. } => {
                    let Some(Iteration::Next {
                        source, value, key, ..
                    }) = work.pop()
                    else {
                        unsafe { unreachable_unchecked() }
                    };

                    memo.write().insert(key, value);

                    if source == usize::MAX {
                        return value;
                    } else {
                        work[source].add_to_value(value);
                        continue;
                    }

                }
                &Iteration::Finished { source, value } => {
                    work.pop();
                    work[source].add_to_value(value);
                    continue;
                }
            };

            let Record {
                mut groups,
                mut springs,
                mut current_group,
            } = key.clone();

            work.last_mut().unwrap().set_expanded();

            macro_rules! nxt {
                ($groups:expr, $springs:expr, $cur:expr) => {{
                    let key = Record {
                        groups: $groups,
                        springs: $springs,
                        current_group: $cur,
                    };

                    if let Some(&value) = memo.read().get(&key) {
                        work.push(Iteration::Finished {
                            source: index,
                            value,
                        });
                    } else {
                        work.push(Iteration::Next {
                            source: index,
                            value: 0,
                            key,
                            expanded: false,
                        });
                    }
                }};
            }

            // Minimum number of springs needed to be valid, if all damaged springs are continuous and
            // separated by a single operational one (sum of each plus one for each separator).
            let min_springs = (groups.iter().sum::<u8>() as usize + groups.len()).saturating_sub(1);
            let spring_count: usize = springs.iter().map(|&(c, _)| c as usize).sum();
            let s = springs.pop();

            match (s, current_group.as_mut()) {
                // Fail fast, if we can't meet the quota no need to continue through iterations
                // and branches
                _ if min_springs > spring_count => {}

                // We have no more springs and no more groups, or we have no more springs and the
                // last group started has been finished, so we have resolved the record
                (None, None) | (None, Some(0)) if groups.is_empty() => {
                    work.push(Iteration::Finished {
                        source: index,
                        value: 1,
                    });
                }

                // We have no more springs but we still have groups
                (None, None) => {}
                // We have no more springs and the started group isn't finished, this record
                // must be invalid
                (None, Some(_)) => {}

                // A group is started and we encounter some '#'s while we expect more
                (Some((y, Damaged)), Some(x)) if *x >= y => {
                    *x -= y;
                    nxt!(groups, springs, current_group);
                }
                // Since the arm above didn't go through this means we have more '#' than the
                // group expects
                (Some((_, Damaged)), Some(_)) => {}
                // We got '#', we don't have a started group
                (Some((y, Damaged)), None) => match groups.last() {
                    // The next group expects enough
                    Some(&x) if x >= y => {
                        current_group = groups.pop().map(|x| x - y);
                        nxt!(groups, springs, current_group);
                    }
                    // We have more than the next group expects or there is no next group
                    Some(_) | None => {}
                },

                // We have '.' after finishing a group, this is normal, end the current group
                // and continue
                (Some((_, Operational)), Some(&mut 0)) => nxt!(groups, springs, None),
                // We have '.' but the current group hasn't been finished, this record must be
                // invalid
                (Some((_, Operational)), Some(_)) => {}
                // We have '.' but haven't started any group, this is also normal and we have
                // nothing to do, continue
                (Some((_, Operational)), None) => nxt!(groups, springs, current_group),

                // We got '?' while we had an unfinished group
                (Some((y, Unknown)), Some(&mut x @ (1..))) => {
                    if x >= y {
                        nxt!(groups, springs, Some(x - y));
                    } else {
                        springs.push((y - x, Unknown));
                        nxt!(groups, springs, Some(0));
                    }
                }

                // We got '?' while we had a finished group, end it and leave the rest for
                // later
                (Some((c, Unknown)), Some(0)) => {
                    springs.push((c - 1, Unknown));
                    nxt!(groups, springs, None);
                }

                // We got '?' while we didn't have a started group
                (Some((c, Unknown)), None) => match groups.pop() {
                    // No group to start, only solution is for all of the '?' to become
                    // '.'
                    None => nxt!(groups, springs, None),
                    // We have a group g
                    Some(g) => {
                        // We can choose to treat i '?' as '.' and the next '?' as a '#'
                        for i in 0..c {
                            let mut new_springs = springs.clone();
                            // Add back the springs we didn't use
                            if c - i - 1 > 0 {
                                new_springs.push((c - i - 1, Unknown));
                            }

                            // Already decrement since we also do the first '#'
                            nxt!(groups.clone(), new_springs, Some(g - 1));
                        }

                        // Or treat all '?' as '.'
                        // Add back the group we didn't consume
                        groups.push(g);
                        nxt!(groups, springs, None);
                    }
                },
            }
        }

        unsafe { unreachable_unchecked() }
    }

    fn unfold(self) -> Self {
        Self {
            groups: std::iter::repeat_with(|| self.groups.iter().copied())
                .take(5)
                .flatten()
                .collect(),
            springs: std::iter::repeat_with(|| {
                std::iter::once((1, Unknown)).chain(self.springs.iter().copied())
            })
            .take(5)
            .flatten()
            .skip(1)
            .collect(),
            ..self
        }
    }
}

pub async fn day12(input: String) -> Result<(String, String)> {
    let records: Vec<Record> = input
        .lines()
        .map(|l| -> Result<Record> {
            let (springs_str, groups_str) = l.split_once(' ').context("Malformed input")?;

            let springs: Vec<Spring> = springs_str
                .chars()
                .map(|c| match c {
                    '#' => Ok(Damaged),
                    '.' => Ok(Operational),
                    '?' => Ok(Unknown),
                    _ => Err(anyhow!("Unexpected character in input")),
                })
                .rev()
                .try_collect()?;
            let springs = springs
                .into_iter()
                .dedup_with_count()
                .map(|(c, x)| (c as u8, x))
                .collect_vec();

            let groups: Vec<u8> = groups_str
                .split(',')
                .map(|x| x.parse())
                .rev()
                .try_collect()?;

            Ok(Record {
                springs,
                groups,
                current_group: None,
            })
        })
        .try_collect()?;

    let solve = |records: Vec<Record>| records.into_iter().map(|r| r.resolve()).sum::<u64>();

    let part1 = solve(records.clone());
    let part2 = solve(records.into_iter().map(Record::unfold).collect());

    Ok((part1.to_string(), part2.to_string()))
}
