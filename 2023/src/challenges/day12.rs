use std::{
    collections::HashMap,
    fmt::Display,
    hash::Hash,
    sync::{Arc, OnceLock},
};

use anyhow::{anyhow, Context, Result};

use itertools::Itertools;
use parking_lot::RwLock;
use rayon::prelude::{IntoParallelIterator, ParallelIterator};
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

impl Display for Spring {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Damaged => write!(f, "#"),
            Operational => write!(f, "."),
            Unknown => write!(f, "?"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ResolvingError {
    UnfinishedGroup,
    UnfinishedGroupAtEnd,
    NotEnoughSprings,
    TooManyDamaged,
    NoMoreGroups,
    LeftoverGroups,
}

impl Display for ResolvingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TooManyDamaged => write!(f, "got more damaged springs than the group expected"),
            Self::NotEnoughSprings => write!(
                f,
                "the groups would need more springs than there is to complete"
            ),
            Self::UnfinishedGroup => write!(f, "can't end a group that wasn't finished (on .)"),
            Self::UnfinishedGroupAtEnd => {
                write!(f, "can't end a group that wasn't finished (at end)")
            }
            Self::NoMoreGroups => write!(f, "can't start group because there are no more"),
            Self::LeftoverGroups => write!(f, "got to the end with leftover groups"),
        }
    }
}

#[derive(Debug, Clone, Eq)]
enum Record {
    Resolved {
        springs: Vec<Spring>,
    },
    Unresolved {
        current_group: Option<u32>,
        // Groups in reverse order for efficient unshift
        groups: Vec<u32>,
        // Springs in reverse order for efficient unshift
        springs: Vec<(u32, Spring)>,
        // Resolved springs in order
        resolved: Vec<(u32, Spring)>,
    },
    Invalid(ResolvingError),
}

impl PartialEq for Record {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Resolved { springs } => match other {
                Self::Resolved { springs: others } => springs == others,
                _ => false,
            },
            Self::Invalid(err) => match other {
                Self::Invalid(others) => err == others,
                _ => false,
            },
            Self::Unresolved {
                current_group,
                groups,
                springs,
                ..
            } => match other {
                Self::Unresolved {
                    current_group: others_cg,
                    groups: others_gs,
                    springs: others_sprgs,
                    ..
                } => current_group == others_cg && groups == others_gs && springs == others_sprgs,
                _ => false,
            },
        }
    }
}

impl Hash for Record {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Self::Resolved { springs } => springs.hash(state),
            Self::Invalid(e) => e.hash(state),
            Self::Unresolved {
                current_group,
                groups,
                springs,
                ..
            } => {
                current_group.hash(state);
                groups.hash(state);
                springs.hash(state);
            }
        }
    }
}

impl Display for Record {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Invalid(err) => write!(f, "\x1b[1;31m[ invalid: {err} ]\x1b[0m"),
            Self::Resolved { springs } => {
                write!(f, "\x1b[32m[ ")?;
                for s in springs {
                    write!(f, "{s}")?;
                }
                write!(f, " ]\x1b[0m")
            }
            Self::Unresolved {
                groups,
                springs,
                resolved,
                current_group,
            } => {
                write!(f, "\x1b[2m[ ")?;
                if !springs.is_empty() {
                    for &(c, s) in springs.iter().rev() {
                        for _ in 0..c {
                            write!(f, "{s}")?;
                        }
                        write!(f, " ")?;
                    }
                } else {
                    write!(f, "ø")?;
                }
                write!(f, " | ")?;
                if let Some(last) = groups.first() {
                    if let Some(cur) = current_group {
                        write!(f, "+{cur},")?;
                    }

                    for g in groups[1..].iter().rev() {
                        write!(f, "{g},")?;
                    }

                    write!(f, "{last}")?;
                } else {
                    if let Some(cur) = current_group {
                        write!(f, "+{cur}")?;
                    } else {
                        write!(f, "ø")?;
                    }
                }
                write!(f, " ] -> [ ")?;
                for &(c, s) in resolved {
                    for _ in 0..c {
                        write!(f, "{s}")?;
                    }
                }
                write!(f, " ]\x1b[0m")
            }
        }
    }
}

static RESOLVE_MEMO: OnceLock<Arc<RwLock<HashMap<Record, usize>>>> = OnceLock::new();
fn get_resolve_memo() -> Arc<RwLock<HashMap<Record, usize>>> {
    RESOLVE_MEMO
        .get_or_init(|| Arc::new(RwLock::new(HashMap::new())))
        .clone()
}

impl Record {
    fn resolve(self) -> usize {
        let memo = get_resolve_memo();

        if let Some(&res) = memo.read().get(&self) {
            return res;
        }

        let key = self.clone();

        let mut nexts = Vec::new();
        match self {
            Self::Resolved { .. } => {
                memo.write().insert(self, 1);
                return 1;
            }
            Self::Invalid(_) => {}
            Self::Unresolved {
                mut groups,
                mut springs,
                mut resolved,
                mut current_group,
            } => {
                // Minimum number of springs needed to be valid, if all damaged springs are continuous and
                // separated by a single operational one (sum of each plus one for each separator).
                let min_springs =
                    (groups.iter().sum::<u32>() as usize + groups.len()).saturating_sub(1);
                let spring_count: usize = springs.iter().map(|&(c, _)| c as usize).sum();
                let s = springs.pop();

                match (s, current_group.as_mut()) {
                    // Fail fast, if we can't meet the quota no need to continue through iterations
                    // and branches
                    _ if min_springs > spring_count => {
                        nexts.push(Self::Invalid(ResolvingError::NotEnoughSprings));
                    }

                    // We have no more springs and no more groups, we have resolved the record
                    (None, None) if groups.is_empty() => nexts.push(Self::Resolved {
                        springs: resolved
                            .into_iter()
                            .flat_map(|(c, s)| std::iter::repeat(s).take(c as usize))
                            .collect(),
                    }),
                    // We have no more springs but we still have groups
                    (None, None) => nexts.push(Self::Invalid(ResolvingError::LeftoverGroups)),
                    // We have no more springs and the last group started has been finished, so we
                    // have resolved the record
                    (None, Some(0)) => nexts.push(Self::Resolved {
                        springs: resolved
                            .into_iter()
                            .flat_map(|(c, s)| std::iter::repeat(s).take(c as usize))
                            .collect(),
                    }),
                    // We have no more springs and the started group isn't finished, this record
                    // must be invalid
                    (None, Some(_)) => {
                        nexts.push(Self::Invalid(ResolvingError::UnfinishedGroupAtEnd))
                    }

                    // A group is started and we encounter some '#'s while we expect more
                    (Some((y, Damaged)), Some(x)) if *x >= y => {
                        *x -= y;
                        resolved.push((y, Damaged));
                        nexts.push(Self::Unresolved {
                            groups,
                            springs,
                            resolved,
                            current_group,
                        })
                    }
                    // Since the arm above didn't go through this means we have more '#' than the
                    // group expects
                    (Some((_, Damaged)), Some(_)) => {
                        nexts.push(Self::Invalid(ResolvingError::TooManyDamaged))
                    }
                    // We got '#', we don't have a started group
                    (Some((y, Damaged)), None) => match groups.last() {
                        // The next group expects enough
                        Some(&x) if x >= y => {
                            current_group = groups.pop().map(|x| x - y);
                            resolved.push((y, Damaged));
                            nexts.push(Self::Unresolved {
                                current_group,
                                groups,
                                springs,
                                resolved,
                            })
                        }
                        // We have more than the next group expects
                        Some(_) => nexts.push(Self::Invalid(ResolvingError::TooManyDamaged)),
                        // There is no next group
                        None => nexts.push(Self::Invalid(ResolvingError::NoMoreGroups)),
                    },

                    // We have '.' after finishing a group, this is normal, end the current group
                    // and continue
                    (Some((c, Operational)), Some(&mut 0)) => {
                        resolved.push((c, Operational));
                        nexts.push(Self::Unresolved {
                            groups,
                            springs,
                            resolved,
                            current_group: None,
                        })
                    }
                    // We have '.' but the current group hasn't been finished, this record must be
                    // invalid
                    (Some((_, Operational)), Some(_)) => {
                        nexts.push(Self::Invalid(ResolvingError::UnfinishedGroup))
                    }
                    // We have '.' but haven't started any group, this is also normal and we have
                    // nothing to do, continue
                    (Some((c, Operational)), None) => {
                        resolved.push((c, Operational));
                        nexts.push(Self::Unresolved {
                            groups,
                            springs,
                            resolved,
                            current_group,
                        })
                    }

                    // We got '?' while we had an unfinished group
                    (Some((y, Unknown)), Some(&mut x @ (1..))) => {
                        if x >= y {
                            resolved.push((y, Damaged));
                            nexts.push(Self::Unresolved {
                                current_group: Some(x - y),
                                groups,
                                springs,
                                resolved,
                            })
                        } else {
                            springs.push((y - x, Unknown));
                            resolved.push((x, Damaged));
                            nexts.push(Self::Unresolved {
                                current_group: Some(0),
                                groups,
                                springs,
                                resolved,
                            })
                        }
                    }

                    // We got '?' while we had a finished group, end it and leave the rest for
                    // later
                    (Some((c, Unknown)), Some(0)) => {
                        springs.push((c - 1, Unknown));
                        resolved.push((1, Operational));
                        nexts.push(Self::Unresolved {
                            current_group: None,
                            groups,
                            springs,
                            resolved,
                        })
                    }

                    // We got '?' while we didn't have a started group
                    (Some((c, Unknown)), None) => match groups.pop() {
                        // No group to start, only solution is for all of the '?' to become
                        // '.'
                        None => {
                            resolved.push((c, Operational));
                            nexts.push(Self::Unresolved {
                                current_group: None,
                                groups,
                                springs,
                                resolved,
                            })
                        }
                        // We have a group g
                        Some(g) => {
                            // We can choose to treat i '?' as '.' and the next '?' as a '#'
                            for i in 0..c {
                                let mut new_springs = springs.clone();
                                let mut new_resolved = resolved.clone();
                                // Add back the springs we didn't use
                                if c - i - 1 > 0 {
                                    new_springs.push((c - i - 1, Unknown));
                                }

                                new_resolved.push((i, Operational));
                                new_resolved.push((1, Damaged));
                                nexts.push(Self::Unresolved {
                                    // Already decrement since we also do the first '#'
                                    current_group: Some(g - 1),
                                    groups: groups.clone(),
                                    springs: new_springs,
                                    resolved: new_resolved,
                                });
                            }

                            // Or treat all '?' as '.'
                            // Add back the group we didn't consume
                            groups.push(g);
                            resolved.push((c, Operational));
                            nexts.push(Self::Unresolved {
                                current_group: None,
                                groups,
                                springs,
                                resolved,
                            })
                        }
                    },
                }
            }
        }
        let res = nexts.into_iter().map(Record::resolve).sum();
        memo.write().insert(key, res);
        res
    }

    fn unfold(self) -> Self {
        match self {
            Self::Resolved { .. } | Self::Invalid(_) => self,
            Self::Unresolved {
                current_group,
                groups,
                springs,
                resolved,
            } => Self::Unresolved {
                groups: std::iter::repeat_with(|| groups.iter().copied())
                    .take(5)
                    .flatten()
                    .collect(),
                springs: std::iter::repeat_with(|| {
                    std::iter::once((1, Unknown)).chain(springs.iter().copied())
                })
                .take(5)
                .flatten()
                .skip(1)
                .collect(),
                current_group,
                resolved,
            },
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
                .map(|(c, x)| (c as u32, x))
                .collect_vec();

            let groups: Vec<u32> = groups_str
                .split(',')
                .map(|x| x.parse())
                .rev()
                .try_collect()?;

            Ok(Record::Unresolved {
                springs,
                groups,
                resolved: Vec::new(),
                current_group: None,
            })
        })
        .try_collect()?;

    let solve = |records: Vec<Record>| {
        records
            .into_iter()
            .map(|r| r.resolve())
            .sum::<usize>()
    };

    let part1 = solve(records.clone());
    let part2 = solve(records.into_iter().map(Record::unfold).collect());

    Ok((part1.to_string(), part2.to_string()))
}
