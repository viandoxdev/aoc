use std::{
    collections::VecDeque,
    fmt::Display,
    mem::MaybeUninit,
    ops::{Deref, Range},
    str::FromStr,
    sync::Arc,
};

use anyhow::{anyhow, Context, Error, Result};
use itertools::Itertools;
use num::Integer;
use rustc_hash::{FxHashMap, FxHashSet};

struct DispDests<'a>(&'a [ModuleKey]);
struct DispLow(bool);

impl<'a> Display for DispDests<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        let mut a = false;
        for &ModuleKey { index, number } in self.0 {
            if a {
                write!(f, ",")?;
            }
            a = true;
            write!(f, "{index}.{number}")?;
        }
        write!(f, "]")
    }
}

impl Display for DispLow {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 {
            write!(f, "low")
        } else {
            write!(f, "high")
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct ModuleKey {
    /// Index of the module
    index: usize,
    /// What the module knows us as
    number: usize,
}

impl ModuleKey {
    fn new(index: usize, number: usize) -> Self {
        Self { index, number }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ModuleKind {
    Broadcaster,
    FlipFlop,
    Conjunction,
    Inverter,
    Untyped,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ModuleState {
    NoState,
    FlipFlop(bool),
    Conjunction(Box<[bool]>),
}

#[derive(Debug, Clone, Copy)]
struct Module {
    index: usize,
    name: &'static str,
    dests: &'static [ModuleKey],
    sources: &'static [usize],
    kind: ModuleKind,
}

#[derive(Debug, Clone, Copy)]
pub struct Signal {
    low: bool,
    to: ModuleKey,
}

impl Signal {
    fn new(low: bool, to: ModuleKey) -> Self {
        Self { low, to }
    }
}

#[derive(Clone)]
struct State {
    modules: Arc<[Module]>,
    modules_state: Box<[ModuleState]>,
    broadcast: ModuleKey,
}

type StateMask = [bool];

impl FromStr for State {
    type Err = Error;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut modules_str: Vec<(&str, &str, &str)> = s
            .lines()
            .map(|l| -> Result<(&str, &str, &str)> {
                let (mod_str, dests) = l.split_once(" -> ").context("Malformed module")?;
                let kind = &mod_str[0..1];
                let name = &mod_str[1..];
                Ok((kind, name, dests))
            })
            .try_collect()?;

        // String interning / name resolution
        let all_names = modules_str
            .iter()
            .flat_map(|&(_, name, dests_str)| std::iter::once(name).chain(dests_str.split(", ")));
        let mut name_table = FxHashMap::default();
        let typed_names = modules_str
            .iter()
            .map(|&(_, name, _)| name)
            .collect::<FxHashSet<_>>();
        for name in all_names {
            if !name_table.contains_key(name) {
                let index = name_table.len();
                name_table.insert(name.to_string(), index);
            }
        }
        // Add fake entries to input to handle untyped modules
        for (untyped_name, _) in name_table
            .iter()
            .filter(|&(k, _)| !typed_names.contains(k.as_str()))
        {
            modules_str.push(("", untyped_name.as_str(), ""));
        }

        // Ensures that the index in modules_str is the same as the index in other arrays
        modules_str.sort_by_key(|&(_, name, _)| name_table[name]);

        let module_count = modules_str.len();

        fn create_vec_of_len<T, F: FnMut() -> T>(len: usize, f: F) -> Vec<T> {
            std::iter::repeat_with(f).take(len).collect_vec()
        }

        fn create_boxed_slice_uninit<T>(len: usize) -> Box<[MaybeUninit<T>]> {
            create_vec_of_len(len, MaybeUninit::uninit).into_boxed_slice()
        }

        let mut sources = create_vec_of_len(module_count, Vec::new);

        let mut modules = create_boxed_slice_uninit::<Module>(module_count);
        let mut modules_state = create_boxed_slice_uninit::<ModuleState>(module_count);

        let mut dests_slices = create_vec_of_len(module_count, || 0..0);
        let mut dests_store = Vec::new();

        let mut sources_slices = create_vec_of_len(module_count, || 0..0);
        let mut sources_store = Vec::new();

        let mut name_slices = create_vec_of_len(module_count, || 0..0);
        let mut name_store = String::new();

        // Go over destinations and names
        for (index, &(_, name, dests_str)) in modules_str.iter().enumerate() {
            if dests_str != "" {
                let dests_start = dests_store.len();
                dests_store.extend(dests_str.split(", ").map(|s| {
                    let dest_index = name_table[s];
                    let count = sources[dest_index].len();
                    sources[dest_index].push(index);
                    ModuleKey::new(dest_index, count)
                }));
                let dests_end = dests_store.len();

                dests_slices[index] = dests_start..dests_end;
            }

            let name_start = name_store.len();
            name_store.push_str(name);
            let name_end = name_store.len();

            name_slices[index] = name_start..name_end;
        }

        for index in 0..module_count {
            let sources_start = sources_store.len();
            sources_store.append(&mut sources[index]);
            let sources_end = sources_store.len();
            sources_slices[index] = sources_start..sources_end;
        }

        drop(sources);

        let dests_store: &'static [ModuleKey] = &*dests_store.leak();
        let name_store: &'static str = &*name_store.leak();
        let sources_store: &'static [usize] = &*sources_store.leak();

        for (index, (kind, _, _)) in modules_str.into_iter().enumerate() {
            let dests = &dests_store[dests_slices[index].clone()];
            let name = &name_store[name_slices[index].clone()];
            let sources = &sources_store[sources_slices[index].clone()];
            let source_count = sources_slices[index].len();

            modules[index].write(Module {
                index,
                name,
                dests,
                sources,
                kind: match kind {
                    "&" if source_count == 1 => ModuleKind::Inverter,
                    "%" => ModuleKind::FlipFlop,
                    "&" => ModuleKind::Conjunction,
                    "b" => ModuleKind::Broadcaster,
                    "" => ModuleKind::Untyped,
                    _ => unreachable!(),
                },
            });

            modules_state[index].write(match kind {
                "&" if source_count == 1 => ModuleState::NoState,
                "%" => ModuleState::FlipFlop(false),
                "&" => ModuleState::Conjunction(vec![true; source_count].into()),
                "b" => ModuleState::NoState,
                "" => ModuleState::NoState,
                _ => unreachable!(),
            });
        }

        // SAFETY: all modules have been initialized
        let modules: Box<[Module]> = unsafe { std::mem::transmute(modules) };
        // SAFETY: all modules state have been initialized
        let modules_state: Box<[ModuleState]> = unsafe { std::mem::transmute(modules_state) };
        let broadcast = ModuleKey::new(
            modules
                .iter()
                .find(|m| m.kind == ModuleKind::Broadcaster)
                .unwrap()
                .index,
            0,
        );

        let modules = modules.into();

        Ok(Self {
            modules,
            modules_state,
            broadcast,
        })
    }
}

impl State {
    fn press_button<const LH: bool>(&mut self, mask: &StateMask) -> (u64, u64) {
        // Broadcast has been masked, we can safely return there
        if !mask[self.broadcast.index] {
            return (0, 0);
        }

        let mut lows = 0u64;
        let mut highs = 0u64;

        let mut queue = VecDeque::new();
        let mut enqueue_signals = |queue: &mut VecDeque<Signal>, low: bool, dests: &[ModuleKey]| {
            if LH {
                if low {
                    lows += dests.len() as u64;
                } else {
                    highs += dests.len() as u64;
                }
            }

            queue.extend(
                dests
                    .iter()
                    .filter_map(|&to| mask[to.index].then_some(Signal::new(low, to))),
            );
        };

        enqueue_signals(&mut queue, true, &[self.broadcast]);
        while let Some(sig) = queue.pop_front() {
            let module = self.modules[sig.to.index];
            match module.kind {
                ModuleKind::Broadcaster => {
                    enqueue_signals(&mut queue, sig.low, module.dests);
                }
                ModuleKind::FlipFlop if sig.low => {
                    let ModuleState::FlipFlop(state) = &mut self.modules_state[module.index] else {
                        unreachable!();
                    };
                    *state = !*state;
                    enqueue_signals(&mut queue, !*state, module.dests);
                }
                ModuleKind::Conjunction => {
                    let ModuleState::Conjunction(memory) = &mut self.modules_state[module.index]
                    else {
                        unreachable!();
                    };
                    memory[sig.to.number] = sig.low;

                    let low = memory.iter().all(|&x| !x);

                    enqueue_signals(&mut queue, low, module.dests);
                }
                ModuleKind::Inverter => {
                    enqueue_signals(&mut queue, !sig.low, module.dests);
                }
                _ => {}
            }
        }

        (lows, highs)
    }

    fn get_state(&self, mask: &StateMask) -> Vec<ModuleState> {
        self.modules_state
            .iter()
            .enumerate()
            .filter(|&(i, _)| mask[i])
            .map(|(_, s)| s.clone())
            .collect_vec()
    }
}

pub async fn day20(input: String) -> Result<(String, String)> {
    let state: State = input.parse()?;

    let part1 = {
        let mut state = state.clone();
        let mut lows = 0;
        let mut highs = 0;
        let mask = vec![true; state.modules.len()];
        for _ in 0..1000 {
            let (nl, nh) = state.press_button::<true>(&mask);
            lows += nl;
            highs += nh;
        }
        highs * lows
    };

    let part2 = {
        let mut subgraphs = state
            .modules
            .iter()
            .map(|m| {
                let mut deps = FxHashSet::default();
                let mut cur_deps = vec![m.index];
                let mut next_deps = Vec::new();

                while !cur_deps.is_empty() {
                    for dep in cur_deps.drain(..) {
                        deps.insert(dep);

                        next_deps.extend(
                            state.modules[dep]
                                .sources
                                .iter()
                                .filter(|&s| !deps.contains(s)),
                        );
                    }
                    std::mem::swap(&mut cur_deps, &mut next_deps);
                }

                deps
            })
            .unique_by(|e| {
                let mut vec = e.iter().copied().collect_vec();
                vec.sort();
                vec
            })
            .collect_vec();

        subgraphs.sort_by_key(|s| s.len());

        let mut lcm = 1;

        for subgraph in subgraphs {
            if subgraph.len() > 16 {
                continue;
            }

            let mask = (0..state.modules.len())
                .map(|i| subgraph.contains(&i))
                .collect_vec();
            let mut state = state.clone();

            let mut past_states = FxHashMap::default();
            past_states.insert(state.get_state(&mask), 0);

            for iter in 1u64.. {
                state.press_button::<false>(&mask);
                let state_value = state.get_state(&mask);
                if past_states.contains_key(&state_value) {
                    lcm = lcm.lcm(&iter);
                    break;
                } else {
                    past_states.insert(state_value, iter);
                }
            }
        }

        lcm
    };

    Ok((part1.to_string(), part2.to_string()))
}
