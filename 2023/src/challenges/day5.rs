use std::{ops::Range, str::FromStr};

use anyhow::{anyhow, Error, Result};
use itertools::Itertools;

/// Get the overlap, if any, of two ranges
fn range_overlap<T: Ord>(a: Range<T>, b: Range<T>) -> Option<Range<T>> {
    (a.start < b.end && b.start < a.end).then(|| a.start.max(b.start)..a.end.min(b.end))
}

// Maximum value of any of the numbers (we use u64 to avoid overflows in intermediary values,
// otherwise all numbers stay at or below u32::MAX)
const MAX: u64 = u32::MAX as u64;

// Two ranges in one with the same length
#[derive(Clone, Copy, PartialEq, Eq)]
struct MappedRange {
    dest: u64,
    source: u64,
    length: u64,
}

impl MappedRange {
    fn new(source: u64, dest: u64, length: u64) -> Self {
        Self {
            dest,
            length,
            source,
        }
    }

    fn dest_range(&self) -> Range<u64> {
        self.dest..(self.dest + self.length)
    }
}

impl FromStr for MappedRange {
    type Err = Error;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        // This is an iterator of Result<u64>, so calling next returns Option<Result<u64>>, which
        // is annoying, and we want to avoid using the collect into Result trick since that would
        // incur an unecessary allocation (for the vec)
        let mut i = s.split_whitespace().map(|x| x.parse::<u64>());
        // Instead we just convert the option to a result and fold the results (and_then) in a
        // closure
        let mut next = || {
            i.next()
                .ok_or(anyhow!("Malformed range"))
                .and_then(|x| x.map_err(|e| e.into()))
        };

        // Order matters here
        let dest = next()?;
        let source = next()?;
        let length = next()?;

        Ok(Self {
            dest,
            source,
            length,
        })
    }
}

struct Map {
    // We use Box<[T]> to emphasize on the fact that theses arrays shouldn't be mutated (order
    // matters and both need to be in sync)
    /// Ranges sorted by source (used for part1 and a little bit of part2)
    ranges: Box<[MappedRange]>,
    /// Ranges sorted by dest, includes implicit ranges too (the ranges mapping the unmapped
    /// numbers to themselves) (used in part2 only)
    ranges_dest: Box<[MappedRange]>,
}

impl Map {
    // Resolve, get the mapped value
    fn get(&self, val: u64) -> u64 {
        // We binary search through the ranges to find the one with our value
        let mut slice = &self.ranges[..];

        while slice.len() > 1 {
            let mid_index = slice.len() / 2;
            let mid = slice[mid_index];
            if val >= mid.source {
                slice = &slice[mid_index..];
            } else {
                slice = &slice[..mid_index];
            }
        }

        // No range returned by the search: map val to itself
        if slice.is_empty() {
            return val;
        }

        let range = slice[0];
        if val < range.source || val >= range.source + range.length {
            // We got a range, but it doesn't contain val
            return val;
        }

        // Map according to the found range
        range.dest + (val - range.source)
    }

    // Get the source ranges for a single destination range (in order)
    fn reverse(&self, dest: Range<u64>) -> impl Iterator<Item = Range<u64>> + '_ {
        // Workaround for Ranges not implementing Copy
        let mut dest_start = dest.start;
        let dest_end = dest.end;
        // The iterator we're basing ourselves off of. We're essentially reimplemeting flat_map in
        // iter::from_fn to be able to keep state. The point of all of this is to avoid having to
        // collect at any point, the compiler should then be able to inline and loopify it all at
        // the calling site, suppressing the overhead of this abomination.
        let mut iter = self.ranges_dest.iter();
        // We essentially just look for a range whose destination range overlaps with the range we
        // want to find the sources of, once we got the overlap we resolved some of the values so
        // we save those and go for the next.
        //       (mapped to) 3..6 6..9 0..3
        // i.e (dest ranges) 0..3 3..6 6..9  <- reverse 4..7
        //     1. 4..7 -> 0..3 overlap: None, next
        //     2. 4..7 -> 3..6 overlap: 4..6 -> source: 7..9 (new range: 6..7)
        //     3. 6..7 -> 6..9 overlap: 6..7 -> source: 0..1 (new range: 7..7, done)
        //     => sources: 7..9 0..1 (so values 7, 8, 9, 0 are mapped to 4, 5, 6, 7)
        std::iter::from_fn(move || {
            if dest_start == dest_end {
                return None;
            }
            // Find the next range containing one or more values we want to reverse
            loop {
                // Rebuild the range here, this works because dest_start and dest_end are copy
                let dest = dest_start..dest_end;
                // Get the next value from the base iterator, we use '?' because once this iterator
                // runs out we won't have any other values to process
                let r = iter.next()?;
                let overlap = range_overlap(dest, r.dest_range());
                if let Some(overlap) = overlap {
                    // There is some overlap: one or more of the values we're interested in are
                    // contained in this range
                    // Update the dest range since we already processed those values
                    dest_start = overlap.end;

                    // Return the source value
                    let len = overlap.end - overlap.start;
                    let start = overlap.start - r.dest + r.source;
                    return Some(start..(start + len));
                }
            }
        })
    }
}

impl FromStr for Map {
    type Err = Error;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        // Get the ranges, we skip the first line which is the name of the map
        let mut ranges: Vec<MappedRange> =
            s.lines().skip(1).map(MappedRange::from_str).try_collect()?;

        // Sort the ranges by source start, this is needed for the binary search
        ranges.sort_by_key(|x| x.source);

        // Get the ranges sorted by dest start
        let mut ranges_dest = ranges.clone();
        ranges_dest.sort_by_key(|x| x.dest);

        // Resolve implicit ranges
        // We do this by filling the gaps between ranges
        // i.e: if we have mapped ranges 2..5 and 8..14 (with a max of 16)
        //   we get: 0..2 2..5 5..8 8..14 14..=16
        // ranges     ─┴─────────┴───────────┘        theses ranges are mapped
        // that were added to "fill the gaps"              to themselves

        let mut all_ranges = Vec::new();

        // Fill the gap, if any, between the start of values and the first mapped range
        if let Some(&r) = ranges_dest.first() {
            if r.dest > 0 {
                all_ranges.push(MappedRange::new(0, 0, r.dest))
            }
        }

        // Fill the gaps between two mapped range
        for win in ranges_dest.windows(2) {
            let prev = win[0];
            let next = win[1];

            // Also include the mapped range itself
            all_ranges.push(prev);

            let after_prev = prev.dest + prev.length;
            if after_prev < next.dest {
                all_ranges.push(MappedRange::new(
                    after_prev,
                    after_prev,
                    next.dest - after_prev,
                ))
            }
        }

        // Fill the gap between the last range and the end of values, also include the last range
        // in the array
        if let Some(&r) = ranges_dest.last() {
            all_ranges.push(r);
            let end = r.dest + r.length;
            if end < MAX {
                all_ranges.push(MappedRange::new(end, end, MAX - end))
            }
        }

        Ok(Self {
            ranges: ranges.into_boxed_slice(),
            ranges_dest: all_ranges.into_boxed_slice(),
        })
    }
}

pub async fn day5(input: String) -> Result<(String, String)> {
    // Parse the maps
    let maps: Vec<Map> = input
        .split("\n\n")
        .skip(1)
        .map(Map::from_str)
        .try_collect()?;

    let seeds: Vec<u64> = input
        .split_once("\n\n")
        .ok_or(anyhow!("Malformed input"))?
        .0
        .split_whitespace()
        .skip(1) // Skip the "seeds: " string slice
        .map(u64::from_str)
        .try_collect()?;

    // Get the location of a seed, by applying all the maps one by one
    let seed_loc = |seed| maps.iter().fold(seed, |s, m| m.get(s));

    // Solve part 1, pretty straightforward
    let part1 = seeds
        .iter()
        .copied()
        .map(seed_loc)
        .min()
        .ok_or(anyhow!("No seeds"))?;

    // Solve part 2, pretty not straightforward
    // We do this by solving a more gengeral problem (because I felt like it): We get the seed for
    // every value of location. Obviously since there are ~4 billion values we don't process each
    // individually, but we work with ranges. Conceptually we take a range, and figure out what
    // sequence of ranges produce the same values in the same order as the original range when
    // mapped (this is the job of map.reverse), then we just do that for each map, obtaining a list
    // of ranges of seeds whose locations are 0 to MAX (the first seed of the first range has
    // location 0, and the last seed of the last range has location MAX)

    // We actually want a vec with a single range, not a vec with the items of a range
    #[allow(clippy::single_range_in_vec_init)]
    let ranges = maps.iter().rev().fold(vec![0..MAX], |rs, m| {
        rs.into_iter().flat_map(|r| m.reverse(r)).collect_vec()
    });

    // Get the seed ranges that we actually have, sorted by lowest seed
    let mut seed_ranges = seeds.chunks(2).map(|c| c[0]..(c[0] + c[1])).collect_vec();
    seed_ranges.sort_by_key(|r| r.start);

    // Find the lowest location seed range that we actually have
    let lowest_seed_range = ranges
        .iter()
        .find_map(|x| {
            seed_ranges
                .iter()
                .find_map(|s| range_overlap(s.clone(), x.clone()))
        })
        .ok_or(anyhow!("Wtf"))?;

    // Get the location of the first seed of that range
    let part2 = seed_loc(lowest_seed_range.start);

    Ok((part1.to_string(), part2.to_string()))
}
