use std::{collections::HashSet, str::FromStr};

use anyhow::{anyhow, Result};

// A Card (we don't store ids because they aren't used in the problem)
struct Card {
    winning: HashSet<u32>,
    numbers: Vec<u32>,
    copies: u32,
}

impl Card {
    // How many numbers are winning numbers
    fn won(&self) -> usize {
        self.numbers
            .iter()
            .filter(|n| self.winning.contains(n))
            .count()
    }
    // Score of the card (0 if no winning number, or 2^(n - 1) with n that number otherwise)
    fn score(&self) -> u32 {
        (1 << (self.won() as u32)) >> 1
    }
}

impl FromStr for Card {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        // Get the substrings with the numbers and winning numbers
        let (_, s) = s.split_once(": ").ok_or(anyhow!("Malformed card"))?;
        let (win_str, num_str) = s.split_once(" | ").ok_or(anyhow!("Malformed card"))?;

        // Parse winnings (into hashset) (some kind of bool array would be better)
        let winning = win_str
            .split_whitespace()
            .map(u32::from_str)
            .collect::<std::result::Result<HashSet<u32>, _>>()?;
        // Parse numbers (into vec)
        let numbers = num_str
            .split_whitespace()
            .map(u32::from_str)
            .collect::<std::result::Result<Vec<u32>, _>>()?;

        Ok(Self {
            winning,
            numbers,
            copies: 1,
        })
    }
}

pub async fn day4(input: String) -> Result<(String, String)> {
    // Parse all the cards
    let mut cards = input
        .lines()
        .map(Card::from_str)
        .collect::<Result<Vec<Card>>>()?;

    // Add up the score of every card (part 1)
    let part1 = cards.iter().map(Card::score).sum::<u32>();

    // Compute copies of all cards
    cards.reverse(); // Reverse since pop is more efficient on Vec
    let mut part2 = 0; // Sum of part 2
    while let Some(first) = cards.pop() {
        part2 += first.copies;
        // We have to get the length here or rust complains about aliasing
        let len = cards.len();
        // Update the number of copies of the next cards
        for card in &mut cards[(len - first.won())..] {
            card.copies += first.copies;
        }
    }

    Ok((part1.to_string(), part2.to_string()))
}
