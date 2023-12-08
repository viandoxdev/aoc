use anyhow::{anyhow, Result};
use itertools::Itertools;

static CARD_MAP: [u32; 256] = {
    let mut res = [0u32; 256];
    res['A' as usize] = 12;
    res['K' as usize] = 11;
    res['Q' as usize] = 10;
    res['J' as usize] = 9;
    res['T' as usize] = 8;
    res['9' as usize] = 7;
    res['8' as usize] = 6;
    res['7' as usize] = 5;
    res['6' as usize] = 4;
    res['5' as usize] = 3;
    res['4' as usize] = 2;
    res['3' as usize] = 1;
    res['2' as usize] = 0;
    res
};

static CARD_MAP_JOKER: [u32; 256] = {
    let mut res = [0u32; 256];
    res['A' as usize] = 12;
    res['K' as usize] = 11;
    res['Q' as usize] = 10;
    res['T' as usize] = 9;
    res['9' as usize] = 8;
    res['8' as usize] = 7;
    res['7' as usize] = 6;
    res['6' as usize] = 5;
    res['5' as usize] = 4;
    res['4' as usize] = 3;
    res['3' as usize] = 2;
    res['2' as usize] = 1;
    res['J' as usize] = 0;
    res
};

struct Hand {
    value: u32,
    bid: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum HandType {
    FiveOfAKind = 7,
    FourOfAKind = 6,
    FullHouse = 5,
    ThreeOfAKind = 4,
    TwoPair = 3,
    OnePair = 2,
    HighCard = 1,
}

impl Hand {
    fn parse(s: &str, joker: bool) -> Result<Self> {
        // Use the right map
        let map = if joker { &CARD_MAP_JOKER } else { &CARD_MAP };
        let (hand_str, bid_str) = s.split_once(" ").ok_or(anyhow!("Malformed hand"))?;
        let bid: u32 = bid_str.parse()?;
        // Array of the counts of each card 
        let mut count = [0u8; 13];
        // Array of the counts of each count of same card, if there are five of the same cards then
        // kinds[5] will be one
        let mut kinds = [0u8; 6];

        // Do a first count of the cards
        for c in hand_str.chars() {
            count[map[c as usize] as usize] += 1;
        }

        // Resolve the joker then
        if joker {
            // Get the card of which there is the most of in the hand (that is not the joker)
            let mut max_card = 1;
            let mut max_card_count = 0;
            for c in hand_str.chars() {
                let card = map[c as usize];
                if card == 0 {
                    continue;
                }
                let count = count[card as usize];
                if count > max_card_count {
                    max_card = card;
                    max_card_count = count;
                }
            }
            // Remeber the number of jokers
            let joker_count = count[map['J' as usize] as usize];
            // Set the number to 0 (since all jokers will be changed)
            count[map['J' as usize] as usize] = 0;
            // Add the number of jokers to the count of max_card, this is the same as turning the
            // jokers into said card
            count[max_card as usize] += joker_count;
        }

        // Count the counts
        for &c in &count {
            kinds[c as usize] += 1;
        }

        // Get the hand kind, here kinds[x] >= y is true if there are more than y groups of x of the same
        // cards
        let hand_type = if kinds[5] >= 1 { // If theres 5 cards of the same
            HandType::FiveOfAKind
        } else if kinds[4] >= 1 { // If theres 4 cards of the same
            HandType::FourOfAKind
        } else if kinds[3] >= 1 && kinds[2] >= 1 { // If theres 3 cards of the same and a pair
            HandType::FullHouse
        } else if kinds[3] >= 1 { // If there are 3 cards of the same
            HandType::ThreeOfAKind
        } else if kinds[2] >= 2 { // If there are two pairs
            HandType::TwoPair
        } else if kinds[2] >= 1 { // If there is one pair
            HandType::OnePair
        } else {
            HandType::HighCard
        };

        // The value is a 24 bit number, with each 4bit being one of the following (in msb order):
        // hand type, first card, second card, third card, second card, first card
        let value = hand_str
            .chars()
            .map(|x| map[x as usize])
            .fold(hand_type as u32, |a, c| (a << 4) | c);

        Ok(Self { value, bid })
    }
}

pub async fn day7(input: String) -> Result<(String, String)> {
    // Only difference between part1 and part2 is the joker flag being set
    let (part1, part2) = [true, false]
        .into_iter()
        .map(|joker| {
            // Get the hands
            let mut hands: Vec<Hand> = input
                .lines()
                .map(|x| Hand::parse(x, joker))
                .try_collect()
                .unwrap();
            // Sort them by value (the value is an agregate of the hand type and the values of each
            // cards, in that order, meaning comparison works)
            hands.sort_by_key(|x| x.value);

            // Sum all the hands' bid multiplied by their rank
            hands
                .iter()
                .enumerate()
                .map(|(i, h)| (i as u32 + 1) * h.bid)
                .sum::<u32>()
        })
        .collect_tuple()
        .ok_or(anyhow!("Wtf"))?;

    Ok((part1.to_string(), part2.to_string()))
}
