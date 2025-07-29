use std::str::FromStr;

use anyhow::{Result, anyhow};
use itertools::Itertools;

use crate::bigletters;

const WIDTH: usize = 25;
const HEIGHT: usize = 6;
const LAYER_SIZE: usize = WIDTH * HEIGHT;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Pixel {
    Transparent,
    White,
    Black,
}

impl TryFrom<char> for Pixel {
    type Error = anyhow::Error;

    fn try_from(c: char) -> Result<Self> {
        match c {
            '0' => Ok(Self::Black),
            '1' => Ok(Self::White),
            '2' => Ok(Self::Transparent),
            _ => Err(anyhow!("Bad input")),
        }
    }
}

struct Image {
    data: Box<[Pixel]>,
    layers: usize,
}

impl FromStr for Image {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        let data = s
            .trim()
            .chars()
            .map(|c| c.try_into())
            .collect::<Result<Vec<Pixel>>>()?
            .into_boxed_slice();
        Ok(Self::new(data))
    }
}

impl Image {
    fn new(data: Box<[Pixel]>) -> Self {
        let layers = data.len() / (WIDTH * HEIGHT);
        Self { data, layers }
    }

    fn layers(&self) -> impl DoubleEndedIterator<Item = &[Pixel]> {
        (0..self.layers).map(|i| &self.data[i * LAYER_SIZE..(i + 1) * LAYER_SIZE])
    }
}

pub async fn day08(input: String) -> Result<(String, String)> {
    let image: Image = input.parse()?;

    let part1 = {
        let fewest_zeros_layer = image
            .layers()
            .sorted_by_cached_key(|l| l.iter().filter(|&&d| d == Pixel::Transparent).count())
            .next()
            .ok_or_else(|| anyhow!("No layer found"))?;
        fewest_zeros_layer
            .iter()
            .filter(|&&d| d == Pixel::Black)
            .count()
            * fewest_zeros_layer
                .iter()
                .filter(|&&d| d == Pixel::White)
                .count()
    };

    let part2 = {
        let mut layer = [Pixel::Transparent; LAYER_SIZE];

        for overlay in image.layers().rev() {
            for (pos, &pixel) in overlay.iter().enumerate() {
                layer[pos] = match (pixel, layer[pos]) {
                    (Pixel::Transparent, any) => any,
                    (any, _) => any,
                }
            }
        }

        let bits = layer.iter().map(|&p| p == Pixel::White).collect_vec();

        bigletters::read_bitmap_string(&bits, WIDTH)
    };

    Ok((part1.to_string(), part2))
}

