use anyhow::Result;

use crate::intcode::Intcode;

#[derive(Debug, Clone, Copy)]
enum Tile {
    Empty = 0,
    Wall = 1,
    Block = 2,
    HorizontalPaddle = 3,
    Ball = 4,
}

impl From<i64> for Tile {
    fn from(value: i64) -> Self {
        match value {
            0 => Self::Empty,
            1 => Self::Wall,
            2 => Self::Block,
            3 => Self::HorizontalPaddle,
            4 => Self::Ball,
            unk => panic!("Invalid tile {unk}"),
        }
    }
}

const WIDTH: usize = 35;
const HEIGHT: usize = 25;

#[derive(Debug, Clone)]
struct BlockBreaker {
    program: Intcode<131>,
    framebuffer: [[Tile; WIDTH]; HEIGHT],
    paddle: (usize, usize),
    ball: (usize, usize),
    score: i64,
}

impl BlockBreaker {
    fn new(program: Intcode<131>) -> Self {
        Self {
            program,
            framebuffer: [[Tile::Empty; WIDTH]; HEIGHT],
            paddle: (0, 0),
            ball: (0, 0),
            score: 0,
        }
    }
    fn get_tile(&mut self, input: &[i64]) -> Result<Option<(usize, usize, Tile)>> {
        let Some([x, y, tile]) = self.program.run_until_array_output(input)? else {
            return Ok(None);
        };

        if x == -1 && y == 0 {
            self.score = tile;
            self.get_tile(input)
        } else {
            let (x, y, tile) = (x as usize, y as usize, Tile::from(tile));

            self.framebuffer[y][x] = tile;

            match tile {
                Tile::HorizontalPaddle => self.paddle = (x, y),
                Tile::Ball => self.ball = (x, y),
                _ => {}
            }

            Ok(Some((x, y, tile)))
        }
    }

    fn get_framebuffer(&mut self, input: &[i64]) -> Result<Option<()>> {
        for _ in 0..(WIDTH * HEIGHT) {
            if self.get_tile(input)?.is_none() {
                return Ok(None);
            }
        }

        Ok(Some(()))
    }
}

pub async fn day13(input: String) -> Result<(String, String)> {
    let program: Intcode<131> = input.parse()?;
    let block_breaker = BlockBreaker::new(program);

    let part1 = {
        let mut block_breaker = block_breaker.clone();

        block_breaker.get_framebuffer(&[])?;
        block_breaker
            .framebuffer
            .iter()
            .flat_map(|r| r.iter().copied())
            .filter(|t| matches!(t, Tile::Block))
            .count()
    };

    let part2 = {
        let mut block_breaker = block_breaker.clone();
        // Put coin in machine
        block_breaker.program.program[0] = 2;

        let mut input = [0];

        block_breaker.get_framebuffer(&input)?;

        loop {
            input[0] = (block_breaker.ball.0 as i64 - block_breaker.paddle.0 as i64).signum();

            if block_breaker.get_tile(&input)?.is_none() {
                break block_breaker.score;
            };
        }
    };

    Ok((part1.to_string(), part2.to_string()))
}
