use std::collections::{HashMap, VecDeque};

use anyhow::{Result, anyhow};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Cell {
    Empty,
    Wall,
    Portal([char; 2]),
    Entrance,
    Exit,
}

struct Grid {
    cells: Vec<Vec<Cell>>,
    portals: HashMap<[char; 2], [(usize, usize); 2]>,
    size: (usize, usize),
    entrance: (usize, usize),
    exit: (usize, usize),
}

impl Grid {
    fn is_outside(&self, pos: (usize, usize)) -> bool {
        pos.0 < 5 || pos.1 < 5 || pos.0 >= self.size.0 - 5 || pos.1 >= self.size.1 - 5
    }
    fn apply_delta(&self, pos: (usize, usize), delta: (isize, isize)) -> Option<(usize, usize)> {
        let x = pos.0.checked_add_signed(delta.0)?;
        let y = pos.1.checked_add_signed(delta.1)?;

        (x < self.size.0 && y < self.size.1).then_some((x, y))
    }
}

impl<'a> From<&'a str> for Grid {
    fn from(value: &'a str) -> Self {
        let width = value
            .chars()
            .position(|c| c == '\n')
            .expect("No newline in grid string");
        let height = value.lines().count();

        let mut grid = Self {
            cells: vec![vec![Cell::Empty; width]; height],
            portals: HashMap::new(),
            size: (width, height),
            entrance: (0, 0),
            exit: (0, 0),
        };

        let index_of = |(x, y)| x + y * (width + 1);
        let char_at = |pos| {
            let index = index_of(pos);
            value[index..(index + 1)].chars().next().unwrap()
        };

        for x in 0..width {
            for y in 0..height {
                match char_at((x, y)) {
                    '.' => {
                        for (delta, flip) in [
                            ((-1, 0), true),
                            ((1, 0), false),
                            ((0, -1), true),
                            ((0, 1), false),
                        ] {
                            if let Some(pos) = grid.apply_delta((x, y), delta) {
                                let char1 = char_at(pos);
                                if matches!(char1, 'A'..='Z') {
                                    let char2 = char_at(grid.apply_delta(pos, delta).unwrap());

                                    let key = if flip { [char2, char1] } else { [char1, char2] };

                                    if key == ['A', 'A'] {
                                        grid.cells[y][x] = Cell::Entrance;
                                        grid.entrance = (x, y);
                                    } else if key == ['Z', 'Z'] {
                                        grid.cells[y][x] = Cell::Exit;
                                        grid.exit = (x, y);
                                    } else {
                                        grid.cells[y][x] = Cell::Portal(key);

                                        grid.portals
                                            .entry(key)
                                            .and_modify(|v| v[1] = (x, y))
                                            .or_insert([(x, y), (x, y)]);
                                    }

                                    break;
                                }
                            }
                        }

                        // If couldn't find any portal around, default is empty anyways, so keep it
                        // that way.
                    }
                    _ => {
                        grid.cells[y][x] = Cell::Wall;
                    }
                }
            }
        }

        grid
    }
}

fn solve_part1(grid: &Grid) -> Result<usize> {
    let mut queue = VecDeque::new();
    let mut visited = vec![vec![false; grid.size.0]; grid.size.1];

    visited[grid.entrance.1][grid.entrance.0] = true;
    queue.push_front((grid.entrance, 0));

    while let Some((pos, dist)) = queue.pop_front() {
        let cell = grid.cells[pos.1][pos.0];

        if matches!(cell, Cell::Empty | Cell::Entrance | Cell::Portal(_)) {
            for delta in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
                if let Some(new_pos) = grid.apply_delta(pos, delta)
                    && grid.cells[new_pos.1][new_pos.0] != Cell::Wall
                    && !visited[new_pos.1][new_pos.0]
                {
                    visited[new_pos.1][new_pos.0] = true;
                    queue.push_back((new_pos, dist + 1));
                }
            }
        }

        if let Cell::Portal(key) = cell {
            let portals = grid.portals[&key];
            let new_pos = if portals[0] == pos {
                portals[1]
            } else {
                portals[0]
            };

            if !visited[new_pos.1][new_pos.0] {
                visited[new_pos.1][new_pos.0] = true;
                queue.push_back((new_pos, dist + 1));
            }
        } else if let Cell::Exit = cell {
            return Ok(dist);
        }
    }

    Err(anyhow!("Search ended without finding exit"))
}

fn solve_part2(grid: &Grid) -> Result<usize> {
    let mut queue = VecDeque::new();
    let mut visited = vec![vec![0u64; grid.size.0]; grid.size.1];

    fn set_visited(visited: &mut Vec<Vec<u64>>, pos: (usize, usize), level: usize) {
        visited[pos.1][pos.0] |= 1 << level
    }

    fn is_visited(visited: &Vec<Vec<u64>>, pos: (usize, usize), level: usize) -> bool {
        (visited[pos.1][pos.0] & (1 << level)) != 0
    }

    set_visited(&mut visited, grid.entrance, 0);
    queue.push_front((grid.entrance, 0, 0));

    while let Some((pos, level, dist)) = queue.pop_front() {
        let cell = grid.cells[pos.1][pos.0];

        if matches!(cell, Cell::Empty | Cell::Entrance | Cell::Portal(_)) {
            for delta in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
                if let Some(new_pos) = grid.apply_delta(pos, delta)
                    && grid.cells[new_pos.1][new_pos.0] != Cell::Wall
                    && !is_visited(&visited, new_pos, level)
                {
                    set_visited(&mut visited, new_pos, level);
                    queue.push_back((new_pos, level, dist + 1));
                }
            }
        }

        let pos_outer = grid.is_outside(pos);
        if let Cell::Portal(key) = cell && (level >= 1 || !pos_outer) && (level < 63 || pos_outer) {
            let portals = grid.portals[&key];
            let new_pos = if portals[0] == pos {
                portals[1]
            } else {
                portals[0]
            };
            let new_level = if pos_outer { level - 1 } else { level + 1 };

            if !is_visited(&visited, new_pos, new_level) {
                set_visited(&mut visited, new_pos, new_level);
                queue.push_back((new_pos, new_level, dist + 1));
            }
        } else if let Cell::Exit = cell && level == 0 {
            return Ok(dist);
        }
    }

    Err(anyhow!("Search ended without finding exit"))
}

pub async fn day20(input: String) -> Result<(String, String)> {
    let grid = Grid::from(&input[..]);

    Ok((solve_part1(&grid)?.to_string(), solve_part2(&grid)?.to_string()))
}

