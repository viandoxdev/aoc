use std::{
    fmt::Debug,
    ops::{Add, Sub},
    str::FromStr,
};

use anyhow::{Context, Error, Result};
use itertools::Itertools;

/// Arbitrary vec3 (because I don't know yet what type I'll need)
struct Vec3<T> {
    x: T,
    y: T,
    z: T,
}

impl<T: FromStr> FromStr for Vec3<T> {
    type Err = Error;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let (x, y, z) = s
            .split(", ")
            .flat_map(T::from_str)
            .collect_tuple()
            .context("Bad Vec3")?;
        Ok(Self { x, y, z })
    }
}

impl<T: Clone> Clone for Vec3<T> {
    fn clone(&self) -> Self {
        Self {
            x: self.x.clone(),
            y: self.y.clone(),
            z: self.y.clone(),
        }
    }
}

impl<T: Add> Add for Vec3<T> {
    type Output = Vec3<<T as Add>::Output>;
    fn add(self, rhs: Self) -> Self::Output {
        Vec3 {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
            z: self.z + rhs.z,
        }
    }
}

impl<T: Sub> Sub for Vec3<T> {
    type Output = Vec3<<T as Sub>::Output>;
    fn sub(self, rhs: Self) -> Self::Output {
        Vec3 {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
            z: self.z - rhs.z,
        }
    }
}

impl<T: Copy> Copy for Vec3<T> {}

impl<T: Debug> Debug for Vec3<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "v3({:?}, {:?}, {:?})", self.x, self.y, self.z)
    }
}

#[derive(Debug, Clone, Copy)]
struct Hail {
    pos: Vec3<i128>,
    vel: Vec3<i128>,
}

impl FromStr for Hail {
    type Err = Error;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let (p, v) = s.split_once(" @ ").context("Bad input")?;
        Ok(Self {
            pos: p.parse()?,
            vel: v.parse()?,
        })
    }
}

pub async fn day24(input: String) -> Result<(String, String)> {
    let mut hails: Vec<Hail> = input.lines().map(Hail::from_str).try_collect()?;

    let region_min = 200000000000000;
    let region_max = 400000000000000;
    //let region_min = 7;
    //let region_max = 27;

    let part1 = hails
        .iter()
        .tuple_combinations()
        .map(|(h1, h2)| {
            let denom = h1.vel.x * h2.vel.y - h1.vel.y * h2.vel.x;

            // Denom is really the 2d cross product of the two direction vectors, it being 0 means
            // they are collinear, and there are therefore 0 or an infinity of intersections (since
            // the lines are parallels)
            if denom == 0 {
                return 0;
            }

            let (a, b) = (h1.pos, h1.pos + h1.vel);
            let (c, d) = (h2.pos, h2.pos + h2.vel);

            // t and u should be t / denom and u / denom, but we only care to see they are both
            // positive (to make sure we don't count past intersections), so we don't do the
            // division
            let t = (a.y - c.y) * h2.vel.x - (a.x - c.x) * h2.vel.y;
            let u = (a.y - c.y) * h1.vel.x - (a.x - c.x) * h1.vel.y;
            // Compute the intersection point, of the whole infinite lines
            let px = ((c.x * d.y - c.y * d.x) * h1.vel.x - (a.x * b.y - a.y * b.x) * h2.vel.x) / denom;
            let py = ((c.x * d.y - c.y * d.x) * h1.vel.y - (a.x * b.y - a.y * b.x) * h2.vel.y) / denom;

            // Make sure the intersection doesn't happen in the past and that it happens in the
            // region too
            (t.signum() == denom.signum()
                && u.signum() == denom.signum()
                && px >= region_min
                && px <= region_max
                && py >= region_min
                && py <= region_max) as usize
        })
        .sum::<usize>();

    Ok((part1.to_string(), "TODO".to_string()))
}
