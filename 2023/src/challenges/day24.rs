use std::{
    fmt::Debug,
    ops::{Add, Sub, SubAssign},
    str::FromStr,
};

use num::Integer;

use anyhow::{Context, Error, Result};
use itertools::Itertools;

/// Arbitrary vec3 (because I don't know yet what type I'll need)
struct Vec3<T> {
    x: T,
    y: T,
    z: T,
}

impl Vec3<i128> {
    fn dot(self, other: Self) -> i128 {
        self.x * other.x + self.y * other.y + self.z * other.z
    }
    fn cross(self, other: Self) -> Self {
        Self {
            x: self.y * other.z - self.z * other.y,
            y: self.z * other.x - self.x * other.z,
            z: self.x * other.y - self.y * other.x,
        }
    }
    fn scale(self, factor: i128) -> Self {
        Self {
            x: self.x * factor,
            y: self.y * factor,
            z: self.z * factor,
        }
    }
    fn try_squeeze(self, factor: i128) -> Option<Self> {
        let (x, x_rem) = self.x.div_rem(&factor);
        let (y, y_rem) = self.y.div_rem(&factor);
        let (z, z_rem) = self.z.div_rem(&factor);
        (x_rem == 0 && y_rem == 0 && z_rem == 0).then_some(Self { x, y, z })
    }
}

impl<T: PartialEq> PartialEq for Vec3<T> {
    fn eq(&self, other: &Self) -> bool {
        self.x == other.x && self.y == other.y && self.z == other.z
    }
}

impl<T: Eq> Eq for Vec3<T> {}

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

impl<T: Default> Default for Vec3<T> {
    fn default() -> Self {
        Self {
            x: Default::default(),
            y: Default::default(),
            z: Default::default(),
        }
    }
}

impl<T: Clone> Clone for Vec3<T> {
    fn clone(&self) -> Self {
        Self {
            x: self.x.clone(),
            y: self.y.clone(),
            z: self.z.clone(),
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

impl<T: SubAssign> SubAssign for Vec3<T> {
    fn sub_assign(&mut self, rhs: Self) {
        self.x -= rhs.x;
        self.y -= rhs.y;
        self.z -= rhs.z;
    }
}

impl<A> Vec3<A> {
    fn convert<B: From<A>>(self) -> Vec3<B> {
        Vec3 {
            x: B::from(self.x),
            y: B::from(self.y),
            z: B::from(self.z),
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
struct Hail<T> {
    pos: Vec3<T>,
    vel: Vec3<T>,
}

impl<A> Hail<A> {
    fn convert<B: From<A>>(self) -> Hail<B> {
        Hail {
            pos: self.pos.convert(),
            vel: self.vel.convert(),
        }
    }
}

impl<T: FromStr> FromStr for Hail<T> {
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
    let hails: Vec<Hail<i128>> = input.lines().map(Hail::from_str).try_collect()?;

    let region_min_x = 200000000000000i128;
    let region_max_x = 400000000000000i128;
    let region_min_y = 200000000000000i128;
    let region_max_y = 400000000000000i128;

    let part1 = hails
        .iter()
        .tuple_combinations()
        .filter(|(h1, h2)| {
            let (h1, h2): (Hail<i128>, Hail<i128>) = (h1.convert(), h2.convert());
            let denom = h1.vel.x * h2.vel.y - h1.vel.y * h2.vel.x;

            // Denom is really the 2d cross product of the two direction vectors, it being 0 means
            // they are collinear, and there are therefore 0 or an infinity of intersections (since
            // the lines are parallels)
            if denom == 0 {
                return false;
            }

            let (a, b) = (h1.pos, h1.pos + h1.vel);
            let (c, d) = (h2.pos, h2.pos + h2.vel);

            // t and u should be t / denom and u / denom, but we only care to see they are both
            // positive (to make sure we don't count past intersections), so we don't do the
            // division
            let t = (a.y - c.y) * h2.vel.x - (a.x - c.x) * h2.vel.y;
            let u = (a.y - c.y) * h1.vel.x - (a.x - c.x) * h1.vel.y;
            // Compute the intersection point, of the whole infinite lines
            let px =
                ((c.x * d.y - c.y * d.x) * h1.vel.x - (a.x * b.y - a.y * b.x) * h2.vel.x) / denom;
            let py =
                ((c.x * d.y - c.y * d.x) * h1.vel.y - (a.x * b.y - a.y * b.x) * h2.vel.y) / denom;

            // Make sure the intersection doesn't happen in the past and that it happens in the
            // region too
            t.signum() == denom.signum()
                && u.signum() == denom.signum()
                && px >= region_min_x
                && px <= region_max_x
                && py >= region_min_y
                && py <= region_max_y
        })
        .count();

    let part2 = hails
        .iter()
        .tuple_combinations()
        .find_map(|(h1, h2, h3)| {
            // Everything will be relative to h1.

            // We pick a second hail, the stone must pass both by the origin hail (so the origin)
            // and this one, meaning its path will be contained in the plane generated by the origin
            // and this hail's path
            let h2 = Hail {
                pos: h2.pos - h1.pos,
                vel: h2.vel - h1.vel,
            };
            // We finally pick a third hail, this one's path will intersect with the earlier plane,
            // since all stone paths must be contained in the plane, and the stone must cross the
            // third one's path, the stone path will pass by their intersection
            let h3 = Hail {
                pos: h3.pos - h1.pos,
                vel: h3.vel - h1.vel,
            };

            // Normal to the plane
            let n = h2.vel.cross(h2.pos);
            // Time at which the stone hits hail3
            let t3 = -h3.pos.dot(n) / h3.vel.dot(n);

            // Repeat with stuff swapped
            let n = h3.vel.cross(h3.pos);
            // Time at which the stone hits hail2
            let t2 = -h2.pos.dot(n) / h2.vel.dot(n);

            // Grab the stones again to discard the offset now that we have the collision time
            let h2 = hails[1];
            let h3 = hails[2];

            let p2 = h2.pos + h2.vel.scale(t2);
            let p3 = h3.pos + h3.vel.scale(t3);

            let stone_vel = (p2 - p3).try_squeeze(t2 - t3)?;
            let stone_pos = p3 - stone_vel.scale(t3);

            Some(stone_pos.x + stone_pos.y + stone_pos.z)
        })
        .unwrap();

    Ok((part1.to_string(), part2.to_string()))
}
