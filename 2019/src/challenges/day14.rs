use std::collections::HashMap;

use anyhow::{Result, anyhow};
use fixedbitset::FixedBitSet;

fn parse_chemical(s: &str) -> Result<(u64, &str)> {
    let (count, name) = s.split_once(" ").ok_or_else(|| anyhow!("Bad chemical"))?;
    let count: u64 = count.parse()?;

    Ok((count, name))
}

#[derive(Debug, Clone)]
struct Recipe {
    produces: u64,
    ingredients: Vec<(u64, usize)>,
}

impl Default for Recipe {
    fn default() -> Self {
        Self {
            produces: 0,
            ingredients: Vec::new(),
        }
    }
}

impl Recipe {
    fn is_flat(&self, recipe: &[Recipe]) -> bool {
        self.ingredients
            .iter()
            .all(|&(_, ingr)| recipe[ingr].produces == 0)
    }

    fn scale(&mut self, factor: u64) {
        self.produces *= factor;
        for (count, _) in &mut self.ingredients {
            *count *= factor;
        }
    }

    fn flatten(&mut self, recipe: &[Recipe], deps: &[FixedBitSet]) {
        let chem_count = recipe.len();

        let mut resolvable = FixedBitSet::with_capacity(chem_count);

        while !self.is_flat(recipe) {

            // Clear because we reuse the same set between iterations
            resolvable.clear();
            // Mark all the ingredient as resolvable
            resolvable.extend(self.ingredients.iter().map(|&(_, ingr)| ingr));

            for &(_, ingr) in &self.ingredients {
                // Mark as unresolvable all the ingredients onto which another is dependent
                resolvable.difference_with(&deps[ingr]);

                // Mark as unresolvable if ingr isn't part of a recipe
                if recipe[ingr].produces == 0 {
                    resolvable.remove(ingr);
                }
            }

            if resolvable.is_empty() {
                panic!("Can't resolve any of the ingredients, but isn't flat yet");
            }

            // Resolve / flatten the ingredients we can
            for ingr in resolvable.ones() {
                let (count, index) = self
                    .ingredients
                    .iter()
                    .copied()
                    .enumerate()
                    .find_map(|(index, (count, i))| (i == ingr).then_some((count, index)))
                    .expect("Ingredient not in recipe");

                // How many times do we need to use this ingredients recipe
                let count = count.div_ceil(recipe[ingr].produces);
                // Whether we have removed ingr from the ingredient list
                let mut removed = false;

                // We can resolve / flatten ingr as no other ingredient depends on it
                for &(next_count, next_ingr) in &recipe[ingr].ingredients {
                    // We need to do the recipe count times, each taking next_count of next_ingr
                    let next_count = next_count * count;

                    // Check if next_ingr isn't alrady an ingredient, if so just increase that
                    // count
                    if let Some(i) = self.ingredients.iter_mut().find(|&&mut (_, ingr)| ingr == next_ingr) {
                        i.0 += next_count;
                    } else if !removed {
                        // next_ingr isn't already an ingredient, we have to add it to the array,
                        // but we have yet to remove ingr from it -> just replace it
                        self.ingredients[index] = (next_count, next_ingr);
                        removed = true;
                    } else {
                        // Can't replace, isn't already there, add it
                        self.ingredients.push((next_count, next_ingr));
                    }
                }

                // We didn't get the chance to remove ingr, do it now
                if !removed {
                    self.ingredients.swap_remove(index);
                }
            }
        }
    }
}

fn resolve_dependencies(chem: usize, deps: &mut [FixedBitSet], recipe: &[Recipe]) {
    let ingredients = &recipe[chem].ingredients;

    // Check if we have already resolved
    if deps[chem].count_ones(..) >= ingredients.len() {
        return;
    }

    // Resolve ingredients first
    for &(_, ingr) in ingredients {
        resolve_dependencies(ingr, deps, recipe);
    }

    // Trick to avoid cloning
    let mut dep = FixedBitSet::new();
    // Take bitset (because that one is initialized with the right capacity)
    std::mem::swap(&mut dep, &mut deps[chem]);

    // Depend on all the ingredients' dependencies
    dep = ingredients
        .iter()
        .map(|&(_, ingr)| &deps[ingr])
        .fold(dep, |mut dep, d| {
            dep.union_with(d);
            dep
        });
    // Depend on all the ingredients
    dep.extend(ingredients.iter().map(|&(_, ingr)| ingr));

    // Put it back
    std::mem::swap(&mut dep, &mut deps[chem]);
}

pub async fn day14(input: String) -> Result<(String, String)> {
    let mut chemical_ids = HashMap::new();
    let mut recipe = Vec::new();
    macro_rules! get_id {
        ($name:expr) => {
            *chemical_ids.entry($name).or_insert_with(|| {
                let id = recipe.len();
                recipe.push(Recipe::default());
                id
            })
        };
    }

    for line in input.trim().lines() {
        let (lhs, rhs) = line.split_once(" => ").ok_or_else(|| anyhow!("Bad line"))?;
        let (prod_count, product) = parse_chemical(rhs)?;
        let prod_id = get_id!(product);
        let ingredients = lhs
            .split(", ")
            .map(|s| {
                let (count, chem) = parse_chemical(s)?;
                let id = get_id!(chem);
                Ok((count, id))
            })
            .collect::<Result<Vec<(u64, usize)>>>()?;

        recipe[prod_id].produces = prod_count;
        recipe[prod_id].ingredients = ingredients;
    }

    let fuel = get_id!("FUEL");
    let chem_count = recipe.len();

    let mut dependencies = vec![FixedBitSet::with_capacity(chem_count); chem_count];
    for chem in 0..chem_count {
        resolve_dependencies(chem, &mut dependencies, &recipe);
    }

    let part1 = {
        let mut fuel_recipe = recipe[fuel].clone();
        fuel_recipe.flatten(&recipe, &dependencies);
        fuel_recipe.ingredients[0].0
    };

    let part2 = {
        let ore_amount = 1_000_000_000_000u64;

        // The true amount of ore pre fuel is rational, not an integer so we can guess a range of
        // maximum fuel from ore_amount
        let mut min = ore_amount / part1;
        let mut max = min * 2;

        // Binary search, I know there are better heuristics but this runs in ~200µs so I don't
        // really care
        while min < max {
            let count = (min + max).div_ceil(2);
            let mut fuel_recipe = recipe[fuel].clone();

            fuel_recipe.scale(count);
            fuel_recipe.flatten(&recipe, &dependencies);

            let ore_cost = fuel_recipe.ingredients[0].0;

            if ore_cost > ore_amount {
                max = count - 1;
            } else {
                min = count;
            }
        }

        min
    };

    Ok((part1.to_string(), part2.to_string()))
}

