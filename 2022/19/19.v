// vi:ft=vlang
module main
import net.http
import os
import benchmark
import math
import arrays

fn get_input() !string {
    session := os.read_lines('../../session')![0]
    config := http.FetchConfig {
	url: 'https://adventofcode.com/2022/day/19/input'
	method: .get
	cookies: { 'session': session }
	verbose: true
    }
    return http.fetch(config)!.body
}

struct Cost {
    ore u16
    clay u16
    obsidian u16
}

[heap]
struct BluePrint {
    index u16
    ore_robot Cost
    clay_robot Cost
    obsidian_robot Cost
    geode_robot Cost

    max_ore u16
    max_clay u16
    max_obsidian u16
    mut: 
    max_geode u16
}

fn max4(a u16, b u16, c u16, d u16) u16 {
    return math.max(a, math.max(b, math.max(c, d)))
}

fn (bp BluePrint) max() BluePrint {
    return BluePrint {
	...bp
	max_ore: max4(bp.ore_robot.ore, bp.clay_robot.ore, bp.obsidian_robot.ore, bp.geode_robot.ore)
	max_clay: max4(bp.clay_robot.clay, bp.clay_robot.clay, bp.obsidian_robot.clay, bp.geode_robot.clay)
	max_obsidian: max4(bp.obsidian_robot.obsidian, bp.clay_robot.obsidian, bp.obsidian_robot.obsidian, bp.geode_robot.obsidian)
    }
}

struct Inventory {
    mut:
    ore u16
    clay u16
    obsidian u16
    geode u16
    ore_robot u16 = 1
    clay_robot u16
    obsidian_robot u16
    geode_robot u16
}

fn (inv Inventory) update() Inventory {
    return Inventory {
	...inv
	ore: inv.ore + inv.ore_robot
	clay: inv.clay + inv.clay_robot
	obsidian: inv.obsidian + inv.obsidian_robot
	geode: inv.geode + inv.geode_robot
    }
}

fn (i Inventory) buy(c Cost, giveup bool) ?Inventory {
    if i.ore >= c.ore && i.clay >= c.clay && i.obsidian >= c.obsidian && !giveup {
	return Inventory {
	    ...i
	    ore: i.ore - c.ore
	    clay: i.clay - c.clay
	    obsidian: i.obsidian - c.obsidian
	}
    } else {
	return none
    }
}

fn run(inv Inventory, mut bp &BluePrint, t int) u16 {
    mut max := u16(0)

    theoretical_max := inv.geode + t * inv.geode_robot + t * (t - 1) / 2
    if theoretical_max <= bp.max_geode {
	return 0
    }

    if t == 0 {
	bp.max_geode = math.max(bp.max_geode, inv.geode)
	return inv.geode
    }

    new_t := t - 1
    if mut new_inv := inv.buy(bp.ore_robot, inv.ore_robot > bp.max_ore) {
	new_inv = new_inv.update()
	new_inv.ore_robot += 1
	r := run(new_inv, mut bp, new_t)
	if r > max {
	    max = r
	}
    }
    if mut new_inv := inv.buy(bp.clay_robot, inv.clay_robot > bp.max_clay) {
	new_inv = new_inv.update()
	new_inv.clay_robot += 1
	r := run(new_inv, mut bp, new_t)
	if r > max {
	    max = r
	}
    }
    if mut new_inv := inv.buy(bp.obsidian_robot, inv.obsidian_robot > bp.max_obsidian) {
	new_inv = new_inv.update()
	new_inv.obsidian_robot += 1
	r := run(new_inv, mut bp, new_t)
	if r > max {
	    max = r
	}
    }
    if mut new_inv := inv.buy(bp.geode_robot, false) {
	new_inv = new_inv.update()
	new_inv.geode_robot += 1
	r := run(new_inv, mut bp, new_t)
	if r > max {
	    max = r
	}
    }
    new_inv := inv.update()
    r := run(new_inv, mut bp, new_t)
    if r > max {
	max = r
    }

    bp.max_geode = math.max(bp.max_geode, max)
    return max
}

const parse_panic_msg = "Malformed input"

fn parse_input(input string) []BluePrint {
    return input.split_into_lines()
	.map(fn (line string) BluePrint {
	    fields := line.split_any(" :")
	    return BluePrint {
		index: u16(fields[1].parse_int(10, 16) or { panic(parse_panic_msg) })
		ore_robot: Cost { ore: u16(fields[7].parse_int(10, 16) or { panic(parse_panic_msg) }) }
		clay_robot: Cost { ore: u16(fields[13].parse_int(10, 16) or { panic(parse_panic_msg) }) }
		obsidian_robot: Cost {
		    ore: u16(fields[19].parse_int(10, 16) or { panic(parse_panic_msg) })
		    clay: u16(fields[22].parse_int(10, 16) or { panic(parse_panic_msg) })
		}
		geode_robot: Cost {
		    ore: u16(fields[28].parse_int(10, 16) or { panic(parse_panic_msg) })
		    obsidian: u16(fields[31].parse_int(10, 16) or { panic(parse_panic_msg) })
		}
	    }.max()
	})
}

fn quality_score(mut bp &BluePrint, mut bm &benchmark.Benchmark) u16 {
    res := run(Inventory {}, mut bp, 24) * bp.index
    println("[1] Done blueprint ${bp.index} (${res})")
    bm.step()
    bm.ok()
    return res
}

fn max_geode(mut bp &BluePrint, mut bm &benchmark.Benchmark) u32 {
    res := u32(run(Inventory {}, mut bp, 32))
    println("[2] Done blueprint ${bp.index} (${res})")
    bm.step()
    bm.ok()
    return res
}

fn solve_part1(str_input string) (u16, benchmark.Benchmark) {
    mut input := parse_input(str_input)

    mut bmark := benchmark.new_benchmark()
    bmark.set_total_expected_steps(input.len)
 
    println("[1] Start")

    mut threads := []thread u16{}
    for i in 0..input.len {
	unsafe {
	    threads << spawn quality_score(mut &input[i], mut &bmark)
	}
    }
    results := threads.wait()
    res := arrays.sum(results) or { u16(0) }

    println("[1] Finished\n")

    bmark.stop()
    return res, bmark
}

fn solve_part2(str_input string) (u32, benchmark.Benchmark) {
    mut input := parse_input(str_input)

    mut bmark := benchmark.new_benchmark()
    bmark.set_total_expected_steps(3)
 
    println("[2] Start")

    mut threads := []thread u32{}
    for i in 0..3 {
	unsafe {
	    threads << spawn max_geode(mut &input[i], mut &bmark)
	}
    }
    results := threads.wait()
    res := arrays.reduce(results, fn (a u32, b u32) u32 { return a * b }) or { 1 }

    println("[2] Finished\n")

    bmark.stop()

    return res, bmark
}

fn main() {
    input := get_input()!
    println("")

    part1, bench1 := solve_part1(input)
    part2, bench2 := solve_part2(input)
 
    print(bench1.total_message('Part1'))
    print(bench2.total_message('Part2'))
    println("")

    println("Part1: ${part1}")
    println("Part2: ${part2}")
}
