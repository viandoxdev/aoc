import fs from 'node:fs/promises';

(async () => {
    let meta;
    let start;
    const namesi = [];
    const names = new Map();
    {
        const session = (await fs.readFile("../../session")).toString().trim();
        const input = await (await fetch("https://adventofcode.com/2022/day/16/input", { headers: { Cookie: `session=${session}` } })).text();
        const parsed = input.trim().split("\n")
            .map(l => l.match(/^.+([A-Z]{2}).+=(\d+).+[es] ([A-Z ,]+)$/))
            .map(m => [m[1], {rate: parseInt(m[2]), tunnels: m[3].split(", ")}]);
        const of = (n) => {
            if(names.has(n)) {
                return names.get(n);
            } else {
                names.set(n, names.size);
                namesi[names.size - 1] = n;
                return of(n);
            }
        }
        meta = parsed.map(([n, o]) => [of(n), o])
            .sort((a, b) => a[0] - b[0])
            .map(v => v[1]);
        start = of("AA");
    }

    const dist = new Array(meta.length).fill(0).map((_, i) => {
        const res = new Array(meta.length).fill(Infinity);
        res[i] = 0;
        for(const t of meta[i].tunnels) {
            res[names.get(t)] = 1;
        }
        return res;
    });

    for(const k in meta) {
        for(const i in meta) {
            for(const j in meta) {
                const d = dist[i][k] + dist[k][j];
                if (dist[i][j] > d) {
                    dist[i][j] = d;
                }
            }
        }
    }

    {
        const state = { closed: new Set(meta.map((v, i) => [v, i]).filter(v => v[0].rate > 0).map(([_, i]) => i)), pressure: 0, current: start, path: `${start}` };
        const results = [];
        const sim = async (min, state) => {
            if(min === 0) {
                results.push(state);
                return;
            }
            let branches = 0;
            await Promise.all(Array.from(state.closed).map(async k => {
                const cost = dist[state.current][k] + 1; // cost of opening valve
                // If the cost is too high, return
                if(cost > min) return;
                const closed = new Set(state.closed);
                const pressure = state.pressure + (min - cost) * meta[k].rate;
                closed.delete(k);
                await sim(min - cost, { closed, pressure, current: k, path: state.path + ` => ${k} (${pressure} ${min - cost})` });
                branches++;
            }));

            if(branches === 0) {
                await sim(0, state);
            }
        };

        await sim(30, state);

        results.sort((a, b) => b.pressure - a.pressure);
        console.log(results[0].pressure, results[0].path);
    }

    {
        const mem = new Map();
        const fun = (cur, closed, min, part2) => {
            const key = `${cur}:${Array.from(closed).join(",")}:${min}:${part2 ? "T" : "F"}`;
            if(mem.has(key)) return mem.get(key);

            let max = 0;
            for(const k of closed) {
                const rest = new Set(closed);
                rest.delete(k);
                const nmin = min - dist[cur][k] - 1;
                if(nmin < 0) continue;
                const flow = meta[k].rate * nmin + fun(k, rest, nmin, part2);
                max = Math.max(flow, max);
            }
            if(part2) {
                max = Math.max(fun(start, closed, 26, false), max);
            }
            mem.set(key, max);
            return max;
        }
        const closed = new Set(meta.map((v, i) => [v, i]).filter(v => v[0].rate > 0).map(([_, i]) => i));
        const res = fun(start, closed, 26, true)
        console.log(res);
    }
})();
