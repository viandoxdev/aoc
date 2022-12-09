const std = @import("std");
const c = @cImport({
    @cInclude("curl/curl.h");
    @cInclude("stddef.h");
});

const CurlBuf = struct {
    ptr: ?[]u8,
    alloc: std.mem.Allocator,
};

pub fn writefunc(ptr: [*]u8, size: usize, nmemb: usize, s: *CurlBuf) usize {
    const cur_size = if (s.ptr) |slice| slice.len else 0;
    const added = size * nmemb;
    const new_size: usize = cur_size + added;

    var new_ptr: []u8 = if (s.ptr) |sptr|
        s.alloc.realloc(sptr, new_size + 1) catch unreachable
    else
        s.alloc.alloc(u8, nmemb + 1) catch unreachable;

    std.mem.copy(u8, new_ptr[cur_size..new_size], ptr[0..added]);
    new_ptr[new_size] = 0;

    s.ptr = new_ptr;

    return added;
}

fn getSession() ![]u8 {
    var file = try std.fs.cwd().openFile("../session", .{});
    defer file.close();

    var buf: [256]u8 = undefined;
    var br = std.io.bufferedReader(file.reader());
    return try br.reader().readUntilDelimiterOrEof(&buf, '\n') orelse unreachable;
}

fn getInput() ![]u8 {
    _ = c.curl_global_init(c.CURL_GLOBAL_ALL);
    var curl: *c.CURL = c.curl_easy_init() orelse unreachable;
    const alloc = std.heap.c_allocator;
    var buf = CurlBuf{
        .ptr = null,
        .alloc = alloc,
    };

    const url: [*c]const u8 = "https://adventofcode.com/2022/day/9/input";
    var cookie_buf: [256]u8 = undefined;
    var cookie: []u8 = try std.fmt.bufPrint(&cookie_buf, "session={s}\x00", .{try getSession()});

    _ = c.curl_easy_setopt(curl, c.CURLOPT_WRITEFUNCTION, writefunc);
    _ = c.curl_easy_setopt(curl, c.CURLOPT_WRITEDATA, &buf);
    _ = c.curl_easy_setopt(curl, c.CURLOPT_COOKIE, cookie.ptr);
    _ = c.curl_easy_setopt(curl, c.CURLOPT_URL, url);

    var res: c.CURLcode = c.curl_easy_perform(curl);
    if (res != c.CURLE_OK) {
        unreachable;
    }

    _ = c.curl_easy_cleanup(curl);

    return buf.ptr orelse unreachable;
}

const Vec2 = struct {
    x: i32,
    y: i32,

    pub fn new(x: i32, y: i32) Vec2 {
        return Vec2{ .x = x, .y = y };
    }

    // Euclidean length function squared: x² + y²
    pub fn sqLen(self: Vec2) i32 {
        return self.x * self.x + self.y * self.y;
    }

    // Different length function: max(|x|, |y|)
    pub fn mxLen(self: Vec2) i32 {
        return std.math.max(std.math.absInt(self.x) catch unreachable, std.math.absInt(self.y) catch unreachable);
    }

    pub fn add(self: Vec2, rhs: Vec2) Vec2 {
        return Vec2{ .x = self.x + rhs.x, .y = self.y + rhs.y };
    }

    pub fn sub(self: Vec2, rhs: Vec2) Vec2 {
        return Vec2{ .x = self.x - rhs.x, .y = self.y - rhs.y };
    }
};

pub fn solve(comptime rope_len: usize, input: []u8) usize {
    const last = rope_len - 1;
    var knots = [1]Vec2{Vec2.new(0, 0)} ** rope_len;
    var visited = std.AutoHashMap(Vec2, void).init(std.heap.c_allocator);
    defer visited.deinit();

    visited.put(knots[last], {}) catch unreachable;

    var i: usize = 0;
    while (i + 2 < input.len) {
        // j is the index of the next new line (or one past the end of the input if none exist)
        var j: usize = i;
        while (j < input.len and input[j] != '\x00' and input[j] != '\n') {
            j += 1;
        }

        const dir = input[i];
        var amount = std.fmt.parseInt(i32, input[(i + 2)..j], 10) catch -1;
        const adjacent = [_]Vec2{
            Vec2.new(0, 1),
            Vec2.new(-1, 0),
            Vec2.new(0, -1),
            Vec2.new(1, 0),
            Vec2.new(-1, 1),
            Vec2.new(1, 1),
            Vec2.new(1, -1),
            Vec2.new(-1, -1),
        };
        const off = switch (dir) {
            'U' => adjacent[0],
            'L' => adjacent[1],
            'D' => adjacent[2],
            'R' => adjacent[3],
            else => continue,
        };

        while (amount > 0) : (amount -= 1) {
            knots[0] = knots[0].add(off);

            var k: usize = 1;
            while (k < rope_len) : (k += 1) {
                const prev = knots[k - 1];
                const curr = knots[k];

                const cur_prev_dist = prev.sub(curr).mxLen();
                if (cur_prev_dist >= 2) { // current isn't adjacent or in a direct diagonal of prev anymore
                    // The new cell the current knot should go to
                    var new_cell: ?Vec2 = null;
                    var least_len: i32 = std.math.maxInt(i32);
                    // Loop over all cells adjacent to prev, and find the one closest to current
                    for (adjacent) |o| {
                        const cell = prev.add(o);
                        const dist = cell.sub(curr).mxLen();

                        if (dist > 1) {
                            continue;
                        }

                        const length = o.sqLen();

                        if (length < least_len) {
                            least_len = length;
                            new_cell = cell;
                        }
                    }

                    knots[k] = new_cell orelse unreachable;
                }
            }

            visited.put(knots[last], {}) catch unreachable;
        }

        i = j + 1;
    }

    return visited.count();
}

pub fn main() !void {
    const input = try getInput();
    const part1 = solve(2, input);
    const part2 = solve(10, input);

    std.debug.print("Part1: {}\n", .{part1});
    std.debug.print("Part2: {}\n", .{part2});
}
