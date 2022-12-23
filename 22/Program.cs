using System.Net.Http;
using System.Text.RegularExpressions;

enum Tile {
    None,
    Free,
    Wall,
}

enum Direction {
    East = 0,
    South = 1,
    West = 2,
    North = 3,
}

abstract record Event {}
record Advance(int amount): Event;
record LeftTurn(): Event;
record RightTurn(): Event;

struct Vec2 {
    public int x;
    public int y;

    public Vec2(int x, int y) {
        this.x = x;
        this.y = y;
    }

    static public Vec2 fromDirection(Direction dir) {
        switch(dir) {
            case Direction.North:
                return new Vec2(0, -1);
            case Direction.West:
                return new Vec2(-1, 0);
            case Direction.South:
                return new Vec2(0, 1);
            case Direction.East:
                return new Vec2(1, 0);
        }
        throw new ArgumentException("dir is invalid", "dir");
    }

    static public Vec2 fromTuple((int, int) tuple) {
        return new Vec2(tuple.Item1, tuple.Item2);
    }

    public Vec2 add(Vec2 o) {
        return new Vec2(x + o.x, y + o.y);
    }

    public Vec2 scale(int o) {
        return new Vec2(x * o, y * o);
    }
}

class Input {
    Tile[][] grid2d = {};
    Tile[,,] grid3d = new Tile[50,50,6];
    Vec2[] facePos = new Vec2[6];
    (int, Direction, Func<int, int, (int, int)>)[,] faceAdj = new (int, Direction, Func<int, int, (int, int)>)[6,4];
    Event[] path = {}; 

    private void parseFace(string[] lines, int face, Vec2 pos) {
        pos = pos.scale(50);
        facePos[face] = pos;
        for(int y = 0; y < 50; y++) {
            for(int x = 0; x < 50; x++) {
                char c = lines[y + pos.y][x + pos.x];
                switch(c) {
                    case '.':
                        grid3d[x,y,face] = Tile.Free;
                        break;
                    case '#':
                        grid3d[x,y,face] = Tile.Wall;
                        break;
                }
            }
        }
    }

    public void parse(string input) {
        string[] lines = input.Split('\n');

        grid2d = new Tile[lines.Length - 3][];

        ArraySegment<string> map = new ArraySegment<string>(lines, 0, lines.Length - 3);
        for(int y = 0; y < lines.Length - 3; y++) {
            Tile[] row = new Tile[map[y].Length];
            for(int x = 0; x < map[y].Length; x++) {
                switch(map[y][x]) {
                    case ' ':
                        row[x] = Tile.None;
                        break;
                    case '.':
                        row[x] = Tile.Free;
                        break;
                    case '#':
                        row[x] = Tile.Wall;
                        break;
                }
            }
            grid2d[y] = row;
        }

        // This may be different for each input, but I don't want to write the code to parses arbitrary unwrapping of a cube
        // Faces are as such on mine:
        //                       ┌───┬───┬───┐
        //     ┌───┐             │   │ 0 │ 1 │
        //     │ 5 │             ├───┼───┼───┤
        // ┌───┼───┼───┐ ┌───┐   │   │ 2 │   │
        // │ 3 │ 0 │ 1 │ │ 4 │   ├───┼───┼───┤
        // └───┼───┼───┘ └───┘   │ 3 │ 4 │   │
        //     │ 2 │             ├───┼───┼───┤
        //     └───┘             │ 5 │   │   │
        //                       └───┴───┴───┘

        parseFace(lines, 0, new Vec2(1, 0));
        parseFace(lines, 1, new Vec2(2, 0));
        parseFace(lines, 2, new Vec2(1, 1));
        parseFace(lines, 3, new Vec2(0, 2));
        parseFace(lines, 4, new Vec2(1, 2));
        parseFace(lines, 5, new Vec2(0, 3));

        // Adjacency
        faceAdj[0, (int)Direction.North] = (5, Direction.East,  (x, y) => (0, x));
        faceAdj[0, (int)Direction.West]  = (3, Direction.East,  (x, y) => (0, 49 - y));
        faceAdj[0, (int)Direction.South] = (2, Direction.South, (x, y) => (x, 0)); 
        faceAdj[0, (int)Direction.East]  = (1, Direction.East,  (x, y) => (0, y));

        faceAdj[1, (int)Direction.North] = (5, Direction.North, (x, y) => (x, 49));
        faceAdj[1, (int)Direction.West]  = (0, Direction.West,  (x, y) => (49, y)); 
        faceAdj[1, (int)Direction.South] = (2, Direction.West,  (x, y) => (49, x));
        faceAdj[1, (int)Direction.East]  = (4, Direction.West,  (x, y) => (49, 49 - y));

        faceAdj[2, (int)Direction.North] = (0, Direction.North, (x, y) => (x, 49)); 
        faceAdj[2, (int)Direction.West]  = (3, Direction.South, (x, y) => (y, 0));
        faceAdj[2, (int)Direction.South] = (4, Direction.South, (x, y) => (x, 0)); 
        faceAdj[2, (int)Direction.East]  = (1, Direction.North, (x, y) => (y, 49));

        faceAdj[3, (int)Direction.North] = (2, Direction.East,  (x, y) => (0, x));
        faceAdj[3, (int)Direction.West]  = (0, Direction.East,  (x, y) => (0, 49 - y));
        faceAdj[3, (int)Direction.South] = (5, Direction.South, (x, y) => (x, 0)); 
        faceAdj[3, (int)Direction.East]  = (4, Direction.East,  (x, y) => (0, y));

        faceAdj[4, (int)Direction.North] = (2, Direction.North, (x, y) => (x, 49));
        faceAdj[4, (int)Direction.West]  = (3, Direction.West,  (x, y) => (49, y));
        faceAdj[4, (int)Direction.South] = (5, Direction.West,  (x, y) => (49, x));
        faceAdj[4, (int)Direction.East]  = (1, Direction.West,  (x, y) => (49, 49 - y));

        faceAdj[5, (int)Direction.North] = (3, Direction.North, (x, y) => (x, 49));
        faceAdj[5, (int)Direction.West]  = (0, Direction.South, (x, y) => (y, 0));
        faceAdj[5, (int)Direction.South] = (1, Direction.South, (x, y) => (x, 0));
        faceAdj[5, (int)Direction.East]  = (4, Direction.North, (x, y) => (y, 49));
        
        string pathStr = lines[lines.Length - 2];
        path = Array.ConvertAll<string, Event>(
            Regex.Split(pathStr, "([LR])"),
            s => {
                switch(s) {
                case "L":
                    return new LeftTurn();
                case "R":
                    return new RightTurn();
                default:
                    return new Advance(Int32.Parse(s));
                }
            }
        );
    }

    private int firstOfRow(int y) {
        for(int i = 0; i < grid2d[y].Length; i++) {
            if(grid2d[y][i] != Tile.None) {
                return i;
            }
        }
        return -1;
    }

    private int lastOfRow(int y) {
        return grid2d[y].Length - 1;
    }

    private int firstOfCol(int x) {
        for(int i = 0; i < grid2d.Length; i++) {
            if(grid2d[i][x] != Tile.None) {
                return i;
            }
        }
        return -1;
    }

    private int lastOfCol(int x) {
        for(int i = grid2d.Length - 1; i >= 0; i--) {
            if(x < grid2d[i].Length && grid2d[i][x] != Tile.None) {
                return i;
            }
        }
        return -1;
    }

    private Direction rotateRight(Direction dir) {
        switch(dir) {
            case Direction.North:
                return Direction.East;
            case Direction.East:
                return Direction.South;
            case Direction.South:
                return Direction.West;
            case Direction.West:
                return Direction.North;
        }
        throw new ArgumentException("dir is invalid", "dir");
    }

    private Direction rotateLeft(Direction dir) {
        switch(dir) {
            case Direction.North:
                return Direction.West;
            case Direction.West:
                return Direction.South;
            case Direction.South:
                return Direction.East;
            case Direction.East:
                return Direction.North;
        }
        throw new ArgumentException("dir is invalid", "dir");
    }

    private Direction rotate(Event e, Direction dir) {
        if(e is LeftTurn) {
            return rotateLeft(dir);
        } else if(e is RightTurn) {
            return rotateRight(dir);
        } else {
            return dir;
        }
    }

    private Boolean inBounds(Vec2 pos) {
        return pos.y >= 0 && pos.y < grid2d.Length && pos.x >= 0 && pos.x < grid2d[pos.y].Length;
    }

    private Tile get(Vec2 pos) {
        if(inBounds(pos)) {
            return grid2d[pos.y][pos.x];
        } else {
            return Tile.None;
        }
    }

    private Vec2 advance2d(Vec2 pos, Direction dir, int amount) {
        Vec2 v = Vec2.fromDirection(dir);
        Vec2 newp;
        while(amount-- > 0) {
            newp = pos.add(v);
            if(get(newp).Equals(Tile.None)) {
                switch (dir) {
                    case Direction.North:
                        newp.y = lastOfCol(newp.x);
                        break;
                    case Direction.West:
                        newp.x = lastOfRow(newp.y);
                        break;
                    case Direction.South:
                        newp.y = firstOfCol(newp.x);
                        break;
                    case Direction.East:
                        newp.x = firstOfRow(newp.y);
                        break;
                }
            }

            if(get(newp) == Tile.Wall) {
                break;
            }

            pos = newp;
        }

        return pos;
    }

    private (Vec2, Direction, int) advance3d(Vec2 pos, int face, Direction dir, int amount) {
        Vec2 v = Vec2.fromDirection(dir);
        Vec2 newp;
        int newf = face;
        Direction newd = dir;
        while(amount-- > 0) {
            newp = pos.add(v);
            if(newp.x < 0 || newp.y < 0 || newp.x >= 50 || newp.y >= 50) {
                (newf, newd, Func<int, int, (int, int)> coords) = faceAdj[face, (int)dir];
                newp = Vec2.fromTuple(coords(pos.x, pos.y));
            }

            if(grid3d[newp.x, newp.y, newf] == Tile.Wall) {
                break;
            }

            pos = newp;
            face = newf;
            dir = newd;
            v = Vec2.fromDirection(dir);
        }

        return (pos, dir, face);
    }

    public int part1() {
        Vec2 pos;
        pos.x = firstOfRow(0);
        pos.y = 0;

        Direction dir = Direction.East;

        foreach (Event e in path) {
            Advance? a = e as Advance;
            if(a != null) {
                pos = advance2d(pos, dir, a.amount);
            } else {
                dir = rotate(e, dir);
            }
        }

        return 1000 * (pos.y + 1) + 4 * (pos.x + 1) + (int)dir;
    }

    public int part2() {
        int face = 0;
        Vec2 pos;
        pos.x = 0;
        pos.y = 0;

        Direction dir = Direction.East;

        foreach (Event e in path) {
            Advance? a = e as Advance;
            if(a != null) {
                (pos, dir, face) = advance3d(pos, face, dir, a.amount);
            } else {
                dir = rotate(e, dir);
            }
        }
        
        int x = pos.x + 1 + facePos[face].x;
        int y = pos.y + 1 + facePos[face].y;
        return 1000 * y + 4 * x + (int)dir;
    }
}

class Program {
    static async Task<string> FetchInput() {
        string session = (await File.ReadAllLinesAsync("../session"))[0];
        HttpClient client = new HttpClient();
        HttpRequestMessage req = new HttpRequestMessage(HttpMethod.Get, "https://adventofcode.com/2022/day/22/input");
        req.Headers.Add("Cookie", $"session={session}");
        return await (await client.SendAsync(req)).Content.ReadAsStringAsync();
    }

    static async Task<string> Input() {
        try {
            return await File.ReadAllTextAsync("input");
        } catch(FileNotFoundException) {
            string input = await FetchInput();
            await File.WriteAllTextAsync("input", input);
            return input;
        }
    }

    async static Task Main() {
        string textInput = await Input();
        Input input = new Input();
        input.parse(textInput);

        int part1 = input.part1();
        int part2 = input.part2();

        Console.WriteLine($"Part1: {part1}");
        Console.WriteLine($"Part2: {part2}");
    }
}
