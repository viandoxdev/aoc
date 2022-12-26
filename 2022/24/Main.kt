import java.io.File
import java.io.FileNotFoundException
import java.net.URI
import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpResponse.BodyHandlers

const val WIDTH = 120
const val HEIGHT = 25
const val LCM = 600

fun fetchInput(): String {
    val session = File("../../session").readLines().first()
    val req = HttpRequest.newBuilder()
        .GET()
        .uri(URI("https://adventofcode.com/2022/day/24/input"))
        .header("Cookie", "session=$session")
        .build()
    return HttpClient.newHttpClient()
        .send(req, BodyHandlers.ofString())
        .body()
}

fun getInput(): String {
    val file = File("input")
    return try {
        file.readText()
    } catch (_: FileNotFoundException) {
        val input = fetchInput()
        file.writeText(input)
        input
    }
}

data class Vec2(var x: Int = 0, var y: Int = 0) {
    operator fun plus(o: Vec2) = Vec2(x + o.x, y + o.y)
}

enum class Direction {
    North, West, South, East;

    fun toVec2(): Vec2 = when (this) {
        North -> Vec2(0, -1)
        West -> Vec2(-1, 0)
        South -> Vec2(0, 1)
        East -> Vec2(1, 0)
    }

    fun toChar(): Char = when (this) {
        North -> '^'
        West -> '<'
        South -> 'v'
        East -> '>'
    }

    companion object {
        fun fromChar(c: Char): Direction? = when (c) {
            '^' -> North
            '<' -> West
            'v' -> South
            '>' -> East
            else -> null
        }
    }
}

data class Blizzard(var pos: Vec2, val dir: Direction)

class State {
    // Grid of booleans, true -> tile is empty, false -> blizzard is on tile
    // Used for O(1) access
    private var grid: Array<Array<Boolean>> = Array(HEIGHT + 2) { Array(WIDTH + 2) { true } }

    // List of blizzards
    private var blizzards: MutableList<Blizzard> = mutableListOf()

    fun parse(input: String) {
        val lines = input.split("\n").slice(1..HEIGHT)
        for ((y, line) in lines.withIndex()) {
            for ((x, c) in line.slice(1..WIDTH).withIndex()) {
                val dir = Direction.fromChar(c) ?: continue
                blizzards.add(Blizzard(Vec2(x, y), dir))
            }
        }

        fillGrid()
    }

    // Clear the grid, set all tiles to empty
    private fun clearGrid() {
        for (row in grid) {
            row.fill(true)
        }
        grid[0].fill(false)
        grid[HEIGHT + 1].fill(false)
        grid.forEach { it[0] = false; it[WIDTH + 1] = false }

        grid[0][1] = true
        grid[HEIGHT + 1][WIDTH] = true
    }

    // Fill in the grid with the blizzards
    private fun fillGrid() {
        clearGrid()
        for (blizzard in blizzards) {
            grid[blizzard.pos.y + 1][blizzard.pos.x + 1] = false
        }
    }

    private fun isFree(tile: Vec2): Boolean {
        if (tile.x < -1 || tile.y < -1 || tile.x > WIDTH || tile.y > HEIGHT) return false
        return grid[tile.y + 1][tile.x + 1]
    }

    private fun step() {
        for (blizzard in blizzards) {
            blizzard.pos = wrap(blizzard.pos + blizzard.dir.toVec2())
        }
        fillGrid()
    }

    fun show(pos: List<Vec2>? = null) {
        val lines = List(HEIGHT + 2) { CharArray(WIDTH + 2) { '.' } }

        // top row
        lines[0].fill('#')
        lines[0][1] = '.'
        // left and right columns
        lines.forEach { it[0] = '#'; it[WIDTH + 1] = '#' }
        // bottom row
        lines[HEIGHT + 1].fill('#')
        lines[HEIGHT + 1][WIDTH] = '.'

        for (blizzard in blizzards) {
            val c = lines[blizzard.pos.y + 1][blizzard.pos.x + 1]
            lines[blizzard.pos.y + 1][blizzard.pos.x + 1] = if (c == '.') {
                blizzard.dir.toChar()
            } else if (c.isDigit()) {
                (c.digitToInt() + 1).digitToChar()
            } else {
                '2'
            }
        }

        if (pos != null) {
            for (p in pos) {
                lines[p.y + 1][p.x + 1] = 'E'
            }
        }

        lines.forEach { println(it) }
    }

    fun bfs(from: Vec2, goal: Vec2): Int {
        var queue = ArrayDeque<Vec2>()
        var newQueue = ArrayDeque<Vec2>()
        val visited = HashMap<Pair<Vec2, Int>, Int>()

        val neighbours = arrayOf(Vec2(0, -1), Vec2(-1, 0), Vec2(0, 1), Vec2(1, 0), Vec2(0, 0))
        var minute = 0
        queue.addLast(from)

        while (queue.isNotEmpty()) {
            step()
            minute++
            while (queue.isNotEmpty()) {
                val v = queue.removeFirst()

                neighbours.map { it + v }
                    .filter(::isFree)
                    .forEach {
                        val key = Pair(it, (minute + 1) % LCM)
                        if (visited.getOrDefault(key, Int.MAX_VALUE) > minute + 1) {
                            visited[key] = minute + 1
                            newQueue.addLast(it)

                            if (it == goal) {
                                return minute
                            }
                        }
                    }
            }

            // Double buffering: swap queues
            queue = newQueue.also { newQueue = queue }
        }

        throw Exception("Goal never reached")
    }

    companion object {
        fun wrap(p: Vec2): Vec2 = when {
            p.x < 0 -> Vec2(WIDTH - 1, p.y)
            p.x >= WIDTH -> Vec2(0, p.y)
            p.y < 0 -> Vec2(p.x, HEIGHT - 1)
            p.y >= HEIGHT -> Vec2(p.x, 0)
            else -> p
        }
    }
}

fun main() {
    val input = getInput()
    val state = State()
    state.parse(input)

    val start = Vec2(0, -1)
    val end = Vec2(WIDTH - 1, HEIGHT)

    val part1 = state.bfs(start, end)
    val roundTrip = state.bfs(end, start)
    val roundRoundTrip = state.bfs(start, end)

    val part2 = part1 + roundTrip + roundRoundTrip

    println()
    println("Part1: $part1")
    println("Part2: $part2 ($part1 + $roundTrip + $roundRoundTrip)")
}
