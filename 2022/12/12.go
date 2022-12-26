package main

import (
	"container/heap"
	"fmt"
	"io"
	"log"
	"math"
	"net/http"
	"os"
	"strings"
)

func getSession() (res string) {
    dat, err := os.ReadFile("../../session")
    res = strings.Trim(string(dat), "\n")

    if err != nil {
        log.Panicln("Couldn't read session file")
    }

    return
}

func getInput(session string) string {
    req, _ := http.NewRequest("GET", "https://adventofcode.com/2022/day/12/input", nil)
    req.Header.Add("Cookie", "session=" + session)
    resp, err := http.DefaultClient.Do(req)

    if err != nil {
        log.Panicln("Error with request")
    }

    defer resp.Body.Close()
    body, err := io.ReadAll(resp.Body)

    if err != nil {
        log.Panicln("Error with response body")
    }

    return string(body)
}

type vec2 struct {
    x int
    y int
}

func parseInput(input string) (res [][]uint8, start vec2, end vec2) {
    lines := strings.Split(input, "\n")
    lines = lines[:len(lines) - 1]
    
    height := len(lines)
    width := len(lines[0])

    res = make([][]uint8, height)
    for y := 0; y < height; y++ {
        res[y] = make([]uint8, width)
        for x := 0; x < width; x++ {
            char := lines[y][x]
            height := char - 'a'

            if char == 'S' {
                start = vec2{x: x, y: y}
                height = 0
            } else if char == 'E' {
                end = vec2{x: x, y: y}
                height = 25
            }

            res[y][x] = height
        }
    }

    return
}

type item struct {
    node vec2
    dist int
}

type itemHeap []item

func (h itemHeap) Len() int           { return len(h) }
func (h itemHeap) Less(i, j int) bool { return h[i].dist < h[j].dist }
func (h itemHeap) Swap(i, j int)      { h[i], h[j] = h[j], h[i] }

func (h *itemHeap) Push(x any) {
	*h = append(*h, x.(item))
}

func (h *itemHeap) Pop() any {
	old := *h
	n := len(old)
	x := old[n-1]
	*h = old[0 : n-1]
	return x
}

type zst struct{}

func reconstructPath(from map[vec2]vec2, node vec2) []vec2 {
    path := make([]vec2, 0)
    exists := true
    for exists {
        path = append(path, node)
        node, exists = from[node]
    }

    // reverse path
    for i, j := 0, len(path)-1; i < j; i, j = i+1, j-1 {
        path[i], path[j] = path[j], path[i]
    }

    return path
}

func pathFind(hm [][]uint8, start vec2, goal vec2) []vec2 {
    width := len(hm[0])
    height := len(hm)
    
    dist := make(map[vec2]int)
    prev := make(map[vec2]vec2)
    open := make(map[vec2]zst)
    queu := make(itemHeap, 0, 1000)
    heap.Init(&queu)

    dist[start] = 0
    open[start] = zst{}
    heap.Push(&queu, item{node: start, dist: 0})

    for queu.Len() > 0 {

        u := heap.Pop(&queu).(item).node
        delete(open, u)

        if u == goal {
            return reconstructPath(prev, u)
        }

        neighbours := []vec2{
            {x: u.x - 1, y: u.y},
            {x: u.x + 1, y: u.y},
            {x: u.x, y: u.y - 1},
            {x: u.x, y: u.y + 1},
        }

        for i := range neighbours {
            v := neighbours[i]

            if v.x < 0 || v.y < 0 || v.x >= width || v.y >= height {
                continue
            }

            delta := int(hm[v.y][v.x]) - int(hm[u.y][u.x])
            if delta > 1 {
                continue
            }

            alt := dist[u] + 1
            d, exists := dist[v]
            if alt < d || !exists {
                dist[v] = alt
                prev[v] = u

                _, contains := open[v]
                if !contains {
                    heap.Push(&queu, item{node: v, dist: alt})
                    open[v] = zst{}
                }
            }
        }
    }

    return []vec2{}
}

func main() {
    session := getSession()
    input := getInput(session)
    hm, start, end := parseInput(input)

    part1 := len(pathFind(hm, start, end)) - 1

    list := make([]vec2, 0, 10)
    for y := range hm {
        for x := range hm[y] {
            if hm[y][x] != 0 {
                continue
            }

            list = append(list, vec2{x: x, y: y})
        }
    }

    part2 := math.MaxInt
    for i := range list {
        source := list[i]
        steps := len(pathFind(hm, source, end)) - 1
        if steps > 0 && steps < part2 {
            part2 = steps
        }
    }

    fmt.Println("(1) Steps: ", part1)
    fmt.Println("(2) Steps: ", part2)
}
