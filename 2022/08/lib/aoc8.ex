defmodule Aoc8 do
  use Application

  def visibility_map(mat, direction) do
    trans =
      if direction == :down or direction == :up,
        do: &Matrex.transpose/1,
        else: &Function.identity/1

    rev =
      if direction == :up or direction == :left, do: &Enum.reverse/1, else: &Function.identity/1

    mat
    |> then(trans)
    |> Matrex.to_list_of_lists()
    |> Enum.map(fn line ->
      line
      |> then(rev)
      |> Enum.map_reduce(-1, fn el, acc -> {if(el > acc, do: 1, else: 0), max(el, acc)} end)
      |> elem(0)
      |> then(rev)
    end)
    |> Matrex.new()
    |> then(trans)
  end

  def scenic_score(mat, x, y) do
    col = Matrex.column_to_list(mat, x)
    row = Matrex.row_to_list(mat, y)
    cur = Matrex.at(mat, y, x)

    w = Enum.count(row)
    h = Enum.count(col)

    up = Enum.slice(col, 0, y - 1) |> Enum.reverse()
    down = Enum.slice(col, y, h - y)
    right = Enum.slice(row, 0, x - 1) |> Enum.reverse()
    left = Enum.slice(row, x, w - x)

    fun = fn el, acc -> if el < cur, do: {:cont, acc + 1}, else: {:halt, acc + 1} end

    up = up |> Enum.reduce_while(0, fun)
    down = down |> Enum.reduce_while(0, fun)
    right = right |> Enum.reduce_while(0, fun)
    left = left |> Enum.reduce_while(0, fun)

    up * down * right * left
  end

  def visibility(mat) do
    mat
    |> Aoc8.visibility_map(:up)
    |> Matrex.add(Aoc8.visibility_map(mat, :right))
    |> Matrex.add(Aoc8.visibility_map(mat, :down))
    |> Matrex.add(Aoc8.visibility_map(mat, :left))
  end

  def start(_type, _args) do
    {:ok, session} = File.read("../../session")
    session = String.trim(session, "\n")

    {:ok, res} =
      HTTPoison.get("https://adventofcode.com/2022/day/8/input", Cookie: "session=" <> session)

    mat =
      res.body
      |> String.split("\n", trim: true)
      |> Enum.map(fn
        line ->
          line
          |> String.split("", trim: true)
          |> Enum.map(&String.to_integer/1)
      end)
      |> Matrex.new()

    vis = Aoc8.visibility(mat)
    scen = Matrex.apply(mat, fn _, row, col -> Aoc8.scenic_score(mat, col, row) end)

    visible_count =
      vis
      |> Matrex.to_list()
      |> Enum.filter(fn x -> x >= 1.0 end)
      |> Enum.count()

    max_scen =
      scen
      |> Matrex.to_list()
      |> Enum.max()

    IO.inspect(visible_count, label: "Result 1")
    IO.inspect(max_scen |> trunc, label: "Result 2")

    Task.start(fn -> IO.puts("Finished") end)
  end
end
