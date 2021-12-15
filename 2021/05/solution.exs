defmodule Day5 do
  def count_intersections(lines) do
    lines
    |> Enum.flat_map(&Day5.coors/1)
    |> Enum.frequencies
    |> Enum.filter(& elem(&1, 1) > 1)
    |> Enum.count
  end

  def coors([[x1, y1], [x2, y2]]) do
    dx = x2 - x1
    dy = y2 - y1
    [[dir_x, dir_y], steps] = if x1 == x2 or y1 == y2 do
      if dx == 0 do
        [[0, trunc(dy/abs(dy))], abs(dy)]
      else
        [[trunc(dx/abs(dx)), 0], abs(dx)]
      end
    else
      [[trunc(dx/abs(dx)), trunc(dy/abs(dy))], abs(dx)]
    end

    for s <- 0..steps do [x1 + s * dir_x, y1 + s * dir_y] end
  end
end

{:ok, contents} = File.read("input.txt")

{straight, diagonal} = contents
|> String.split("\n", trim: true)
|> Enum.map(&String.split(&1, " -> "))
|> Enum.map(&Enum.map(&1, fn s -> String.split(s, ",") end))
|> Enum.map(&Enum.map(&1, fn l -> Enum.map(l, fn s -> String.to_integer(s) end) end))
|> Enum.split_with(fn [[x1, y1], [x2, y2]] -> x1 == x2 or y1 == y2 end)

straight_intersection_count = Day5.count_intersections(straight)
intersection_count = Day5.count_intersections(diagonal ++ straight)

IO.puts("Part 1: #{straight_intersection_count}")
IO.puts("Part 2: #{intersection_count}")

