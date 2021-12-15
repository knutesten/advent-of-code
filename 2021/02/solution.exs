{:ok, lines} = File.read("input.txt")

input = lines
|> String.split("\n", trim: true)
|> Enum.map(&String.split/1)
|> Enum.map(fn [a, b] -> [a, String.to_integer(b)] end)

parts = input
|> Enum.group_by(&List.first/1, &(Enum.at(&1, 1)))
|> Enum.into(%{}, fn {k, v} -> {k, Enum.sum(v)} end)


defmodule Day2 do
  def part_2(insts) do
    Day2.part_2(insts, 0, 0, 0)
  end

  def part_2(insts, aim, depth, horiz) do
    case insts do
      [["down", value] | rest]-> Day2.part_2(rest, aim + value, depth, horiz)
      [["up", value] | rest] -> Day2.part_2(rest, aim - value, depth, horiz)
      [["forward", value] | rest] ->
        Day2.part_2(rest, aim, depth + aim * value, horiz + value)
      [] -> depth * horiz
    end
  end
end

IO.puts("Part 1: #{(parts["down"] - parts["up"]) * parts["forward"]}")
IO.puts("Part 2: #{Day2.part_2(input)}")

