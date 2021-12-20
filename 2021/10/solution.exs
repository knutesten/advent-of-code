{:ok, contents} = File.read("input.txt")

lines = contents
|> String.split("\n", trim: true)
|> Enum.map(&String.split(&1, "", trim: true))

defmodule Day10 do
  def find_corruption(line) do
    find_corruption(line, [])
  end

  def mapping(c) do
    case c do
      "<" -> ">"
      "[" -> "]"
      "{" -> "}"
      "(" -> ")"
    end
  end

  def find_corruption([], opens) do
    Enum.map(opens, &mapping/1)
  end

  def find_corruption(line, opens) do
    [h | t] = line
    open = List.first(opens)
    cond do
      h in ["<", "{", "[", "("] -> find_corruption(t, [h] ++ opens)
      h != mapping(open) -> h
      true -> find_corruption(t, Enum.drop(opens, 1))
    end
  end

  def score_part_2(l) do
    score_map = %{
      ")" => 1,
      "]" => 2,
      "}" => 3,
      ">" => 4
    }
    Enum.map(l, &score_map[&1])
    |> Enum.reduce(0, fn x, acc -> acc * 5 + x end)
  end

  def score_part_1(c) do
    case c do
      ")" -> 3
      "]" -> 57
      "}" -> 1197
      ">" -> 25137
    end
  end
end

{corrupted, incomplete} = lines
|> Enum.map(&Day10.find_corruption/1)
|> Enum.split_with(& !is_list(&1))

score_part_1 = corrupted
|> Enum.map(&Day10.score_part_1/1)
|> Enum.sum

score_part_2 = incomplete
|> Enum.map(&Day10.score_part_2/1)
|> Enum.sort
|> Enum.at(trunc((Enum.count(incomplete) - 1)/2))

IO.puts("Part 1: #{score_part_1}")
IO.puts("Part 2: #{score_part_2}")

