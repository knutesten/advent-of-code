{:ok, lines} = File.read("input.txt")

input = lines
|> String.split("\n", trim: true)
|> Enum.map(&String.graphemes/1)

gamma = input
|> Enum.zip_with(& &1)
|> Enum.map(& elem(Enum.max_by(Enum.frequencies(&1), fn {_, c} -> c end), 0))

epsilon = gamma
|> Enum.map(& case &1 do
                "0"-> "1"
                "1"-> "0" end)

gamma = Enum.join(gamma, "") |> Integer.parse(2) |> elem(0)
epsilon = Enum.join(epsilon, "") |> Integer.parse(2) |> elem(0)


defmodule Day3 do
  def frequency(coll, pos) do
    freq = coll
    |> Enum.map(&Enum.at(&1, pos))
    |> Enum.frequencies

    {Map.get(freq, "0", 0), Map.get(freq, "1", 0)}
  end

  def filter_most_common(coll, pos) do
    {a, b} = frequency(coll, pos)
    val = cond do
      a == b -> "1"
      a > b -> "0"
      a < b -> "1"
    end
    Enum.filter(coll, &Enum.at(&1, pos) == val)
  end

  def filter_least_common(coll, pos) do
    {a, b} = frequency(coll, pos)
    val = cond do
      a == b -> "0"
      a > b -> "1"
      a < b -> "0"
    end
    Enum.filter(coll, &Enum.at(&1, pos) == val)
  end

  def o2([a], _, _) do
    a
  end

  def o2(nrs, pos, f) do
    o2(f.(nrs, pos), pos + 1, f)
  end

  def life_support_rating(nrs) do
    a = Enum.join(o2(nrs, 0, &filter_least_common/2), "") |> Integer.parse(2) |> elem(0)
    b = Enum.join(o2(nrs, 0, &filter_most_common/2), "") |> Integer.parse(2) |> elem(0)
    a * b
  end
end

IO.inspect("Part 1: #{gamma * epsilon}")
IO.inspect("Part 2: #{Day3.life_support_rating(input)}")

