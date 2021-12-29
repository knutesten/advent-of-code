{:ok, contents} = File.read("input.txt")

[alg_str, pic_str] = contents
|> String.trim
|> String.split("\n\n")

alg = alg_str
|> String.split("", trim: true)

pic_matrix = pic_str
|> String.split("\n")
|> Enum.map(&String.split(&1, "", trim: true))

width = Enum.count(List.first(pic_matrix))
height = Enum.count(pic_matrix)

pic = for x <- 0..(width - 1), y <- 0..(height - 1) do [x, y] end
|> Enum.filter(fn [x, y] -> Enum.at(Enum.at(pic_matrix, y), x) == "#" end)
|> MapSet.new

defmodule Day20 do
  def minmax(coll, f) do
    a = f.(List.first(coll))
    Enum.reduce(coll, [a, a], fn x, [min, max] ->
      [if f.(x) < min do f.(x) else min end,
      if f.(x) > max do f.(x) else max end]
    end)
  end

  def lit_px?(alg, pic, [x, y]) do
    index = for yn <- (y - 1)..(y + 1), xn <- (x - 1)..(x + 1) do [xn, yn] end
    |> Enum.map(& if MapSet.member?(pic, &1) do "1" else "0" end)
    |> Enum.join("")
    |> String.to_integer(2)

    Enum.at(alg, index) == "#"
  end

  def count_px(pic, alg, steps) do
    [w1, w2] = minmax(Enum.to_list(pic), &List.first/1)
    [h1, h2] = minmax(Enum.to_list(pic), &List.last/1)
    offset = steps
    w_offset = (w1 - offset - steps)..(w2 + offset + steps)
    h_offset = (h1 - offset - steps)..(h2 + offset + steps)
    w = (w1 - steps)..(w2 + steps)
    h = (h1 - steps)..(h2 + steps)
    iterate(pic, alg, w_offset, h_offset, steps)
    |> Enum.filter(fn [x, y] -> x in w and y in h end)
    |> Enum.count
  end

  def iterate(pic, alg, w, h, steps) do
    if steps == 0 do
      pic
    else
      pic = for x <- w, y <- h do [x, y] end
      |> Enum.filter(&lit_px?(alg, pic, &1))
      |> MapSet.new

      iterate(pic, alg, w, h, steps - 1)
    end
  end
end

IO.puts("Part 1: #{Day20.count_px(pic, alg, 2)}")
IO.puts("Part 2: #{Day20.count_px(pic, alg, 50)}")

