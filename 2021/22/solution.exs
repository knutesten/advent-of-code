{:ok, contents} = File.read("input.txt")

steps = contents
|> String.trim
|> String.split("\n")
|> Enum.map(&String.split(&1, " "))
|> Enum.map(fn [a, b] ->
  {a, b
  |> String.split(",")
  |> Enum.map(fn s -> String.split(s, "=")
     |> List.last
     |> String.split("..")
     |> Enum.map(&String.to_integer/1)
   end)}
end)

defmodule Day22 do
  def overlap([a, b], [c, d]) do
    if d < a or c > b do
      nil
    else
      [max(a, c), min(b, d)]
    end
  end

  def remove_overlap(
    [[xa1, xa2], [ya1, ya2], [za1, za2]],
    [[xb1, xb2], [yb1, yb2], [zb1, zb2]]) do
    ox = overlap([xa1, xa2], [xb1, xb2])
    oy = overlap([ya1, ya2], [yb1, yb2])
    oz = overlap([za1, za2], [zb1, zb2])

    if ox == nil or oy == nil or oz == nil do
      [[[xa1, xa2], [ya1, ya2], [za1, za2]]]
    else
      [[[xa1, List.first(ox) - 1], oy, oz],
        [[List.last(ox) + 1, xa2], oy, oz],

        [[xa1, xa2], [ya1, ya2], [za1, List.first(oz) - 1]],
        [[xa1, xa2], [ya1, ya2], [List.last(oz) + 1, za2]],

        [[xa1, xa2], [ya1, List.first(oy) - 1], oz],
        [[xa1, xa2], [List.last(oy) + 1, ya2], oz]]
      |> Enum.reject(&Enum.any?(&1, fn [x, y] -> x > y end))
    end
  end

  def volume(cube) do
    cube
    |> Enum.map(fn [x, y] -> y - x + 1 end)
    |> Enum.product
  end

  def iterate(cubes, steps) do
    if Enum.empty?(steps) do
      cubes
      |> Enum.map(&volume/1)
      |> Enum.sum
    else
      [{state, cube} | steps] = steps
      cubes = cubes
        |> Enum.flat_map(&remove_overlap(&1, cube))

      if state == "on" do
        iterate([cube | cubes], steps)
      else
        iterate(cubes, steps)
      end
    end
  end
end

steps_50x50 = steps
|> Enum.filter(fn {_, cube} ->
  Enum.all?(cube, fn [x1, x2] ->
    x1 >= -50 and x2 <= 50
  end)
end)

IO.puts("Part 1: #{Day22.iterate([], steps_50x50)}")
IO.puts("Part 2: #{Day22.iterate([], steps)}")
