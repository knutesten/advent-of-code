{:ok, contents} = File.read("input.txt")

defmodule Day13 do
  def dimensions(points) do
    width = points
    |> Enum.max_by(&List.first/1)
    |> List.first

    height = points
    |> Enum.max_by(&List.last/1)
    |> List.last

    {width, height}
  end

  def print(points) do
    {width, height} = dimensions(points)
    for y <- 0..height do
      for x <- 0..width do
        IO.write(if [x, y] in points do "#" else " " end)
      end
      IO.puts("")
    end
  end

  def fold_horizontal(points, fold_y) do
    points
    |> Enum.map(fn [x, y] ->
      if y > fold_y do [x, -y + 2 * fold_y]
      else [x, y] end
    end)
    |> MapSet.new
  end

  def fold_vertical(points, fold_x) do
    points
    |> Enum.map(fn [x, y] ->
      if x > fold_x do [-x + 2 * fold_x, y]
      else [x, y] end
    end)
    |> MapSet.new
  end

  def fold(points, []) do
    points
  end

  def fold(points, instructions) do
    [[dir, i] | instructions] = instructions
    if dir == "x" do
      fold(fold_vertical(points, i), instructions)
    else
      fold(fold_horizontal(points, i), instructions)
    end
  end
end

[points_str, instructions_str] = contents
|> String.split("\n\n", trim: true)

points = points_str
|> String.split("\n", trim: true)
|> Enum.map(&String.split(&1, ",", trim: true))
|> Enum.map(&Enum.map(&1, fn s -> String.to_integer(s) end))
|> MapSet.new

instructions = instructions_str
|> String.split("\n", trim: true)
|> Enum.map(&String.split(&1, "=", trim: true))
|> Enum.map(fn [dir, nr] ->
  [List.last(String.codepoints(dir)), String.to_integer(nr)] end)

point_count = points
|> Day13.fold(Enum.take(instructions, 1))
|> Enum.count

kO.puts("Part 1: #{point_count}")
IO.puts("Part 2:")
Day13.print(Day13.fold(points, instructions))

