{:ok, contents} = File.read("input.txt")

target = contents
|> String.trim
|> String.split(", ")
|> Enum.map(&String.split(&1, "="))
|> Enum.map(&List.last/1)
|> Enum.map(&String.split(&1, ".."))
|> Enum.map(&Enum.map(&1, fn s -> String.to_integer(s) end))

defmodule Day17 do
  def next_pos([x, y], [dx, dy]) do
    ddx = cond do
      dx == 0 -> 0
      dx < 0 -> 1
      dx > 0 -> -1
    end
    {[x + dx, y + dy], [dx + ddx, dy - 1]}
  end

  def hits?(target, vel) do
    hits?(target, vel, [0, 0], 0)
  end

  def hits?(target, vel, coor, hy) do
    hy = max(hy, Enum.at(coor, 1))
    cond do
      on_target?(coor, target) -> hy
      past_target?(coor, target) -> nil
      true ->
        {p, v} = next_pos(coor, vel)
        hits?(target, v, p, hy)
    end
  end

  def on_target?([x,y], [[x_min, x_max], [y_min, y_max]]) do
    x_min <= x and x <= x_max and y_min <= y and y <= y_max
  end

  def past_target?([x, y], [[_, x_max], [y_min, _]]) do
    x_max < x or y < y_min
  end
end

highest = for x <- 1..50, y <- 1..250 do [x, y] end
|> Enum.map(&Day17.hits?(target, &1))
|> Enum.filter(& &1 != nil)
|> Enum.sort
|> List.last

count = for x <- 1..100, y <- -250..250 do [x, y] end
|> Enum.filter(&Day17.hits?(target, &1))
|> Enum.count

IO.puts("Part 1: #{highest}")
IO.puts("Part 2: #{count}")

