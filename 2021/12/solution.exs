{:ok, contents} = File.read("input.txt")

defmodule Day12 do
  def small?(s) do
    s == String.downcase(s)
  end

  def small_two_times(path) do
    if path
    |> Enum.filter(&small?/1)
    |> Enum.frequencies
    |> Enum.any?(& elem(&1, 1) >= 2) do
      Enum.filter(path, &small?/1)
    else
      ["start"]
    end
  end

  def paths_part_1(neighbours) do
    paths(neighbours, [["start"]], [], &Enum.filter(&1, fn x -> small?(x) end))
  end

  def paths_part_2(neighbours) do
    paths(neighbours, [["start"]], [], &small_two_times/1)
  end

  def paths(_, [], finished_paths, _) do
    finished_paths
  end

  def paths(neighbours, queue, finished_paths, alg) do
    [p | queue] = queue
    n = List.last(p)
    if n == "end" do
      paths(neighbours, queue, [p | finished_paths], alg)
    else
      new_paths = neighbours[n]
      |> Enum.filter(& &1 not in alg.(p))
      |> Enum.map(& p ++ [&1])

      paths(neighbours, new_paths ++ queue, finished_paths, alg)
    end
  end
end

neighbours = contents
|> String.split("\n", trim: true)
|> Enum.map(&String.split(&1, "-", trim: true))
|> Enum.flat_map(fn [x, y] -> [[x, y], [y, x]] end)
|> Enum.group_by(&List.first/1, &List.last/1)

IO.puts("Part 1: #{Enum.count(Day12.paths_part_1(neighbours))}")
IO.puts("Part 2: #{Enum.count(Day12.paths_part_2(neighbours))}")

