{:ok, contents} = File.read("input.txt")

defmodule Day15 do
  def valid_coor?(matrix, [x, y], factor) do
    height = Enum.count(matrix) * factor
    width = Enum.count(List.first(matrix)) * factor
    0 <= x and x < width and 0 <= y and y < height
  end

  def adjacent(matrix, [x, y], factor) do
    [[x - 1, y], [x + 1, y], [x, y - 1], [x, y + 1]]
    |> Enum.filter(& valid_coor?(matrix, &1, factor))
  end

  def get_2d(matrix, [x, y]) do
    height = Enum.count(matrix)
    width = Enum.count(List.first(matrix))
    val = Enum.at(Enum.at(matrix, rem(y, height)), rem(x, width))
    val = val + div(x, width) + div(y, height) - 1
    1 + rem(val, 9)
  end

  def dijkstra(matrix, factor) do
    height = Enum.count(matrix)
    width = Enum.count(List.first(matrix))
    dijkstra(
      matrix,
      [width * factor - 1, height * factor - 1],
      [[0, 0]],
      %{[0, 0] => 0},
      %{},
      &adjacent(&1, &2, factor))
  end

  def calc_score(matrix, came_from, current, path) do
    next = came_from[current]
    if next == nil do
      path
      |> Enum.map(&get_2d(matrix, &1))
      |> Enum.drop(1)
      |> Enum.sum
    else
      calc_score(matrix, came_from, next, [next | path])
    end
  end

  def dijkstra(matrix, goal, open_set, g_score, came_from, neighbours_fn) do
    [current | open_set] = Enum.sort_by(open_set, &g_score[&1])
    if goal == current do
      calc_score(matrix, came_from, current, [current])
    else
      neigh = neighbours_fn.(matrix, current)
      |> Enum.map(& {g_score[current] + get_2d(matrix, &1), &1})
      |> Enum.filter(fn {s, c} -> g_score[c] == nil or s < g_score[c] end)
      came_from = Enum.reduce(neigh, came_from, fn {_, c}, acc ->
        Map.put(acc, c, current)
      end)
      g_score = Enum.reduce(neigh, g_score, fn {s, c}, acc ->
        Map.put(acc, c, s)
      end)
      open_set = Enum.map(neigh, &elem(&1, 1)) ++ open_set
      dijkstra(matrix, goal, open_set, g_score, came_from, neighbours_fn)
    end
  end
end

matrix = contents
|> String.split("\n", trim: true)
|> Enum.map(&String.split(&1, "", trim: true))
|> Enum.map(&Enum.map(&1, fn x -> String.to_integer(x) end))

IO.inspect(Day15.dijkstra(matrix, 1))
IO.inspect(Day15.dijkstra(matrix, 5))
