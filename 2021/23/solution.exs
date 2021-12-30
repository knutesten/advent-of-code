{:ok, contents} = File.read("input.txt")

matrix = contents
|> String.trim
|> String.split("\n")
|> Enum.map(&String.split(&1, "", trim: true))

matrix_part_1 = matrix
|> List.insert_at(4, String.split("###A#B#C#D#", "", trim: true))
|> List.insert_at(4, String.split("###A#B#C#D#", "", trim: true))

matrix_part_2 = matrix
|> List.insert_at(3, String.split("###D#B#A#C#", "", trim: true))
|> List.insert_at(3, String.split("###D#C#B#A#", "", trim: true))

width = 0..(Enum.count(List.first(matrix_part_1)) - 1)
height = 0..(Enum.count(matrix_part_1) - 1)

prawns_part_1 = for x <- width, y <- height  do
  {[x, y], Enum.at(Enum.at(matrix_part_1, y), x)} end
  |> Enum.filter(fn {_, a} -> a in ["A", "B", "C", "D"] end)
  |> Map.new

prawns_part_2 = for x <- width, y <- height  do
  {[x, y], Enum.at(Enum.at(matrix_part_2, y), x)} end
  |> Enum.filter(fn {_, a} -> a in ["A", "B", "C", "D"] end)
  |> Map.new

defmodule PriorityQueue do
  def new do
    :gb_trees.empty
  end

  def add(tree, priority, element) do
    elems = :gb_trees.lookup(priority, tree)
    if elems == :none do
      :gb_trees.enter(priority, element, tree)
    else
      {_, coll} = elems
      coll = [element | coll]
      :gb_trees.update(priority, coll, tree)
    end
  end

  def smallest(tree) do
    {priority, element, tree} = :gb_trees.take_smallest(tree)
    if is_list(element) do
      [element | coll] = element
      {element, PriorityQueue.add(tree, priority, coll)}
    else
      {element, tree}
    end
  end
end

defmodule Day23 do
  @rooms %{
    "A" => [[3, 2], [3, 3], [3, 4], [3, 5]],
    "B" => [[5, 2], [5, 3], [5, 4], [5, 5]],
    "C" => [[7, 2], [7, 3], [7, 4], [7, 5]],
    "D" => [[9, 2], [9, 3], [9, 4], [9, 5]]}

  @hallway [[1, 1], [2, 1], [4, 1], [6, 1], [8, 1], [10, 1], [11, 1]]

  @cost %{"A" => 1, "B" => 10, "C" => 100, "D" => 1000}

  @goal %{
    [3, 2] => "A",
    [3, 3] => "A",
    [3, 4] => "A",
    [3, 5] => "A",
    [5, 2] => "B",
    [5, 3] => "B",
    [5, 4] => "B",
    [5, 5] => "B",
    [7, 2] => "C",
    [7, 3] => "C",
    [7, 4] => "C",
    [7, 5] => "C",
    [9, 2] => "D",
    [9, 3] => "D",
    [9, 4] => "D",
    [9, 5] => "D"}

  def cost(prawn, [x1, y1], [x2, y2]) do
    (abs(x2 - x1) + abs(y2 - 1) + abs(y1 - 1)) * @cost[prawn]
  end

  def path_open?(prawns, [x1, y1], [x2, y2]) do
    [x1, y1] != [x2, y2]
    and
    for x <- x1..x2, x != x1 do [x, 1] end
    ++
    for y <- 1..y1, y != y1 do [x1, y] end
    ++
    for y <- 1..y2 do [x2, y] end
    |> Enum.all?(&prawns[&1] == nil)
  end

  def available_room(prawns, prawn) do
    [a, b, c, d] = @rooms[prawn]
    case Enum.map([a, b, c, d], &prawns[&1]) do
      [nil, nil, nil, nil] -> [d]
      [nil, nil, nil, ^prawn] -> [c]
      [nil, nil, ^prawn, ^prawn] -> [b]
      [nil, ^prawn, ^prawn, ^prawn] -> [a]
      _ -> []
    end
  end

  def moves_prawn(prawns, {coor, prawn}) do
    rooms = available_room(prawns, prawn)
    if coor in @hallway do rooms else @hallway ++ rooms end
    |> Enum.filter(&path_open?(prawns, coor, &1))
    |> Enum.map(fn new_coor ->
      {cost(prawn, coor, new_coor),
       prawns
       |> Map.delete(coor)
       |> Map.put(new_coor, prawn)}
    end)
  end

  def moves(prawns) do
    prawns
    |> Enum.reject(fn {x, p} ->
      [a, b, c, d] = @rooms[p]
      r = Enum.map(@rooms[p], &prawns[&1])

      x == d and [p] == Enum.take(r, -1) or
      x == c and [p, p] == Enum.take(r, -2) or
      x == b and [p, p, p] == Enum.take(r, -3) or
      x == a and [p, p, p, p] == r
    end)
    |> Enum.flat_map(&moves_prawn(prawns, &1))
  end

  def heuristic(prawns) do
    prawns
    |> Enum.map(fn {c, p} ->
      cost(p, c, List.last(@rooms[p]))
    end)
    |> Enum.sum
  end

  def energy_required(state) do
    open_set = PriorityQueue.add(PriorityQueue.new, 0, state)
    a_star(open_set, %{state => 0}, %{})
  end

  def a_star(open_set, g_score, came_from) do
    {current, open_set} = PriorityQueue.smallest(open_set)
    if current == @goal do
      g_score[current]
    else
      neigh = moves(current)
      |> Enum.map(fn {cost, state} -> {g_score[current] + cost, state} end)
      |> Enum.filter(fn {s, c} -> g_score[c] == nil or s < g_score[c] end)
      came_from = Enum.reduce(neigh, came_from, fn {_, c}, acc ->
        Map.put(acc, c, current)
      end)
      g_score = Enum.reduce(neigh, g_score, fn {s, c}, acc ->
        Map.put(acc, c, s)
      end)
      open_set = Enum.reduce(neigh, open_set, fn {s, c}, acc ->
        PriorityQueue.add(acc, s + heuristic(c), c)
      end)
      a_star(open_set, g_score, came_from)
    end
  end
end

IO.puts("Part 1: #{Day23.energy_required(prawns_part_1)}")
IO.puts("Part 2: #{Day23.energy_required(prawns_part_2)}")
