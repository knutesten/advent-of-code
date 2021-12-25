{:ok, contents} = File.read("input.txt")

defmodule Day18 do
  def side_of_rec(tree, path, side) do
    v = lookup(tree, path)
    if !is_list(v) do
      {v, path}
    else
      if side == 0 do
        side_of_rec(tree, path ++ [1], side)
      else
        side_of_rec(tree, path ++ [0], side)
      end
    end
  end

  def side_of(tree, path, side) do
    if Enum.all?(path, & &1 == side) do
      {0, nil}
    else
      new_path = path
      |> Enum.reverse
      |> Enum.drop_while(& &1 == side)
      |> Enum.drop(1)
      |> Enum.reverse
      side_of_rec(tree, new_path ++ [side], side)
    end
  end

  def explode(tree) do
    pair_to_explode = find(tree, [], fn p, v ->
      Enum.count(p) == 4 and is_list(v)
    end)

    if pair_to_explode == nil do
      tree
    else
      {[l, r], path} = pair_to_explode
      {l_val, l_path} = side_of(tree, path, 0)
      {r_val, r_path} = side_of(tree, path, 1)

      tree = tree
      |> replace([], l_path, l + l_val)
      |> replace([], r_path, r + r_val)
      |> replace([], path, 0)

      explode(tree)
    end
  end

  def lookup(tree, path) do
    cond do
      path == [] -> tree
      !is_list(tree) -> nil
      true ->
        [d | path] = path
        [a, b] = tree
        if d == 0 do
          lookup(a, path)
        else
          lookup(b, path)
        end
    end
  end

  def find(tree, path, cond_fn) do
    cond do
      cond_fn.(path, tree) and is_list(tree)-> {tree, path}
      cond_fn.(path, tree) -> {tree, path}
      is_list(tree) ->
        [a, b] = tree
        v = find(a, path ++ [0], cond_fn)
        if v do v else find(b, path ++ [1], cond_fn) end
      true -> nil
    end
  end

  def replace(tree, curr_path, path, replacement) do
    cond do
      path == nil -> tree
      path == curr_path -> replacement
      is_list(tree) ->
        [a, b] = tree
        [replace(a, curr_path ++ [0], path, replacement),
         replace(b, curr_path ++ [1], path, replacement)]
      true -> tree
    end
  end

  def split(tree) do
    pair_to_split = find(tree, [], fn _, v ->
      !is_list(v) and v > 9
    end)

    if pair_to_split do
      {nr, path} = pair_to_split
      l = trunc(nr / 2)
      r = round(nr / 2)
      replace(tree, [], path, [l, r])
    else
      tree
    end
  end

  def sum_rec(tree) do
    tree_e = explode(tree)
    tree_s = split(tree_e)
    if tree_s == tree do
      tree
    else
      sum_rec(tree_s)
    end
  end

  def sum(a, b) do
    tree = [a, b]
    sum_rec(tree)
  end

  def magnitude([a, b]) do
    3 * magnitude(a) + 2 * magnitude(b)
  end

  def magnitude(x) do
    x
  end
end

snail_nrs = contents
|> String.trim
|> String.split("\n")
|> Enum.map(&elem(Code.eval_string(&1), 0))

part_1 = snail_nrs
|> Enum.reduce(fn l, acc -> Day18.sum(acc, l) end)
|> Day18.magnitude

part_2 = for nr1 <- snail_nrs, nr2 <- snail_nrs, nr1 != nr2 do
  [nr1, nr2]
end
|> Enum.map(fn [a, b] -> Day18.sum(a,b) end)
|> Enum.map(&Day18.magnitude/1)
|> Enum.sort
|> List.last

IO.puts("Part 1: #{part_1}")
IO.puts("Part 2: #{part_2}")

