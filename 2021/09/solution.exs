{:ok, contents} = File.read("input.txt")

matrix = contents
|> String.split("\n", trim: true)
|> Enum.map(&String.split(&1, "", trim: true))
|> Enum.map(&Enum.map(&1, fn s -> String.to_integer(s) end))

defmodule Day9 do
  def valid_coor?(matrix, [x, y]) do
    height = Enum.count(matrix)
    width = Enum.count(List.first(matrix))
    0 <= x and x < width and 0 <= y and y < height
  end

  def adjacent(matrix, [x, y]) do
    [[x - 1, y], [x + 1, y], [x, y - 1], [x, y + 1]]
    |> Enum.filter(& valid_coor?(matrix, &1))
  end

  def get_2d(matrix, [x, y]) do
    Enum.at(Enum.at(matrix, y), x)
  end

  def low_point?(matrix, coor) do
    v = get_2d(matrix, coor)
    adjacent(matrix, coor)
    |> Enum.map(& get_2d(matrix, &1))
    |> Enum.all?(& v < &1)
  end

  def basin(matrix, coor) do
    basin(matrix, [coor], [])
  end

  def basin(matrix, coors, visited) do
    [coor | coors] = coors
    visited = [coor] ++ visited

    coors = coors ++ adjacent(matrix, coor)
    |> Enum.filter(& &1 not in visited)
    |> Enum.filter(& get_2d(matrix, &1) != 9)

    if Enum.empty?(coors) do
      visited
    else
      basin(matrix, coors, visited)
    end
  end
end

height = Enum.count(matrix)
width = Enum.count(List.first(matrix))
sum_low_points = for x <- 0..(height - 1), y <- 0..(width - 1) do [x, y] end
|> Enum.filter(& Day9.low_point?(matrix, &1))
|> Enum.map(& Day9.get_2d(matrix, &1) + 1)
|> Enum.sum

basins_multiplied = for x <- 0..(height - 1), y <- 0..(width - 1) do [x, y] end
|> Enum.filter(& Day9.low_point?(matrix, &1))
|> Enum.map(& Day9.basin(matrix, &1))
|> Enum.map(&Enum.count/1)
|> Enum.sort
|> Enum.take(-3)
|> Enum.product

IO.puts("Part 1: #{sum_low_points}")
IO.puts("Part 2: #{basins_multiplied}")

