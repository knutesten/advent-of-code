{:ok, contents} = File.read("input.txt")

defmodule Day11 do
  def adjacent([x, y]) do
    for dx <- -1..1, dy <- -1..1 do [x + dx, y + dy] end
    |> Enum.filter(fn [x, y] -> 0 <= x and x < 10 and 0 <= y and y < 10 end )
  end

  def inc_adjacent(matrix, coor) do
    adjacent(coor)
    |> Enum.reduce(matrix, fn c, m -> Map.update!(m, c, & &1 + 1) end)
  end

  def iterate(matrix) do
    matrix = for x <- 0..9, y <- 0..9 do [x, y] end
    |> Enum.reduce(matrix, fn c, m -> Map.update!(m, c, & &1 + 1) end)

    iterate(matrix, [])
  end

  def iterate(matrix, flashed) do
    to_flash = for x <- 0..9, y <- 0..9 do [x, y] end
    |> Enum.filter(& &1 not in flashed)
    |> Enum.filter(& Map.get(matrix, &1) > 9)

    matrix = to_flash
    |> Enum.reduce(matrix, fn c, m -> inc_adjacent(m, c) end)

    flashed = flashed ++ to_flash

    if Enum.empty?(to_flash) do
      matrix = for x <- 0..9, y <- 0..9 do [x, y] end
      |> Enum.reduce(matrix, fn c, m ->
        Map.update!(m, c, & if &1 > 9 do 0 else &1 end) end)

      {Enum.count(flashed), matrix}
    else
      iterate(matrix, flashed)
    end
  end

  def find_first_simultaneous_flash(matrix) do
    find_first_simultaneous_flash(matrix, 0)
  end

  def find_first_simultaneous_flash(matrix, count) do
    count = count + 1
    {_, matrix} = iterate(matrix)

    if 0 == Enum.sum(Map.values(matrix))  do
      count
    else
      find_first_simultaneous_flash(matrix, count)
    end
  end
end

matrix_2d = contents
|> String.split("\n", trim: true)
|> Enum.map(&String.split(&1, "", trim: true))
|> Enum.map(&Enum.map(&1, fn c -> String.to_integer(c) end))

matrix = for x <- 0..9, y <- 0..9 do [x, y] end
|> Enum.reduce(%{}, fn [x, y], m -> Map.put(m, [x, y], Enum.at(Enum.at(matrix_2d, y), x)) end)

flashes = Enum.reduce(1..100, {0, matrix},
  fn _, {count, matrix} ->
    {new_count, new_matrix} = Day11.iterate(matrix)
    {new_count + count, new_matrix}
  end)
|> elem(0)

IO.puts("Part 1: #{flashes}")
IO.puts("Part 2: #{Day11.find_first_simultaneous_flash(matrix)}")

