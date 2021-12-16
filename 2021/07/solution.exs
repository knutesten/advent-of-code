{:ok, contents} = File.read("input.txt")

positions = contents
|> String.trim
|> String.split(",", trim: true)
|> Enum.map(&String.to_integer/1)

fuel_cost_part_1 = Enum.to_list(0..Enum.max(positions))
|> Enum.map(&Enum.map(positions, fn p -> abs(p - &1) end))
|> Enum.map(&Enum.sum/1)
|> Enum.min

fuel_cost_part_2 = Enum.to_list(0..Enum.max(positions))
|> Enum.map(&Enum.map(positions, fn p -> trunc(abs(p - &1) * (abs(p-&1) + 1) / 2) end))
|> Enum.map(&Enum.sum/1)
|> Enum.min


IO.puts("Part 1: #{fuel_cost_part_1}")
IO.puts("Part 1: #{fuel_cost_part_2}")

