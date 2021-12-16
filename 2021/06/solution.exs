{:ok, contents} = File.read("input.txt")

init_state = contents
|> String.trim
|> String.split(",")
|> Enum.map(&String.to_integer/1)


defmodule Day6 do
  def iterate(state, goal) do
    iterate(Enum.frequencies(state), 0, goal)
  end

  def iterate(state, step, goal) do
    if step == goal do
      state
      |> Map.values
      |> Enum.sum
    else
      new_state = state
      |> Enum.map(fn {k, v} -> {k-1, v} end)
      |> Map.new

      new_state = new_state
      |> Map.put(6, Map.get(new_state, -1, 0) + Map.get(new_state, 6, 0))
      |> Map.put(8, Map.get(new_state, -1, 0))
      |> Map.delete(-1)

      iterate(new_state, step + 1, goal)
    end
  end
end

IO.puts("Part 1: #{Day6.iterate(init_state, 80)}")
IO.puts("Part 2: #{Day6.iterate(init_state, 256)}")

