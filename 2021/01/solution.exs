{:ok, contents} = File.read("input.txt")

defmodule Day1 do
  def count_prev_smaller(coll) do
    [_|rest] = coll
    Enum.zip_reduce(coll, rest, 0, fn
      a, b, acc when a < b -> acc + 1
      _, _, acc -> acc
    end)
  end
end

lines = contents
|> String.split("\n", trim: true)
|> Enum.map(&String.to_integer/1)

chunks = Enum.chunk_every(lines, 3, 1, :discard)
|> Enum.map(&Enum.sum/1)

IO.puts("Part 1: #{Day1.count_prev_smaller(lines)}")
IO.puts("Part 2: #{Day1.count_prev_smaller(chunks)}")

