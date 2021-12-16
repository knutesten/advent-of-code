{:ok, contents} = File.read("input.txt")

inputs_outputs = contents
|> String.split("\n", trim: true)
|> Enum.map(&String.split(&1, " | "))
|> Enum.map(fn l -> Enum.map(l, &String.split(&1, " ")) end)
|> Enum.map(fn l -> Enum.map(l, fn l2 -> Enum.map(l2, &Enum.join(Enum.sort(String.split(&1, "", trim: true)), "")) end) end)

count_part_1 = inputs_outputs
|> Enum.map(&List.last/1)
|> List.flatten
|> Enum.filter(fn w -> String.length(w) in [2, 7, 4, 3] end)
|> Enum.count

defmodule Day5 do
  def contains(s1, s2) do
    MapSet.subset?(
      MapSet.new(String.split(s2, "", trim: true)),
      MapSet.new(String.split(s1, "", trim: true)))
  end

  def decode([input, output]) do
    one = Enum.find(input, & String.length(&1) == 2)
    four = Enum.find(input, & String.length(&1) == 4)
    seven = Enum.find(input, & String.length(&1) == 3)
    eight = Enum.find(input, & String.length(&1) == 7)
    three = Enum.find(input, & String.length(&1) == 5 and contains(&1, seven))
    six = Enum.find(input, & String.length(&1) == 6 and !contains(&1, seven))
    nine = Enum.find(input, & String.length(&1) == 6 and contains(&1, three))
    zero = Enum.find(input, & String.length(&1) == 6 and &1 != nine and &1 != six)
    five = Enum.find(input, & String.length(&1) == 5 and contains(six, &1))
    two = Enum.find(input, & String.length(&1) == 5 and &1 != three and &1 != five)

    mapping = %{
      zero => 0,
      one => 1,
      two => 2,
      three => 3,
      four => 4,
      five => 5,
      six => 6,
      seven => 7,
      eight => 8,
      nine => 9
    }

    output
    |> Enum.map(& mapping[&1])
    |> Enum.join("")
    |> String.to_integer
  end
end

sum_outputs = inputs_outputs
|> Enum.map(&Day5.decode/1)
|> Enum.sum

IO.puts("Part 1: #{count_part_1}")
IO.puts("Part 2: #{sum_outputs}")

