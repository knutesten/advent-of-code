{:ok, contents} = File.read("input.txt")

hex_to_bin = %{
  "0" => "0000",
  "1" => "0001",
  "2" => "0010",
  "3" => "0011",
  "4" => "0100",
  "5" => "0101",
  "6" => "0110",
  "7" => "0111",
  "8" => "1000",
  "9" => "1001",
  "A" => "1010",
  "B" => "1011",
  "C" => "1100",
  "D" => "1101",
  "E" => "1110",
  "F" => "1111"
}

defmodule Day16 do
  def to_int(coll) do
    String.to_integer(Enum.join(coll, ""), 2)
  end

  def decode_value_rec(msg, value) do
    [x, a, b, c, d | msg] = msg
    if x == "0" do
      {msg, value ++ [a, b, c, d]}
    else
      decode_value_rec(msg, value ++ [a, b, c, d])
    end
  end

  def decode_value(msg) do
    {msg, value_coll} = decode_value_rec(msg, [])
    {msg, to_int(value_coll)}
  end

  def decode_for_length(len, msg, vals) do
    if len == 0 do
      {msg, vals}
    else
      len_before = length(msg)
      {msg, pkg} = decode(msg)
      diff = len_before - length(msg)
      decode_for_length(len - diff, msg, vals ++ [pkg])
    end
  end

  def decode(msg) do
    [v1, v2, v3, t1, t2, t3 | msg] = msg
    p = %{
      :version => to_int([v1, v2, v3]),
      :type => to_int([t1, t2, t3])
    }

    if p.type == 4 do
      {msg, value} = decode_value(msg)
      {msg, Map.put(p, :value, value)}
    else
      [i | msg] = msg
      if i == "1" do
        l = to_int(Enum.take(msg, 11))
        msg = Enum.drop(msg, 11)
        {msg, sub_packets} = List.duplicate(0, l)
        |> Enum.reduce({msg, []}, fn _, {msg, sp} -> {}
          {msg, pkg} = decode(msg)
          {msg, sp ++ [pkg]}
        end)
        {msg, Map.put(p, :sub_packets, sub_packets)}
      else
        l = to_int(Enum.take(msg, 15))
        msg = Enum.drop(msg, 15)
        {msg, sub_packets} = decode_for_length(l, msg, [])
        {msg, Map.put(p, :sub_packets, sub_packets)}
      end
    end
  end

  def sum_versions(pkg) do
    sum = pkg.version
    if pkg[:sub_packets] == nil do
      sum
    else
      sum + Enum.sum(Enum.map(pkg.sub_packets, &sum_versions/1))
    end
  end

  def pkg_value(pkg) do
    case pkg.type do
      0 -> Enum.sum(Enum.map(pkg.sub_packets, &pkg_value/1))
      1 -> Enum.product(Enum.map(pkg.sub_packets, &pkg_value/1))
      2 -> Enum.min(Enum.map(pkg.sub_packets, &pkg_value/1))
      3 -> Enum.max(Enum.map(pkg.sub_packets, &pkg_value/1))
      4 -> pkg.value
      5 ->
        [a, b] = Enum.map(pkg.sub_packets, &pkg_value/1)
        if a > b do 1 else 0 end
      6 ->
        [a, b] = Enum.map(pkg.sub_packets, &pkg_value/1)
        if a < b do 1 else 0 end
      7 ->
        [a, b] = Enum.map(pkg.sub_packets, &pkg_value/1)
        if a === b do 1 else 0 end
    end
  end
end

message = contents
|> String.trim
|> String.split("", trim: true)
|> Enum.map(&hex_to_bin[&1])
|> Enum.flat_map(&String.split(&1, "", trim: true))

pkg = elem(Day16.decode(message), 1)
IO.puts("Part 1: #{Day16.sum_versions(pkg)}")
IO.puts("Part 2: #{Day16.pkg_value(pkg)}")

