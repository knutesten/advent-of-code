{:ok, contents} = File.read("input.txt")

[template_str, rules_str] = contents
|> String.split("\n\n")

template_list = template_str
|> String.split("", trim: true)

edges = [List.first(template_list), List.last(template_list)]

template = template_list
|> Enum.chunk_every(2, 1, :discard)
|> Enum.map(&Enum.join(&1, ""))
|> Enum.frequencies

rules = rules_str
|> String.split("\n", trim: true)
|> Enum.map(&String.split(&1, " -> "))
|> Enum.map(
fn [a, b] ->
  [x, y] = String.split(a, "", trim: true)
  {a, Enum.map([[x, b], [b, y]], &Enum.join(&1, ""))}
end)
|> Map.new

defmodule Day14 do
  def score(template, [f, l]) do
    counts = template
    |> Enum.map(fn {r, c} -> {String.split(r, "", trim: true), c} end)
    |> Enum.flat_map(fn {ls, c} -> Enum.map(ls, & {&1, c / 2}) end)
    |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
    |> Enum.map(fn {r, cs} -> {r, Enum.sum(cs)} end)
    |> Enum.map(fn {r, c} -> {r, if r == f do c + 0.5 else c end} end)
    |> Enum.map(fn {r, c} -> {r, if r == l do c + 0.5 else c end} end)
    |> Enum.map(&trunc(elem(&1, 1)))
    |> Enum.sort

    List.last(counts) - List.first(counts)
  end

  def iterate(rules, template, steps, edges) do
    if steps == 0 do
      score(template, edges)
    else
      template = template
      |> Enum.flat_map(fn {r, c} -> Enum.map(rules[r], & {&1, c}) end)
      |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
      |> Enum.map(fn {r, cs} -> {r, Enum.sum(cs)} end)
      iterate(rules, template, steps - 1, edges)
    end
  end
end

IO.puts("Part 1: #{Day14.iterate(rules, template, 10, edges)}")
IO.puts("Part 2: #{Day14.iterate(rules, template, 40, edges)}")

