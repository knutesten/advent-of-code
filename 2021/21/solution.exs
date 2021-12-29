{:ok, contents} = File.read("input.txt")

[p1_start, p2_start] = contents
|> String.trim
|> String.split("\n")
|> Enum.map(& String.to_integer(List.last(String.split(&1, ": "))))

defmodule Day21 do
  def move(pos, roll) do
    roll = rem(roll, 10)
    pos = pos + roll
    if pos >= 11 do pos - 10 else pos end
  end

  def play(p1_start, p2_start) do
    play(p1_start, p2_start, 0, 0, 0)
  end

  def play(p1_pos, p2_pos, p1_score, p2_score, turn) do
    roll = turn * 6 * 3 + 1 + 2 + 3
    p1_pos = move(p1_pos, roll)
    p1_score = p1_score + p1_pos
    if p1_score >= 1000 do
      p2_score * (turn * 6 + 3)
    else
      roll = turn * 6 * 3 + 4 + 5 + 6
      p2_pos = move(p2_pos, roll)
      p2_score = p2_score + p2_pos
      if p2_score >= 1000 do
        p1_score * (turn * 6 + 6)
      else
        play(p1_pos, p2_pos, p1_score, p2_score, turn + 1)
      end
    end
  end

  def roll_dice({pos, score}) do
    for d1 <- 1..3, d2 <- 1..3, d3 <- 1..3 do
      pos = move(pos, d1 + d2 + d3)
      score = score + pos
      {pos, score}
    end
  end

  def play_p2(p1_pos, p2_pos) do
    play_p2([{{{p1_pos, 0}, {p2_pos, 0}}, 1}], 0, 0)
  end

  def play_p2(games, p1_wins, p2_wins) do
    {wins, games} = games
    |> Enum.flat_map(fn {{p1, p2}, nr} ->
      Enum.map(roll_dice(p1), & {{&1, p2}, nr})
    end)
    |> Enum.split_with(fn {{{_, s}, _}, _} -> s >= 21 end)
    p1_wins = p1_wins + Enum.reduce(wins, 0, fn {_, nr}, acc -> nr + acc end)
    games = games
    |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
    |> Enum.map(& {elem(&1, 0), Enum.sum(elem(&1, 1))})

    {wins, games} = games
    |> Enum.flat_map(fn {{p1, p2}, nr}->
      Enum.map(roll_dice(p2), & {{p1, &1}, nr})
    end)
    |> Enum.split_with(fn {{_, {_, s}}, _} -> s >= 21 end)
    p2_wins = p2_wins + Enum.reduce(wins, 0, fn {_, nr}, acc -> nr + acc end)
    games = games
    |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
    |> Enum.map(& {elem(&1, 0), Enum.sum(elem(&1, 1))})

    if Enum.empty?(games) do
      max(p1_wins, p2_wins)
    else
      play_p2(games, p1_wins, p2_wins)
    end
  end
end

IO.puts("Part 1: #{Day21.play(p1_start, p2_start)}")
IO.puts("Part 2: #{Day21.play_p2(p1_start, p2_start)}")

