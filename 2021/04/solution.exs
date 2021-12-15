{:ok, file_str} = File.read("input.txt")

defmodule Day4 do
  def transpose(coll) do
    coll
    |> List.zip
    |> Enum.map(&Tuple.to_list/1)
  end

  def is_winner?(board, drawn_balls) do
    Enum.any?(board, fn r -> MapSet.subset?(r, drawn_balls) end)
  end

  def calc_score(board, balls, pos) do
    drawn_balls = MapSet.new(Enum.take(balls, pos))
    sum_unmarked = board
    |> Enum.reduce(&MapSet.union/2)
    |> MapSet.difference(drawn_balls)
    |> Enum.sum

    sum_unmarked * Enum.at(balls, pos - 1)
  end

  def first_winning_board(boards, balls) do
    first_winning_board(boards, balls, 5)
  end

  def first_winning_board(boards, balls, pos) do
    drawn_balls = MapSet.new(Enum.take(balls, pos))
    winning_board = Enum.find(boards, fn b -> is_winner?(b, drawn_balls) end)

    if winning_board == nil do
      first_winning_board(boards, balls, pos + 1)
    else
      calc_score(winning_board, balls, pos)
    end
  end

  def last_winning_board(boards, balls) do
    last_winning_board(boards, balls, Enum.count(balls))
  end

  def last_winning_board(boards, balls, pos) do
    drawn_balls = MapSet.new(Enum.take(balls, pos))
    loosing_board = Enum.find(boards, fn b -> !is_winner?(b, drawn_balls) end)

    if loosing_board == nil do
      last_winning_board(boards, balls, pos - 1)
    else
      calc_score(loosing_board, balls, pos + 1)
    end
  end
end

lines = file_str |> String.split("\n", trim: true)

[balls_str | boards_str] = lines

balls = balls_str
|> String.split(",", trim: true)
|> Enum.map(&String.to_integer/1)

boards = boards_str
|> Enum.map(&String.split(&1, " ", trim: true))
|> Enum.map(&Enum.map(&1, fn s -> String.to_integer(s) end))
|> Enum.chunk_every(5)
|> Enum.map(fn b -> b ++ Day4.transpose(b) end)
|> Enum.map(&Enum.map(&1, fn b -> MapSet.new(b) end))

IO.puts("Part 1: #{Day4.first_winning_board(boards, balls)}")
IO.puts("Part 2: #{Day4.last_winning_board(boards, balls)}")

