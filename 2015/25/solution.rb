input = File.read("./input.txt").match(/(\d+), column (\d+)/)
row = input[1].to_i
column = input[2].to_i

def calc_next_code(code)
  code * 252533 % 33554393
end

def walk_diagonals(row, column)
  value = 20151125
  r = 1
  while true
    tmp_r = r
    c = 1
    while c <= tmp_r
      return value if r == row && c == column
      value = calc_next_code(value)
      r = r - 1
      c = c + 1
    end
    r = tmp_r + 1
  end
end

puts "Part 1: #{walk_diagonals(row, column)}"
