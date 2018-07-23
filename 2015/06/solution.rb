lights_part_one = (1..1000).map { Array.new(1000, 0) }
lights_part_two = (1..1000).map { Array.new(1000, 0) }

File
  .readlines('./input.txt')
  .each do |instruction|

  match = /^(turn off|turn on|toggle|) (\d+),(\d+).* (\d+),(\d+)/.match(instruction)
  action = match[1]
  start_coordinate = [match[2].to_i, match[3].to_i]
  end_coordinate = [match[4].to_i, match[5].to_i]

  (start_coordinate[0]..end_coordinate[0]).each do |x|
    (start_coordinate[1]..end_coordinate[1]).each do |y|
      case action
      when 'turn off'
        lights_part_one[y][x] = 0
        lights_part_two[y][x] = [0, lights_part_two[y][x] - 1].max
      when 'turn on'
        lights_part_one[y][x] = 1
        lights_part_two[y][x] += 1
      else
        lights_part_one[y][x] = lights_part_one[y][x] == 1 ? 0 : 1
        lights_part_two[y][x] += 2
      end
    end
  end
end

puts lights_part_one.reduce(0) { |sum, row| sum + row.reduce(0) { |sum_row, light| sum_row + light } }
puts lights_part_two.reduce(0) { |sum, row| sum + row.reduce(0) { |sum_row, light| sum_row + light } }
