require 'set'

movements = File.read('./input.txt').split('')

def get_locations(movements)
  directions = { '>': [1, 0], 'v': [0, -1], '<': [-1, 0], '^': [0, 1] }

  movements.reduce([[0, 0]]) do |aggregated_locations, direction|
    current_coordinate = aggregated_locations.last
    aggregated_locations << [
      current_coordinate[0] + directions[direction.to_sym][0],
      current_coordinate[1] + directions[direction.to_sym][1]
    ]
  end
end

puts get_locations(movements).to_set.size

santa_movements, robo_santa_movements = movements.partition.with_index { |_, index| index.even? }
combined_locations = get_locations(santa_movements) + get_locations(robo_santa_movements)
puts combined_locations.to_set.size
