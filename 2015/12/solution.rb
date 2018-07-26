require 'json'

puzzle_input = File.read('./input.txt')

part_one_sum = puzzle_input.scan(/-?\d+/).reduce(0) { |sum, x| sum + x.to_i }
puts part_one_sum


json = JSON.parse(puzzle_input)
nodes_to_process = [json]
part_two_sum = 0
until nodes_to_process.empty?
  node = nodes_to_process.pop
  case node
  when Array
    nodes_to_process.concat node
  when Hash
    nodes_to_process.concat node.values unless node.values.include? 'red'
  when Integer
    part_two_sum += node
  end
end

puts part_two_sum
