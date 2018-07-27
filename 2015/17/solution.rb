containers = File.readlines('./input.txt').map(&:to_i)
amount_to_store = 150

combination_max_length =
  containers
  .sort
  .reduce([]) { |acc, size| acc << (acc[-1].nil? ? size : acc[-1] + size) }
  .index { |acc| acc > amount_to_store }

combinations =
  (1..combination_max_length)
  .map { |length| containers.combination(length) }
  .flat_map(&:to_a)
  .select { |comb| comb.reduce(:+) == amount_to_store }

puts combinations.size

min_number_of_containers = combinations.min_by(&:size).size
puts combinations
  .select { |comb| comb.size == min_number_of_containers }
  .size
