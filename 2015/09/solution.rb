distances = {}
cities = []

File
  .readlines('./input.txt')
  .each do |line|
  from, to, distance = line.match(/(\w+) to (\w+) = (\d+)/).captures
  distances[[from, to].sort!] = distance.to_i

  cities << from unless cities.include? from
  cities << to unless cities.include? to
end

min = NIL
max = NIL
cities.permutation.each do |path|
  distance = 0
  (0..path.size - 2).each do |from_index|
    to_index = from_index + 1
    distance += distances[[path[from_index], path[to_index]].sort!]
  end

  min = distance if min.nil? || distance < min
  max = distance if max.nil? || distance > max
end

puts min
puts max