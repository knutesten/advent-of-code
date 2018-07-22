instructions = File.read('./input.txt').split('')

floor = instructions.reduce(0) { |level, char| level + (char == '(' ? 1 : -1) }

puts floor


level = 0
instructions.each.with_index(1) do |char, position|
  level += (char == '(' ? 1 : -1)
  if level < 0
    puts position
    break
  end
end