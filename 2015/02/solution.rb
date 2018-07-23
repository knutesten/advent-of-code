gifts = File
        .open('./input.txt')
        .map { |str| str.split('x').map(&:to_i) }

wrapping_paper = gifts.reduce(0) do |sum, gift|
  side_a = gift[0] * gift[1] * 2
  side_b = gift[0] * gift[2] * 2
  side_c = gift[1] * gift[2] * 2

  sum + side_a + side_b + side_c + [side_a, side_b, side_c].min / 2
end

puts wrapping_paper

ribbon_to_order = gifts.reduce(0) do |sum, gift|
  a, b = gift.sort

  sum + 2 * a + 2 * b + gift.reduce(&:*)
end

puts ribbon_to_order