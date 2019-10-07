require 'prime'

def calculate_presents_part_1(house)
  10 * Prime
         .prime_division(house)
         .map { |n, p| (n ** (p + 1) - 1) / (n - 1) }
         .inject(:*)
end

def calculate_presents_part_2(house)
  factors = Prime
              .prime_division(house)
              .flat_map { |n, pow| Array.new(pow) { n } }
              .<<(1)

  if factors.size <= 2
    all_divisible = factors
  else
    all_divisible = (1..factors.size - 1)
                      .map { |c| factors.combination(c) }
                      .flat_map { |comb| comb.map { |l| l.inject(:*) } }
                      .uniq!
                      .filter { |f| f * 50 >= house }
  end

  all_divisible.sum * 11
end

goal = 29000000

(2..).each do |house|
  if calculate_presents_part_1(house) >= goal
    puts "Part 1: " + house.to_s
    break
  end
end

(1..).each do |house|
  if calculate_presents_part_2(house) >= goal
    puts "Part 2: " + house.to_s
    break
  end
end
