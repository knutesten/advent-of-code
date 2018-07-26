ingredients = File.readlines('./input.txt').reduce([]) do |list, line|
  list << line
          .match(/^.* (-?\d+).* (-?\d+).* (-?\d+).* (-?\d+).* (-?\d+)/)
          .captures
          .map(&:to_i)
end

combinations =
  (1..100 - ingredients.size + 1)
  .to_a
  .repeated_permutation(ingredients.size)
  .select { |comb| comb.reduce(:+) == 100 }

max_score = nil
max_score_500_cals = nil
combinations.each do |combination|
  score =
    [combination, ingredients]
    .transpose
    .map { |amount, ingredient| ingredient[0..3].map { |prop| prop * amount } }
    .transpose
    .map { |props| props.reduce(:+) }
    .map { |prop| [0, prop].max }
    .reduce(:*)

  calories =
    [combination, ingredients]
    .transpose
    .map { |amount, ingredient| ingredient[4] * amount }
    .reduce(:+)

  max_score = score if max_score.nil? || score > max_score
  max_score_500_cals = score if calories == 500 && (max_score_500_cals.nil? || score > max_score_500_cals)
end

puts max_score
puts max_score_500_cals
