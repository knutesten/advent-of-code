happiness = {}
people = []

File
  .readlines('./input.txt')
  .each do |line|
  name1, lose_gain, amount, name2 =
    line.match(/(\w+) would (\w+) (\d+) happiness .* to (\w+)/).captures

  happiness[[name1, name2]] = amount.to_i * (lose_gain == 'lose' ? -1 : 1)
  people << name1 unless people.include? name1
  people << name2 unless people.include? name2
end

def calculate_max_happiness_change(people, happiness)
  max_change = nil

  people.permutation.each do |arrangement|
    change = 0
    arrangement.each.with_index do |person, index|
      person_left = arrangement[(index - 1) % arrangement.size]
      person_right = arrangement[(index + 1) % arrangement.size]
      change += happiness[[person, person_left]] + happiness[[person, person_right]]
    end

    max_change = change if max_change.nil? || max_change < change
  end

  max_change
end

puts calculate_max_happiness_change(people, happiness)

people.each do |person|
  happiness[['Knut', person]] = 0
  happiness[[person, 'Knut']] = 0
end
people << 'Knut'

puts calculate_max_happiness_change(people, happiness)
