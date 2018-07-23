require 'set'

words = File.readlines('./input.txt')

def nice_word_part_1?(word)
  word_list = word.split('')

  vowels = 'aeiou'.split('')
  nr_of_vowels = word_list.select { |c| vowels.include? c }.size
  return false if nr_of_vowels < 3


  letter_appears_twice = false
  word_list.each.with_index do |char, index|
    if index < word_list.size - 1 && char == word_list[index + 1]
      letter_appears_twice = true
    end
  end
  return false unless letter_appears_twice

  disallowed_strings = %w[ab cd pq xy]
  has_disallowed_string = disallowed_strings.reduce(false) { |acc, str| acc || word.include?(str) }
  return false if has_disallowed_string

  true
end

nice_words_part_one = words.select { |w| nice_word_part_1?(w) }

puts nice_words_part_one.size

def nice_word_part_2?(word)
  return false if /([a-z][a-z]).*\1/.match(word).nil?

  return false if /([a-z])[a-z]\1/.match(word).nil?

  true
end

nice_words_part_two = words.select { |w| nice_word_part_2?(w) }

puts nice_words_part_two.size