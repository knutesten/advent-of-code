words = File
        .readlines('./input.txt')
        .map(&:strip)

characters_in_code = words.reduce(0) { |sum, word| sum + word.size }
characters_in_memory = words.reduce(0) do |sum, word|
  sum + word.size - word.scan(/\\["\\]/).length - 3 * word.scan(/\\x[0-9a-f]{2}/).length - 2
end

puts characters_in_code - characters_in_memory

encoded_characters = words.reduce(0) do |sum, word|
  sum + 4 + 2 * word.scan(/\\["\\]/).length + word.scan(/\\x[0-9a-f]{2}/).length + word.size
end

puts encoded_characters - characters_in_code
