puzzle_input = '3113322113'

def look_and_say(string)
  new_string = ''

  index = 0
  while index < string.size
    prev_number = string[index]
    count = 1
    count += 1 until string[index + count] != prev_number
    index += count
    new_string += count.to_s + prev_number.to_s
  end

  new_string
end

result = puzzle_input
(1..50).each do |i|
  result = look_and_say(result)
  puts result.size if i == 40
  puts result.size if i == 50
end
