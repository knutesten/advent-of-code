require 'set'

def convert_santa_to_int(password)
  alphabet = 'abcdefghijklmnopqrstuvwxyz'

  password
    .each_char
    .map { |num| alphabet.index(num).to_s(26) }
    .join
    .to_i(26)
end

def convert_int_to_santa(password)
  alphabet = 'abcdefghijklmnopqrstuvwxyz'

  password
    .to_s(26)
    .each_char
    .map { |num| alphabet[num.to_i(26)] }
    .join
    .rjust(8, 'a')
    .slice(-8..-1)
end

def valid_password?(password_int)
  password = password_int.to_s(26)

  has_straight = false
  (password.size - 3).times.each do |index|
    num1 = password[index].to_i(26) + 2
    num2 = password[index + 1].to_i(26) + 1
    num3 = password[index + 2].to_i(26)
    has_straight = true if num1 == num2 && num2 == num3
  end
  return false unless has_straight
  return false unless password.count('8eb').zero?
  return false unless password.scan(/(.)\1/).flat_map { |x| x }.to_set.size > 1

  true
end

puzzle_input = 'hxbxwxba'

new_password_int_part_one = convert_santa_to_int puzzle_input
new_password_int_part_one += 1 until valid_password?(new_password_int_part_one)

puts convert_int_to_santa(new_password_int_part_one)


new_password_int_part_two = new_password_int_part_one + 1
new_password_int_part_two += 1 until valid_password?(new_password_int_part_two)

puts convert_int_to_santa(new_password_int_part_two)
