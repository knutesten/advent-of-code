instructions = File
                 .readlines('./input.txt')
                 .map do |line|
  line_split = line.split(' ')
  if line_split.size == 3
    ['START', line_split[0], line_split[2]]
  elsif line_split.size == 5
    [line_split[1], line_split[0], line_split[2], line_split[4]]
  elsif line_split.size == 4
    [line_split[0], line_split[1], line_split[3]]
  else
    raise 'Unrecognized instruction: ' + line
  end
end

def calculate_wires(instructions)
  wires = {}
  instructions = instructions.clone

  until instructions.empty?
    instruction = instructions.pop
    case instruction[0]
    when 'START'
      a = Integer(instruction[1]) rescue wires[instruction[1]]

      if a.nil?
        instructions.unshift(instruction)
      else
        wires[instruction[2]] = a
      end
    when 'AND', 'OR', 'RSHIFT', 'LSHIFT'
      a = Integer(instruction[1]) rescue wires[instruction[1]]
      b = Integer(instruction[2]) rescue wires[instruction[2]]

      if a.nil? || b.nil?
        instructions.unshift(instruction)
      else
        wires[instruction[3]] =
          case instruction[0]
          when 'AND';    a &  b
          when 'OR';     a |  b
          when 'RSHIFT'; a >> b
          when 'LSHIFT'; a << b
          end
      end
    when 'NOT'
      a = Integer(instruction[1]) rescue wires[instruction[1]]

      if a.nil?
        instructions.unshift(instruction)
      else
        wires[instruction[2]] = ~a
      end
    end
  end

  wires
end

wires_part_one = calculate_wires(instructions)
puts wires_part_one['a']

instructions_part_two = instructions.clone
instructions_part_two[3][1] = wires_part_one['a']
print(calculate_wires(instructions_part_two)['a'])
