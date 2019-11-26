input = File.readlines('input.txt')
                 .map { |l| l.rstrip }
                 .map { |l| [l[0..2]].concat l[4..].split(', ') }

def run_program(instructions, a, b)

  registers = {
    :a => a,
    :b => b
  }

  position = 0
  inst = instructions[position]
  until inst.nil?
    case inst[0]
    when "hlf"
      registers[inst[1].to_sym] /= 2
    when "tpl"
      registers[inst[1].to_sym] *= 3
    when "inc"
      registers[inst[1].to_sym] += 1
    when "jmp"
      position += inst[1].to_i - 1
    when "jie"
      position += inst[2].to_i - 1 if registers[inst[1].to_sym].even?
    when "jio"
      position += inst[2].to_i - 1 if registers[inst[1].to_sym] == 1
    end
    position += 1
    inst = instructions[position]
  end

  registers
end

p run_program(input, 0, 0)
p run_program(input, 1, 0)
