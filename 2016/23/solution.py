test_input = [
    "cpy 2 a",
    "tgl a",
    "tgl a",
    "tgl a",
    "cpy 1 a",
    "dec a",
    "dec a"
]

puzzle_input = [line.strip() for line in open('./input.txt', 'r')]


def get_value_or_register(value, registers):
    try:
        return int(value)
    except ValueError:
        return registers[value]


def handle_cpy(value, registers):
    [first, key] = value.split(' ')
    registers[key] = get_value_or_register(first, registers)
    return 1


def handle_inc(key, registers):
    registers[key] = registers[key] + 1
    return 1


def handle_dec(key, registers):
    registers[key] = registers[key] - 1
    return 1


def handle_jnz(value, registers):
    [key, offset] = value.split(' ')
    return 1 if get_value_or_register(key, registers) == 0 else get_value_or_register(offset, registers)


def handle_mul(key, registers):
    registers[key] = registers[key] * 2
    return 1


def handle_tgl(value, registers, position, instructions):
    offset = get_value_or_register(value, registers)

    try:
        offset_instruction = instructions[position + offset]
        instruction_type = offset_instruction[:3]
        arguments = offset_instruction[4:].split(' ')
        if instruction_type == 'inc':
            instructions[position + offset] = 'dec ' + arguments[0]
        elif len(arguments) == 1:
            instructions[position + offset] = 'inc ' + arguments[0]
        elif instruction_type == 'jnz':
            instructions[position + offset] = 'cpy ' + ' '.join(arguments)
        else:
            instructions[position + offset] = 'jnz ' + ' '.join(arguments)

    except IndexError:
        return 1

    return 1


instruction_handler = {
    'cpy': handle_cpy,
    'inc': handle_inc,
    'dec': handle_dec,
    'jnz': handle_jnz,
}


def calculate(instructions, registers):
    position = 0
    while 0 <= position < len(instructions):
        next_instruction = instructions[position][:3]
        next_value = instructions[position][4:]
        if next_instruction == 'tgl':
            handle_tgl(next_value, registers, position, instructions)
            position = position + 1
        else:
            position = position + instruction_handler[next_instruction](next_value, registers)
    return registers


print("Part 1: " + str(calculate(puzzle_input.copy(), {'a': 7, 'b': 0, 'c': 0, 'd': 0})['a']))

print("Part 2: " + str(calculate(puzzle_input.copy(), {'a': 12, 'b': 0, 'c': 0, 'd': 0})['a']))
