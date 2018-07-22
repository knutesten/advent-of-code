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


instruction_handler = {
    'cpy': handle_cpy,
    'inc': handle_inc,
    'dec': handle_dec,
    'jnz': handle_jnz,
}


def is_clock(instructions, registers):
    position = 0
    prev = 1
    count = 0
    while 0 <= position < len(instructions):
        next_instruction = instructions[position][:3]
        next_value = instructions[position][4:]
        if next_instruction == 'out':
            output = get_value_or_register(next_value, registers)
            if output not in (0, 1) or prev == output:
                return False

            prev = output
            count = count + 1
            if count == 10000:
                return True

            position = position + 1
        else:
            position = position + instruction_handler[next_instruction](next_value, registers)

    return False


a = 0
while not is_clock(puzzle_input.copy(), {'a': a, 'b': 0, 'c': 0, 'd': 0}):
    a = a + 1

print(a)
