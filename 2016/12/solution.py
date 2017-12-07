test_input = [
    "cpy 41 a",
    "inc a",
    "inc a",
    "dec a",
    "jnz a 2",
    "dec a"
]

input = [line.strip() for line in open('./input.txt', 'r')]


def handleCpy(value, registers):
    [first, key] = value.split(' ')
    try:
        registers[key] = int(first)
    except ValueError:
        registers[key] = registers[first]
    return 1


def handleInc(key, registers):
    registers[key] = registers[key] + 1
    return 1


def handleDec(key, registers):
    registers[key] = registers[key] - 1
    return 1


def handleJnz(value, registers):
    [key, offset] = value.split(' ')
    try:
        return 1 if int(key) == 0 else int(offset)
    except ValueError:
        return 1 if registers[key] == 0 else int(offset)


instruction_handler = {
    'cpy': handleCpy,
    'inc': handleInc,
    'dec': handleDec,
    'jnz': handleJnz
}


def calculate(instructions, registers):
    position = 0
    while 0 <= position < len(instructions):
        next_instrution = instructions[position][:3]
        next_value = instructions[position][4:]
        position = position + instruction_handler[next_instrution](next_value, registers)
    return registers


print("Part 1: " + str(calculate(input, {'a': 0, 'b': 0, 'c': 0, 'd': 0})['a']))
print("Part 2: " + str(calculate(input, {'a': 0, 'b': 0, 'c': 1, 'd': 0})['a']))
