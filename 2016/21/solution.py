def swap(string, a, b):
    try:
        a, b = int(a), int(b)
    except ValueError:
        a, b = string.index(a), string.index(b)

    string_list = list(string)
    string_list[a], string_list[b] = string_list[b], string_list[a]

    return ''.join(string_list)


def rotate_right(string, a):
    try:
        steps = int(a) % len(string)
    except ValueError:
        index = string.index(a)
        steps = 1 + index + (0 if index < 4 else 1)

    rotate_point = len(string) - steps
    return string[rotate_point:] + string[0:rotate_point]


def rotate_left(string, a):
    steps = int(a) % len(string)
    return string[steps:] + string[0:steps]


def rotate_for_letter_reverse(string, a):
    goal = string
    prev_string = string
    while rotate_right(prev_string, a) != goal:
        prev_string = rotate_left(prev_string, 1)

    return prev_string


def reverse(string, a, b):
    a, b = int(a), int(b)
    return string[0:a] + string[a:b + 1][::-1] + string[b + 1:len(string)]


def move(string, a, b):
    a, b = int(a), int(b)
    string_list = list(string)
    tmp = string_list.pop(a)
    string_list.insert(b, tmp)

    return ''.join(string_list)


def scramble(string, instructions):
    scrambled_string = string
    for instruction in instructions:
        if instruction[0] == 'swap':
            scrambled_string = swap(scrambled_string, instruction[2], instruction[5])
        elif instruction[0] == 'rotate':
            if instruction[1] == 'right':
                scrambled_string = rotate_right(scrambled_string, instruction[2])
            elif instruction[1] == 'left':
                scrambled_string = rotate_left(scrambled_string, instruction[2])
            else:
                scrambled_string = rotate_right(scrambled_string, instruction[6])
        elif instruction[0] == 'reverse':
            scrambled_string = reverse(scrambled_string, instruction[2], instruction[4])
        elif instruction[0] == 'move':
            scrambled_string = move(scrambled_string, instruction[2], instruction[5])

    return scrambled_string


def unscramble(string, instructions):
    scrambled_string = string
    for instruction in reversed(instructions):
        if instruction[0] == 'swap':
            scrambled_string = swap(scrambled_string, instruction[2], instruction[5])
        elif instruction[0] == 'rotate':
            if instruction[1] == 'right':
                scrambled_string = rotate_left(scrambled_string, instruction[2])
            elif instruction[1] == 'left':
                scrambled_string = rotate_right(scrambled_string, instruction[2])
            else:
                scrambled_string = rotate_for_letter_reverse(scrambled_string, instruction[6])
        elif instruction[0] == 'reverse':
            scrambled_string = reverse(scrambled_string, instruction[2], instruction[4])
        elif instruction[0] == 'move':
            scrambled_string = move(scrambled_string, instruction[5], instruction[2])

    return scrambled_string


puzzle_input = [x.strip().split(' ') for x in open('./input.txt', 'r')]
print(scramble('abcdefgh', puzzle_input))
print()
print(unscramble('fbgdceah', puzzle_input))
