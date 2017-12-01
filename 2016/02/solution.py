instructions = list(line.replace('\n', '') for line in open('input.txt', 'r'))

directions = {
    'U': (-1, 0),
    'R': (0, 1),
    'D': (1, 0),
    'L': (0, -1)
}

# Part 1

keypadPart1 = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
]


def calculateKeypadNumber(position, path):
    if not path:
        return position

    head, *tail = path

    x = position[0] + directions[head][0]
    y = position[1] + directions[head][1]

    newPosition = (x if 0 <= x <= 2 else position[0], y if 0 <= y <= 2 else position[1])

    return calculateKeypadNumber(newPosition, tail)


def calculateCode(code, startingPosition, instructions):
    if not instructions:
        return code

    head, *tail = instructions

    position = calculateKeypadNumber(startingPosition, head)
    newCode = code + str(keypadPart1[position[0]][position[1]])

    return calculateCode(newCode, position, tail)


codePart2 = calculateCode("", (1, 1), instructions)

print("Code (part 1): " + codePart2)

# part 2

keypadPart2 = [
    [None, None, 1, None, None],
    [None, 2, 3, 4, None],
    [5, 6, 7, 8, 9],
    [None, 'A', 'B', 'C', None],
    [None, None, 'D', None, None]
]


def calculateKeypadNumberPart2(position, path):
    if not path:
        return position

    head, *tail = path

    x = position[0] + directions[head][0]
    y = position[1] + directions[head][1]

    tempPos = (x if 0 <= x <= 4 else position[0], y if 0 <= y <= 4 else position[1])

    newPosition = tempPos if keypadPart2[tempPos[0]][tempPos[1]] is not None else position

    return calculateKeypadNumberPart2(newPosition, tail)


def calculateCodePart2(code, startingPosition, instructions):
    if not instructions:
        return code

    head, *tail = instructions

    position = calculateKeypadNumberPart2(startingPosition, head)
    newCode = code + str(keypadPart2[position[0]][position[1]])

    return calculateCodePart2(newCode, position, tail)


codePart2 = calculateCodePart2("", (2, 0), instructions)

print("Code (part 2): " + codePart2)
