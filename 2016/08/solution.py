instructions = [line.replace('\n', '') for line in open('input.txt', 'r')]

screen = [
    [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '],
    [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ']
]

def handleRect(instructionList):
    col = int(instructionList[1].split('x')[0])
    row = int(instructionList[1].split('x')[1])
    for x in range(row):
        for y in range(col):
            screen[x][y] = 'X'

def handleRow(instructionList):
    row = int(instructionList[2].split('=')[1])
    shift = int(instructionList[4]) % len(screen[row])
    copy = list(screen[row])
    for index in range(len(copy)):
        screen[row][index] = copy[(index - shift) % len(screen[row])]

def handleColumn(instructionList):
    col = int(instructionList[2].split('=')[1])
    shift = int(instructionList[4]) % len(screen)
    copy = [list[col] for list in screen]

    for index in range(len(copy)):
        screen[index][col] = copy[(index - shift) % len(copy)]

rotateHandlers = {
    'column': handleColumn,
    'row': handleRow
}

def handleRotate(instructionList):
    rotateHandlers[instructionList[1]](instructionList)

handlers = {
    'rect': handleRect,
    'rotate': handleRotate
}

for instruction in instructions:
    instructionList = instruction.split(' ')
    handlers[instructionList[0]](instructionList)

numberOfPixelsLit = sum(map(lambda list: list.count('X'), screen))

print('Number of pixels lit:', numberOfPixelsLit)

for row in screen:
    print(row)
