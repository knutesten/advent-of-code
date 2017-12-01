instructions = [line.replace('\n', '') for line in open('input.txt', 'r')]

bots = {}
outputs = {}

for valueInstruction in filter(lambda x: x.startswith('value'), instructions):
    botNr = valueInstruction.split(' ')[5]
    value = int(valueInstruction.split(' ')[1])
    if botNr not in bots:
        bots[botNr] = []

    bots[botNr].append(value)

while len(list(filter(lambda x: len(x) > 1, bots.values()))) > 0:
    botNr = next(filter(lambda x: len(bots[x]) > 1, bots.keys()))
    botInstruction = next(ins for ins in instructions if ins.split(' ')[1] == botNr and ins.startswith('bot'))
    lowId = botInstruction.split(' ')[6]
    highId = botInstruction.split(' ')[11]
    low = min(bots[botNr])
    high = max(bots[botNr])

    if low == 17 and high == 61:
        print('Number of bot (part 1):', botNr)

    bots[botNr] = []

    if botInstruction.split(' ')[5] == 'bot':
        if lowId not in bots:
            bots[lowId] = []
        bots[lowId].append(low)
    else:
        if lowId not in outputs:
            outputs[lowId] = []
        outputs[lowId].append(low)

    if botInstruction.split(' ')[10] == 'bot':
        if highId not in bots:
            bots[highId] = []
        bots[highId].append(high)
    else:
        if highId not in outputs:
            outputs[highId] = []
        outputs[highId].append(high)

valueForPart2 = outputs['0'][0] * outputs['1'][0] * outputs['2'][0]

print("Product of values (part 2):", valueForPart2)