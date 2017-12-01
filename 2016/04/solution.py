lines = [line.replace('\n', '') for line in open('input.txt', 'r')]

# part 1

def convertStringToRoom(line):
    parts = line.split('-')
    name = '-'.join(parts[:-1])
    id = int(parts[-1].split('[')[0])
    checksum = parts[-1].split('[')[1][:-1]
    return name, id, checksum

rooms = list(map(convertStringToRoom, lines))

def checksumIsValid(room):
    charMap = {}
    name = room[0].replace('-', '')
    for char in name:
        if char not in charMap:
            charMap[char] = 1
        else:
            charMap[char] = charMap[char] + 1

    sortedKeys = sorted(charMap.keys(), key=lambda x: (-charMap[x], x))

    checksum = room[2]
    for index in range(len(checksum)):
        if checksum[index] != sortedKeys[index]:
            return False

    return True

sumOfIds = sum(map(lambda room: room[1], filter(checksumIsValid, rooms)))

print(sumOfIds)


# part 2

letters = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']

validRooms = list(filter(checksumIsValid, rooms))

def decodeName(room):
    name = room[0]
    id = room[1]
    def rotateLetter(letter):
        if letter == '-':
            return ' '
        return letters[(id + letters.index(letter)) % len(letters)]

    return ''.join(map(rotateLetter, name)), id, room[2]


print(list(filter(lambda room: 'pole' in room[0], map(decodeName, validRooms))))
