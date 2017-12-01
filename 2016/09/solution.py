word = open('input.txt', 'r').readline()


# part 1

def decompress(word):
    decompressed = ''
    index = 0
    while index < len(word):
        char = word[index]
        if char is not '(':
            decompressed += char
            index += 1
        else:
            endOfMarker = word.find(')', index)
            marker = word[index + 1:endOfMarker]
            numberOfChars = int(marker.split('x')[0])
            numberOfTimes = int(marker.split('x')[1])
            substring = word[endOfMarker + 1:endOfMarker + 1 + numberOfChars]
            for _ in range(numberOfTimes):
                decompressed += substring
            index = endOfMarker + 1 + len(substring)

    return decompressed


print('Length of decompressed word (part 1):', len(decompress(word)))


# part 2

def decompressedSizePart2(word, size):
    if not word:
        return size
    startOfMarker = word.find('(') + 1

    if startOfMarker is not 0:
        endOfMarker = word.find(')')
        marker = word[startOfMarker:endOfMarker]
        numberOfChars = int(marker.split('x')[0])
        numberOfTimes = int(marker.split('x')[1])
        newSize = decompressedSizePart2(word[endOfMarker + 1: endOfMarker + 1 + numberOfChars],
                                        0) * numberOfTimes + startOfMarker - 1 + size
        return decompressedSizePart2(word[endOfMarker + 1 + numberOfChars:], newSize)

    return size + len(word)


print('Length of decompressed word (part 2):', decompressedSizePart2(word, 0))
