words = [line.replace('\n', '') for line in open('input.txt', 'r')]

charCount = []

for word in words:
    for charIndex in range(len(word)):
        if charIndex >= len(charCount):
            charCount = charCount + [{}]

        if word[charIndex] not in charCount[charIndex]:
            charCount[charIndex][word[charIndex]] = 1
        else:
            charCount[charIndex][word[charIndex]] += 1


# part 1

decodedWord1 = ''.join(list(map(lambda dict: sorted(dict.keys(), key=lambda key: -dict[key])[0], charCount)))

print(decodedWord1)


# part 2

decodedWord2 = ''.join(list(map(lambda dict: sorted(dict.keys(), key=lambda key: dict[key])[0], charCount)))

print(decodedWord2)
