from re import findall

discs = [
    tuple(map(
        int,
        findall('(\d+)\spositions.*position\s(\d+)', line.strip())[0]
    ))
         for line
         in open('./input.txt', 'r')
]

test_discs = [
    (5, 4),
    (2, 1)
]

def capsule_goes_through(discs):
    time = 0
    while True:
        went_through = True
        for i in range(len(discs)):
            if 0 != (discs[i][1] + time + i + 1) % discs[i][0]:
                went_through = False
                break

        if went_through:
            return time

        time = time + 1


print("Time to press button (part 1): "  + str(capsule_goes_through(discs)))

print()

discs.append((11, 0))
print("Time to press button (part 2): "  + str(capsule_goes_through(discs)))
