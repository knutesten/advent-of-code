path = open('input.txt', 'r').read().replace('\n', '').split(', ')

directions = [
    (0, 1),  # north
    (1, 0),  # east
    (0, -1),  # south
    (-1, 0)  # west
]


# Part 1

def calculatePosition(directionIndex, position, path):
    if not path:
        return position

    head, *tail = path

    newDirectionIndex = (directionIndex + (1 if head[0] == 'R' else -1)) % len(directions)

    numberOfSteps = int(head[1:])

    newPosition = (position[0] + directions[newDirectionIndex][0] * numberOfSteps,
                   position[1] + directions[newDirectionIndex][1] * numberOfSteps)

    return calculatePosition(newDirectionIndex, newPosition, tail)


finalPosition = calculatePosition(0, (0, 0), path)

print("Final position: " + str(finalPosition))
print("Distance to final position: " + str(abs(finalPosition[0]) + abs(finalPosition[1])))


# Part 2

def calculateFirstPositionVisitedTwice(directionIndex, position, previousPositions, path):
    if not path:
        return "No position was visited twice"

    head, *tail = path

    newDirectionIndex = (directionIndex + (1 if head[0] == 'R' else -1)) % len(directions)

    numberOfSteps = int(head[1:])

    newPositions = list(map(lambda steps: (position[0] + directions[newDirectionIndex][0] * steps,
                                           position[1] + directions[newDirectionIndex][1] * steps),
                            range(1, numberOfSteps + 1)))

    for position in newPositions:
        if position in previousPositions:
            return position

    return calculateFirstPositionVisitedTwice(newDirectionIndex, newPositions[-1], previousPositions + newPositions,
                                              tail)

firstPositionVisitedTwice = calculateFirstPositionVisitedTwice(0, (0, 0), [], path)

print("First position visited twice: " + str(firstPositionVisitedTwice))
print("Distance to the first position visited twice: " + str(
    abs(firstPositionVisitedTwice[0]) + abs(firstPositionVisitedTwice[1])))
