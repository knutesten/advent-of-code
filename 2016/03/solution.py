import re

# part 1

trianglesPart1 = list(
    list(
        map(int,
            re.split('\s+', line.replace('\n', '').strip()))) for line in open('input.txt', 'r'))


def validateTriangle(triangle):
    largest, *rest = sorted(triangle, reverse=True)
    return largest < sum(rest)


numberOfValidTrianglesPart1 = len(list(filter(validateTriangle, trianglesPart1)))

print("Valid triangles (part 1): " + str(numberOfValidTrianglesPart1))


# part 2

def convertTrianglesToPart2(oldFormat, newFormat):
    if not oldFormat:
        return newFormat

    first, second, third, *rest = oldFormat

    return convertTrianglesToPart2(rest, newFormat + [list(tri) for tri in zip(first, second, third)])


trianglesPart2 = convertTrianglesToPart2(trianglesPart1, [])

numberOfValidTrianglesPart2 = len(list(filter(validateTriangle, trianglesPart2)))

print("Valid triangles (part 2): " + str(numberOfValidTrianglesPart2))
