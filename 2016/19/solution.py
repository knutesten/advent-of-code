from math import floor

nr_of_elves = 3001330


def part_1(nr_of_elves):
    elves = [x for x in range(1, nr_of_elves + 1)]
    while len(elves) > 1:
        len_elves = len(elves)
        elves = [x for ind, x in enumerate(elves) if ind % 2 == 0]
        if len_elves % 2 != 0:
            elves = [elves[-1]] + elves[:-1]
    return elves[0]


print(part_1(nr_of_elves))
print()


def part_2_slow(nr_of_elves):
    elves = [x for x in range(1, 1 + nr_of_elves)]
    removed_elves = []
    current_pos = 0

    print(elves)
    print(removed_elves)
    print()

    while len(elves) > 1:
        l = len(elves)
        current_elf = elves[current_pos]
        next_elf = elves[(current_pos + 1) % l]
        next_next_elf = elves[(current_pos + 2) % l]
        steal_pos = int((current_pos + int(floor(l / 2))) % l)
        removed_elves.append(elves[steal_pos])
        del elves[steal_pos]

        print([str(x) if x == current_elf else x for x in elves])
        print(sorted(removed_elves))
        print()

        try:
            current_pos = elves.index(next_elf)
        except ValueError:
            current_pos = elves.index(next_next_elf)

    return elves[0]


# print(part_2_slow(5))
# print()


def part_2_fast(nr_of_elves):
    elves = [x for x in range(1, 1 + nr_of_elves)]

    while len(elves) > 1:
        l = len(elves)
        half = int(floor(l / 2))

        odd = l % 2 == 1
        excluded_indices = {(i + int(floor((i + (1 if odd else 0)) / 2)) + half) % l for i in range(half)}
        elves = [x for ind, x in enumerate(elves) if ind not in excluded_indices]

        shift = half - len([x for x in excluded_indices if x < half])
        elves = elves[shift:] + elves[0:shift]

    return elves[0]


print(part_2_fast(nr_of_elves))
