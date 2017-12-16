from functools import reduce
from copy import deepcopy

input = [floors for floors in open("./input.txt", "r")]

def find_traps(floors, nr_of_floors):
    floors = deepcopy(floors)
    is_trap = ['^^.', '.^^', '^..', '..^']
    for i in range(nr_of_floors - 1):
        new_line = ''
        prev_line = '.' + floors[i] + '.'
        floor_len = len(floors[i])
        for j in range(floor_len):
            new_line = new_line + ('^' if prev_line[j:j+3] in is_trap else '.')
        floors.append(new_line)
    return floors

def find_free_spaces(floors):
    free_spaces = 0
    for floor in floors:
        free_spaces = free_spaces + reduce((lambda sum, char: sum + (0 if char is '^' else 1)), floor, 0)
    return free_spaces

floors = find_traps(input, 40)
print(find_free_spaces(floors))


floors = find_traps(input, 400000)
print(find_free_spaces(floors))


