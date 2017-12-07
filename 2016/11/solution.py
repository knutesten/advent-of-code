import copy
import hashlib
import itertools
import re
import time

floorsText = [re.findall('(\w+ generator|\w+-compatible microchip)', floor) for floor in open('input.txt', 'r')]
input = [[re.sub('^(..).*\s(.).*$', '\g<1>\g<2>', element).upper() for element in floor] for floor in floorsText]

start = copy.deepcopy(input)
start[0].insert(0, 'E')

elements = [item for floor in input for item in floor]
elements.sort()

goal = [[], [], [], ['E'] + elements]


def calculate_valid_elevator_states(elems):
    return [comb for comb in itertools.combinations(elems, 2)
            if comb[0][2] == comb[1][2] or (comb[0][2] != comb[1][2] and comb[0][:2] == comb[1][:2])]


valid_elevator_states = calculate_valid_elevator_states(elements)


def heuristic(floors):
    floorsWithoutElevator = list(map(lambda lst: list(filter(lambda x: x != "E", lst)), floors))
    nrOfFloors = len(floorsWithoutElevator)
    heur = 0
    for i in range(nrOfFloors):
        factor = (nrOfFloors - 1 - i)
        heur += max(factor, factor * 2 * len(floorsWithoutElevator[i]) - factor)
    return heur


def calckey(node):
    return hashlib.md5(str.encode(str(node))).hexdigest()


def reconstruct_path(cameFrom, current):
    total_path = [current]
    while calckey(current) in cameFrom:
        current = cameFrom[calckey(current)]
        total_path.insert(0, current)

    return total_path


def count_empty_floors(node):
    empty_floors = 0
    for floor in node:
        if floor:
            return empty_floors
        empty_floors = empty_floors + 1


def state_is_valid(old_node, new_node):
    if count_empty_floors(new_node) < count_empty_floors(old_node):
        return False

    node_copy = copy.deepcopy(new_node)
    for floor in node_copy:
        if "E" in floor:
            floor.remove("E")
        if [elm for elm in floor if elm.endswith('G')]:
            for elm in [elm for elm in floor if elm.endswith('M')]:
                if (elm[:2] + 'G') not in floor:
                    return False
    return True


def calculate_closed_key(node):
    return calckey([[elem[2] if len(elem) == 3 else elem for elem in floor] for floor in node])


def calculate_neighbors(node, valid_elev_states):
    elevator_position = next(i for i in range(len(node)) if "E" in node[i])
    posibilities = []
    directions = []

    if elevator_position > 0:
        directions.append(-1)
    if elevator_position < (len(node) - 1):
        directions.append(1)

    for direction in directions:
        for i in range(1, len(node[elevator_position])):
            node_copy = copy.deepcopy(node)
            tmp = node_copy[elevator_position].pop(i)
            node_copy[elevator_position + direction].insert(0, tmp)
            node_copy[elevator_position].pop(0)
            node_copy[elevator_position + direction].sort()
            node_copy[elevator_position + direction].insert(0, "E")
            if state_is_valid(node, node_copy):
                posibilities.append(node_copy)

        for valid_elv_comb in valid_elev_states:
            if valid_elv_comb[0] in node[elevator_position] and valid_elv_comb[1] in node[elevator_position]:
                node_copy = copy.deepcopy(node)
                node_copy[elevator_position].remove(valid_elv_comb[0])
                node_copy[elevator_position].remove(valid_elv_comb[1])
                node_copy[elevator_position].remove("E")
                node_copy[elevator_position + direction].append(valid_elv_comb[0])
                node_copy[elevator_position + direction].append(valid_elv_comb[1])
                node_copy[elevator_position + direction].sort()
                node_copy[elevator_position + direction].insert(0, "E")
                if state_is_valid(node, node_copy):
                    posibilities.append(node_copy)

    unique_posibilities = {calculate_closed_key(pos): pos for pos in posibilities}.values()
    return unique_posibilities


def astar(start, goal, valid_elev_states):
    closed_set = set()
    open_set_map = {calckey(start): start}
    open_set = set([calckey(start)])
    came_from = {}
    g_score = {calckey(start): 0}
    f_score = {calckey(start): heuristic(start)}
    goal_key = calckey(goal)

    while len(open_set) != 0:
        current_key = sorted(open_set, key=lambda x: f_score[x])[0]
        current = open_set_map[current_key]

        if len(closed_set) % 10000 == 0 and len(closed_set) > 0:
            print("Closed nodes: " + str(len(closed_set)) + " Open nodes: " + str(len(open_set)))

        if current_key == goal_key:
            return reconstruct_path(came_from, current)

        open_set.remove(current_key)

        del open_set_map[current_key]
        closed_set.add(calculate_closed_key(current))

        for neighbor in calculate_neighbors(current, valid_elev_states):
            neighbor_key = calckey(neighbor)

            if calculate_closed_key(neighbor) in closed_set:
                continue

            if neighbor_key not in open_set:
                open_set.add(neighbor_key)
                open_set_map[neighbor_key] = neighbor

            tentative_g_score = g_score[current_key] + 1

            if neighbor_key in g_score and tentative_g_score >= g_score[neighbor_key]:
                continue

            came_from[neighbor_key] = current
            g_score[neighbor_key] = tentative_g_score
            f_score[neighbor_key] = tentative_g_score + heuristic(neighbor)

    print("Failed")


print("Starting calculation part 1 ...")
start_time = time.time()
result = astar(start, goal, valid_elevator_states)
print("Calculation complete.")
print("Execution took: " + str(time.time() - start_time) + " seconds.")
print()
print("Steps:")
for step in result:
    print(step)
print()
print("Number of steps (part 1): " + str(len(result) - 1))
print()
print()

new_input = copy.deepcopy(input)
new_input[0].append('ELG')
new_input[0].append('ELM')
new_input[0].append('DIG')
new_input[0].append('DIM')
new_input[0].sort()

start = copy.deepcopy(new_input)
start[0].insert(0, 'E')

elements = [item for floor in new_input for item in floor]
elements.sort()

goal = [[], [], [], ['E'] + elements]

valid_elevator_states = calculate_valid_elevator_states(elements)

print("Starting calculation part 2 ...")
start_time = time.time()
result = astar(start, goal, valid_elevator_states)
print("Calculation complete.")
print("Execution took: " + str(time.time() - start_time) + " seconds.")
print()
print("Steps:")
for step in result:
    print(step)
print()
print("Number of steps (part 2): " + str(len(result) - 1))
