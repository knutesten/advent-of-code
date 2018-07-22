from itertools import permutations, combinations

world = [list(line.strip()) for line in open('./input.txt', 'r')]


def is_wall(coor):
    return world[coor[1]][coor[0]] == '#'


def find_neighbors(coor):
    directions = [
        (0, 1),
        (1, 0),
        (0, -1),
        (-1, 0)
    ]

    neighbors = []

    for direction in directions:
        new_coor = (coor[0] + direction[0], coor[1] + direction[1])
        if not is_wall(new_coor):
            neighbors.append(new_coor)

    return neighbors


def heuristic_cost_estimate(coor, goal):
    return goal[0] - coor[0] + goal[1] - coor[1]


def reconstruct_path(came_from, current):
    total_path = [current]
    while current in came_from:
        current = came_from[current]
        total_path.append(current)
    return total_path


def a_star(start, goal):
    closed_set = set()
    open_set = {start}
    came_from = {}
    g_score = {start: 0}
    f_score = {start: heuristic_cost_estimate(start, goal)}

    while len(open_set) > 0:
        current = sorted(open_set, key=lambda x: f_score[x])[0]

        open_set.remove(current)
        closed_set.add(current)

        if current == goal:
            return reconstruct_path(came_from, current)

        for neighbor in find_neighbors(current):
            if neighbor in closed_set:
                continue

            if neighbor not in open_set:
                open_set.add(neighbor)

            tentative_score = g_score[current] + 1
            if neighbor in g_score and tentative_score >= g_score[neighbor]:
                continue

            came_from[neighbor] = current
            g_score[neighbor] = tentative_score
            f_score[neighbor] = tentative_score + heuristic_cost_estimate(neighbor, goal)


start = None
goals = []
for y in range(len(world)):
    for x in range(len(world[y])):
        if world[y][x] not in ['#', '.']:
            if world[y][x] == '0':
                start = (x, y)
            else:
                goals.append((x, y))

goals_permutations = list(permutations(goals))

costs = {}

for goal in goals:
    costs[(start, goal)] = len(a_star(start, goal)) - 1

for a, b in combinations(goals, 2):
    costs[(a, b)] = len(a_star(a, b)) - 1

min_cost = None
min_path = None
for permutation in goals_permutations:
    cost = costs[(start, permutation[0])]
    for ind, a in enumerate(permutation):
        if ind < (len(permutation) - 1):
            b = permutation[ind + 1]
            try:
                cost = cost + costs[(a, b)]
            except KeyError:
                cost = cost + costs[(b, a)]

    if not min_cost or cost < min_cost:
        min_cost = cost
        min_path = [start] + list(permutation)

print(min_cost)

whole_path = []
for ind, coor in enumerate(min_path):
    if ind < (len(min_path) - 1):
        whole_path = whole_path + a_star(coor, min_path[ind + 1])


def draw_world(path):
    for y, row in enumerate(world):
        print(''.join(['o' if (x, y) in path else cell for x, cell in enumerate(row)]))


# draw_world(whole_path)


min_cost = None
min_path = None
for permutation in goals_permutations:
    cost = costs[(start, permutation[0])]
    for ind, a in enumerate(permutation):
        if ind < (len(permutation) - 1):
            b = permutation[ind + 1]
            try:
                cost = cost + costs[(a, b)]
            except KeyError:
                cost = cost + costs[(b, a)]
        else:
            cost = cost + costs[start, a]
    if not min_cost or cost < min_cost:
        min_cost = cost

print(min_cost)
