import re

lines = [line.strip() for line in open('./input.txt', 'r')]
lines = lines[2:]
lines = [re.split(" +", line) for line in lines]
lines = [[line[0].split("/")[3].split("-")[1:], int(line[2][:-1]), int(line[3][:-1]), ind] for ind, line in
         enumerate(lines)]
for line in lines:
    line[0] = [int(x[1:]) for x in line[0]]

puzzle_input = lines


def movable_data_nodes(nodes):
    pairs = []
    movable_coors = set()

    for node_a in nodes:
        for node_b in nodes:
            if node_a[1] != 0 and node_a[0] != node_b[0] and node_a[1] <= node_b[2]:
                pairs.append((node_a, node_b))
                movable_coors.add((node_a[0][0], node_a[0][1]))
                movable_coors.add((node_b[0][0], node_b[0][1]))

    return pairs, movable_coors


pairs, movable_coors = movable_data_nodes(puzzle_input)
print(len(pairs))
print()


def heuristic_cost_estimate(coor, goal):
    curr_goal_data = coor[1]
    goal_goal_data = goal[1]
    curr_empty = coor[0]
    goal_empty = (curr_goal_data[0] + 1, curr_goal_data[1])

    return \
        2 * abs(goal_goal_data[0] - curr_goal_data[1] + goal_goal_data[1] - curr_goal_data[1]) + \
        abs(goal_empty[0] - curr_empty[0] + goal_empty[1] - curr_empty[1])


def reconstruct_path(came_from, current):
    total_path = [current]
    while current in came_from:
        current = came_from[current]
        total_path.append(current)
    return list(reversed(total_path))


def find_neighbors(coordinates, movable_coors):
    directions = [
        (0, 1),
        (1, 0),
        (0, -1),
        (-1, 0)
    ]

    neighbors = []

    empty_coor = coordinates[0]
    goal_data_coor = coordinates[1]

    for direction in directions:
        new_empty_coor = (empty_coor[0] + direction[0], empty_coor[1] + direction[1])
        if new_empty_coor in movable_coors:
            if new_empty_coor == goal_data_coor:
                new_goal_data_coor = empty_coor
            else:
                new_goal_data_coor = goal_data_coor

            neighbors.append((new_empty_coor, new_goal_data_coor))

    return neighbors


def a_star(start, goal, movable_coors):
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

        for neighbor in find_neighbors(current, movable_coors):
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


empty_coor = ()
for node in puzzle_input:
    if node[1] == 0:
        empty_coor = (node[0][0], node[0][1])
        break
goal_data_coor = (36, 0)
goal_position_coor = (0, 0)

start = (empty_coor, goal_data_coor)
goal = ((1, 0), goal_position_coor)

path = a_star(start, goal, movable_coors)
print(len(path) - 1)
