from functools import reduce

# input = 10
input = 1352

def is_wall(coor):
    (x, y) = coor
    res = x*x + 3*x + 2*x*y + y + y*y + input
    nr_of_ones = reduce(
        lambda sum, x: 1 + sum if x == '1' else sum,
        str(bin(res)),
        0)
    return nr_of_ones % 2 != 0


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
        if not is_wall(new_coor) and new_coor[0] >= 0 and new_coor[1] >= 0:
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
    open_set = set([start])
    came_from = {}
    g_score = {start: 0}
    f_score = {start: heuristic_cost_estimate(start, goal)}

    while len(open_set) > 0:
        current = sorted(open_set, key=lambda x: f_score[x])[0]

        open_set.remove(current)
        closed_set.add(current)

        if current == goal:
            result = (reconstruct_path(came_from, current), closed_set)
            return result

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


def draw_map(size, closed_set, path):
    for y in range(size):
        print("".join(['#' if is_wall((x, y)) else '.' if (x,y) in path else 'o' if (x,y) in closed_set else ' ' for x in range(size)]))

result_part_1 = a_star((1,1), (31,39))

print("Number of steps from (1,1) to (31, 39): " + str(len(result_part_1[0])))
print()
draw_map(60, result_part_1[1], result_part_1[0])

def bredth_first_search(start, max_steps):
    closed_set = set()
    open_set = set([start])
    came_from = {}
    g_score = {start: 0}

    while len(open_set) > 0:
        current = open_set.pop()
        closed_set.add(current)

        if len(reconstruct_path(came_from, current)) == max_steps + 1:
            continue

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

    return closed_set

print()
print()

result_part_2 = bredth_first_search((1,1), 50)
print("Number of locations: " + str(len(result_part_2)))
print()
draw_map(35, result_part_2, [])

closed_set = bredth_first_search((1,1), 50)

