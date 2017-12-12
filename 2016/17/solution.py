from hashlib import md5
import curses
from time import sleep


screen = curses.initscr()


def draw_world(start_row, position):
    world = [
        "#########",
        "# | | | #",
        "#-#-#-#-#",
        "# | | | #",
        "#-#-#-#-#",
        "# | | | #",
        "#-#-#-#-#",
        "# | | |  ",
        "####### V"
    ]
    (x, y) = position
    row = y * 2 + 1
    col = x * 2 + 1
    player_line = list(world[row])
    player_line[col] = "S"
    world[row] = "".join(player_line)

    for i in range(len(world)):
        screen.addstr(start_row + i, 0, world[i])

    screen.addstr(start_row + len(world), 0, "\n")
    screen.refresh()


def door_is_open(direction, hash):
    open = ['b', 'c', 'd', 'e', 'f']
    index_map = {'U': 0, 'D': 1, 'L': 2, 'R': 3}
    return hash[index_map[direction]] in open


def find_neighbors(path, prefix):
    directions = {
        (0, 1): 'D',
        (1, 0): 'R',
        (0, -1): 'U',
        (-1, 0): 'L'
    }

    neighbors = []

    hash = md5(str.encode(prefix + path)).hexdigest()
    coor = calc_coor(path)

    for direction in directions.keys():
        new_coor = (coor[0] + direction[0], coor[1] + direction[1])
        if door_is_open(directions[direction], hash) and 0 <= new_coor[0] < 4 and 0 <= new_coor[1] < 4:
            neighbors.append(path + directions[direction])

    return neighbors


def heuristic_cost_estimate(path, goal):
    coor = calc_coor(path)
    return abs(goal[0] - coor[0]) + abs(goal[1] - coor[1])

def reconstruct_path(came_from, current):
    total_path = [current]
    while current in came_from:
        current = came_from[current]
        total_path.append(current)
    return total_path


def calc_coor(path):
    directions = {
        'D': (0, 1),
        'R': (1, 0),
        'U': (0, -1),
        'L': (-1, 0)
    }

    coor = (0, 0)
    for direction in path:
        coor = (coor[0] + directions[direction][0], coor[1] + directions[direction][1])

    return coor


def a_star(start, goal, prefix):
    closed_set = set()
    open_set = set([start])
    g_score = {start: 0}
    f_score = {start: heuristic_cost_estimate(start, goal)}

    while len(open_set) > 0:
        current = sorted(open_set, key=lambda x: f_score[x])[0]

        open_set.remove(current)
        closed_set.add(current)

        if calc_coor(current) == goal:
            return current

        for neighbor in find_neighbors(current, prefix):
            if neighbor in closed_set:
                continue

            if neighbor not in open_set:
                open_set.add(neighbor)

            tentative_score = g_score[current] + 1
            if neighbor in g_score and tentative_score >= g_score[neighbor]:
                continue

            g_score[neighbor] = tentative_score
            f_score[neighbor] = tentative_score + heuristic_cost_estimate(neighbor, goal)


prefix = "yjjvjgan"
path = a_star("", (3, 3), prefix)

screen.addstr(0, 0, "Shortest path: " + path)

for i in range(len(path) + 1):
    draw_world(2, calc_coor(path[:i]))
    sleep(0.2)

curses.endwin()

print('Shortest path: ' + path)
print()


def find_longest_path(start, goal, prefix):
    closed_set = set()
    open_set = set([start])
    longest_path = 0

    while len(open_set) > 0:
        current = list(open_set)[0]

        open_set.remove(current)
        closed_set.add(current)

        if calc_coor(current) == goal:
            if len(current) > longest_path:
                longest_path = len(current)
            continue

        for neighbor in find_neighbors(current, prefix):
            if neighbor in closed_set:
                continue

            if neighbor not in open_set:
                open_set.add(neighbor)

    return longest_path


print('Longest path: ' + str(find_longest_path("", (3,3), prefix)))