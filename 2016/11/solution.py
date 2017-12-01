import re

floors = [re.findall('(\w+ generator|\w+-compatible)', floor) for floor in open('input.txt', 'r')]
elevatorPosition = 0

def isDone:
    for index in range(len(floors) - 1):
        if len(floors[index]) > 0:
            return False
    return True


moveCount = 0

def findNewMove():
    if isDone():
        return



print(floors)
