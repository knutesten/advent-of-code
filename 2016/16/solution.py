def calculate_dragon_curve(initial_data, disk_length):
    dragon_curve = initial_data

    while len(dragon_curve) < disk_length:
        dragon_curve = dragon_curve + "0" + "".join(reversed(["1" if x == "0" else "0" for x in dragon_curve]))

    return dragon_curve[:disk_length]


def calculate_checksum(data):
    checksum = data
    while len(checksum) % 2 == 0:
        new_checksum = ""
        for i in range(0, len(checksum), 2):
            new_checksum = new_checksum + ("1" if checksum[i] == checksum[i + 1] else "0")
        checksum = new_checksum

    return checksum


disk_length = 272
initial_data = "01000100010010111"

# test_disk_length = 20
# test_initial_data = "10000"
# print(calculate_checksum(calculate_dragon_curve(test_initial_data, test_disk_length)))
print("Checksum (part 1): " + str(calculate_checksum(calculate_dragon_curve(initial_data, disk_length))))
print()
disk_length_part2 = 35651584
print("Checksum (part 2): " + str(calculate_checksum(calculate_dragon_curve(initial_data, disk_length_part2))))
