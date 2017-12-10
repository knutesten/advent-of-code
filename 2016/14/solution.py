from hashlib import md5


def five_in_a_row(key):
    letters = []
    for i in range(len(key) - 4):
        if key[i] == key[i + 1] == key[i + 2] == key[i + 3] == key[i + 4]:
            letters.append(key[i])
    return letters


def three_in_a_row(key):
    for i in range(len(key) - 2):
        if key[i] == key[i + 1] == key[i + 2]:
            return key[i]
    return False


def hash(string, extra_runs):
    key = md5(str.encode(string)).hexdigest()
    for _ in range(extra_runs):
        key = md5(str.encode(key)).hexdigest()
    return key


def find_keys(amount, salt, extra_runs):
    keys = []
    index = 0
    three_of_a_kind_dict = {}
    final_thousand = 1000

    # 14937
    while final_thousand > 0:
        key = hash(salt + str(index), extra_runs)

        for five_in_row_letter in five_in_a_row(key):
            three_in_row_indices = sorted(
                [pair for pair in three_of_a_kind_dict[five_in_row_letter] if pair[0] > index - 1000],
                key=lambda x: x[0])
            three_of_a_kind_dict[five_in_row_letter] = []
            for pair in three_in_row_indices:
                keys.append(pair)

        if amount <= len(keys):
            final_thousand = final_thousand - 1

        letter = three_in_a_row(key)
        if letter:
            if letter in three_of_a_kind_dict:
                three_of_a_kind_dict[letter].append((index, key))
            else:
                three_of_a_kind_dict[letter] = [(index, key)]

        index = index + 1

    keys = sorted(keys, key=lambda x: x[0])[0:amount]

    count = 0
    for pair in keys:
        count = 1 + count
        print(str(pair[0]) + " " + pair[1] + " " + str(count))

    return keys[-1][0]


print("Index that produces the 64th key (part 1): " + str(find_keys(64, "qzyelonm", 0)))
print()
print("Index that produces the 64th key (part 2): " + str(find_keys(64, "qzyelonm", 2016)))
