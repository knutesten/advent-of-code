import hashlib

# part 1

def calculatePasswordPart1(id):
    password = ''
    index = 0
    while len(password) < 8:
        hash = hashlib.md5(str.encode(id + str(index))).hexdigest()
        if hash.startswith('00000'):
            password = password + hash[5]
        index = index + 1

    return password

print('Password (part 1): ' + calculatePasswordPart1('abbhdwsy'))

# part 2

def calculatePasswordPart2(id):
    password = [None, None, None, None, None, None, None, None]
    index = 0
    while None in password:
        hash = hashlib.md5(str.encode(id + str(index))).hexdigest()
        if hash.startswith('00000') and int(hash[5], 16) < 8 and password[int(hash[5])] is None:
            password[int(hash[5])] = hash[6]
        index = index + 1

    return ''.join(password)

print('Password (part 2): ' + calculatePasswordPart2('abbhdwsy'))
