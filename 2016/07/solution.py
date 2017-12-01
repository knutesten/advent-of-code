import re

ips = [line.replace('\n', '') for line in open('input.txt', 'r')]

# part 1

bracketPattern = re.compile(r'\[\w+\]')
tlsPattern = re.compile(r'(\w)(?!\1)(\w)\2\1')


def ipSupportsTls(ip):
    notInBracket = bracketPattern.split(ip)
    inBracket = bracketPattern.findall(ip)

    if any(tlsPattern.search(word) is not None for word in inBracket):
        return False

    return any(tlsPattern.search(word) is not None for word in notInBracket)


numberOfIpsSupportingTls = len(list(filter(ipSupportsTls, ips)))

print("Number of IPs supporting TLS: " + str(numberOfIpsSupportingTls))

# part 2

test = [
    'abarir[bab]xyz',
    'xyx[xyx]xyx',
    'aaa[kek]eke',
    'zazbz[bzb]cdb'
]

sslPattern = re.compile(r'(?=(\w)(?!\1)(\w)\1)')


def ipSupportsSsl(ip):
    notInBracket = bracketPattern.split(ip)
    inBracket = bracketPattern.findall(ip)

    matchesList = list(filter(lambda l: len(l) > 0, [list(map(lambda m: [m.group(1), m.group(2)], iter)) for iter in
                                                      [sslPattern.finditer(word) for word in notInBracket]]))

    for matches in matchesList:
        for match in matches:
            for word in inBracket:
                if re.search(match[1] + match[0] + match[1], word) is not None:
                    return True



numberOfIpsSupportingSsl = len(list(filter(ipSupportsSsl, ips)))

print("Number of IPs supporting SSL:", numberOfIpsSupportingSsl)
