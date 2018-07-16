puzzle_input = [list(map(lambda i: int(i), x.split('-'))) for x in open('./input.txt', 'r')]


def find_min_ip(blocked_ips):
    sorted_blocked_ips = sorted(blocked_ips, key=lambda x: x[0], reverse=True)
    min_ip = 0
    while len(sorted_blocked_ips) > 0:
        ip_range = sorted_blocked_ips.pop()

        if ip_range[0] <= min_ip:
            min_ip = max(ip_range[1] + 1, min_ip)
        elif not sorted_blocked_ips:
            return max(ip_range[1] + 1, min_ip)
        else:
            return min_ip


print(find_min_ip(puzzle_input.copy()))
print()


def allowed_ips(blocked_ips):
    sorted_blocked_ips = sorted(blocked_ips, key=lambda x: x[0], reverse=True)

    count = 0
    min_ip = 0
    max_ip = 4294967295
    while len(sorted_blocked_ips) > 0:
        ip_range = sorted_blocked_ips.pop()

        if ip_range[0] > min_ip:
            count = count + ip_range[0] - min_ip

        min_ip = max(ip_range[1] + 1, min_ip)

    return count + max_ip - min_ip + 1


print(allowed_ips(puzzle_input.copy()))
