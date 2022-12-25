import requests
import math

def fetch_input():
    session_file = open("../session")
    session = session_file.readline().strip()
    session_file.close()
    res = requests.get("https://adventofcode.com/2022/day/25/input", cookies = {"session": session})
    return res.text

def get_input():
    try:
        read = open("input")
        input = read.read()
        read.close()
        return input
    except:
        input = fetch_input()
        write = open("input", "w")
        write.write(input)
        write.close()
        return input

snafu_char_map = ['=','-','0','1','2']
snafu_map_char = {'=': -2, '-': -1, '0': 0, '1': 1, '2': 2}

def to_snafu(num):
    length = (0 if num == 0 else int(math.ceil(math.log(abs(num), 5)))) + 1
    list = [0 for i in range(length)]
    list[0] = num
    for i in range(length):
        if list[i] < -2 or list[i] >= 2:
            n = (list[i] + 2) // 5
            list[i + 1] += n
            list[i] -= n * 5

    # trim zeros
    for i in reversed(range(1,length)):
        if(list[i] == 0):
            list.pop()
        else:
            break

    return ''.join(map(lambda d: snafu_char_map[d + 2], reversed(list)))

def from_snafu(str):
    digits = list(map(lambda c: snafu_map_char[c], str))
    pos = 1
    res = 0

    for d in reversed(digits):
        res += d * pos
        pos *= 5

    return res

def solve_part1(input):
    return to_snafu(sum(map(from_snafu, input.split("\n"))))

input = get_input()
print(f"Part1: {solve_part1(input)}")
