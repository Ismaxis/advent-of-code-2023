import re

test_input = [
    "467..114..",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......755.",
    "...$.*....",
    ".664.598..",
]


def is_symbol(ch: str):
    return not (ch.isdigit() or ch == "." or ch.isspace())


def dots(n):
    return "".join("." for _ in range(n))


def solve(input: list[str]):
    n = len(input)
    m = len(input[0])

    def get(i, j):
        if 0 <= i < n and 0 <= j < m:
            return input[i][j]
        return "."

    for i in range(n):
        was_digit = False
        digit_start = 0
        was_symbol = False
        for j in range(m):
            has_symbol = any(
                map(is_symbol, [get(i - offset, j) for offset in range(-1, 2)])
            )
            was_symbol |= has_symbol
            cur = input[i][j]
            if not cur.isdigit():
                if was_digit and not was_symbol:
                    input[i] = (
                        f"{input[i][:digit_start]}{dots(j - digit_start)}{input[i][j:]}"
                    )
                was_digit = False
                was_symbol = has_symbol
                continue

            if not was_digit:
                was_digit = True
                if was_symbol:
                    while get(i, j + 1).isdigit():
                        j += 1
                else:
                    digit_start = j

    return input


if __name__ == "__main__":
    with open("day3.txt") as fin:
        ans = sum(
            sum(int(i) for i in re.split(r"[^0-9]", row) if len(i) > 0)
            for row in solve(fin.readlines())
        )
        print(ans)
