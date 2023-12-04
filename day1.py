digit_map = {
    "one": "1",
    "two": "2",
    "three": "3",
    "four": "4",
    "five": "5",
    "six": "6",
    "seven": "7",
    "eight": "8",
    "nine": "9",
}
    

def parse(word):

    def lookahead(substring, acc=""):
        for c in substring:
            acc += c
            if (number := digit_map.get(acc)):
                return number
        return False

    results = []

    for i, c in enumerate(word):
        
        if c.isdigit():
            results.append(c)
            continue
        idx = min(i + 5, len(word))
        
        if (number := lookahead(word[i:idx])):
            results.append(number)

    return results
        

def collect(values):
    int("".join([values[0], values[-1]]))

    
def main():
    examples = [
        "two1nine",
        "eightwothree",
        "abcone2threexyz",
        "xtwone3four",
        "4nineeightseven2",
        "zoneight234",
        "7pqrstsixteen"
    ]

    def part2(lines):
        return sum(map(lambda x: int("".join([x[0], x[-1]])),
                   map(parse, lines)))
    with open("day1.txt") as fo:
        return part2(fo.readlines())
    
    
        
