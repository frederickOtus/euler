def name_sum(name):
    sum = 0
    for n in name:
        sum += ord(n) - 64
    return sum

def solve(fname):
    f = open(fname)
    text = f.readlines()[0]

    names = [ n[1:-1] for n in text.split(',') ]
    names.sort()

    tot = 0
    for ind, name in enumerate(names):
        tot += (ind + 1) * name_sum(name)

    return tot

if __name__ == "__main__":
    print(solve("p022_names.txt")) 
