import math

def gen_pent(n):
    return n * (3 * n - 1) / 2
    
def is_sqrt(n):
    x = n
    y = (x + n // x) // 2
    while y < x:
        x = y
        y = (x + n // x) // 2
    if x * x == n:
        return x
    return -1

def is_pentagonal(n):
    #this tests if the b^2 - 4ac portion of quad formula is an int
    sqrt = is_sqrt(24*n+1)
    
    tmp = sqrt + 1
    if sqrt > 0 and tmp % 6 == 0:
        return tmp / 6
    else:
        return -1

def test_diff(d):
    k = d
    nk, nd = (gen_pent(k), gen_pent(d))
    while nd >= gen_pent(k+1) - nk:
        if is_pentagonal(nd + nk) > 0:
            if is_pentagonal(nd + nk + nk) > 0:
                return True
        k = k+1
        nk = gen_pent(k)
    return False
    #while nd >= gen_pent(k+1) - nk:
    #    if is_pentagonal(nd + nk) > 0:
    #        if is_pentagonal(nd + nk + nk) > 0:
    #            return True
    #    k = k+1
    #    nk = gen_pent(k)
    #return False

def test_diff2(d):
    return is_pentagonal(3*d+1)

def find_min_diff():
    j = 1
    while True:
        t = test_diff2(j)
        if t > 0:
            print j, j*3+1, is_pentagonal(j*3+1)
            add = is_pentagonal(gen_pent(j) + gen_pent(j+1))
            if add > 0:
                break
        j = j+1
    return j*3+1

if __name__=="__main__":
    print find_min_diff()
