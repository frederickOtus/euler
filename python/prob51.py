from utils import sieve_of_eratosthenes as prime_sieve
from utils import digit_array, explode_number, implode_number

def inds_of_n(numarray, n):
    res = []
    for i in range(len(numarray)-1):
        if numarray[i] == n:
            res.append(i)
    return res

def replace_inds(numarray, inds, n):
    newlst = []
    for i in range(len(numarray)):
        if i in inds:
            newlst.append(n)
        else:
            newlst.append(numarray[i])
    return implode_number(newlst)

def solve_for_n(n, sieve_size=10000000):
    primes, sieve = prime_sieve(sieve_size)

    for prime in primes:
       
        dig_array = digit_array(prime / 10)
        exn = explode_number(prime)
        
        for i in range(10):
            if dig_array[i] == 0:
                continue

            misses = 0
            inds = inds_of_n(exn, i)
            if inds == []:
                continue

            for testnum in range(i + 1, 10): #not correct bound
                if misses + i > n:
                    break
                
                rplc = replace_inds(exn, inds, testnum)
                if sieve[rplc] == 0:
                    misses += 1
            if misses + i <= 10 - n:
                return prime
    return None

if __name__ == "__main__":
    print(solve_for_n(8))
