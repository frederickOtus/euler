import time, sys

def explode_number(n):
    return [ int(c) for c in str(n) ]

def implode_number(ar):
    n = 0
    ar.reverse()
    for i in range(len(ar)):
        n += (10 ** i) * ar[i]
    return n

def digit_array(n):
    nums = [0] * 10
    for c in str(n):
        nums[int(c)] += 1
    return nums

def isprime(n):
    """Returns True if n is prime."""
    if n == 2:
        return True
    if n == 3:
        return True
    if n % 2 == 0:
        return False
    if n % 3 == 0:
        return False

    """
    Think about this:

        For numbers greater than three:
            if i % 6 in (0,2,6), then it is divisible by 2 and not prime
            if i % 6 == 3, then it is divisible by 3 and not prime

        This only numbers x6 +- 1 can be prime. Thus a simple way to speed up a bit is to increment by 2, then 4
            when testing primes. That's what's happening here.

    """
    i = 5
    w = 2

    while i * i <= n:
        if n % i == 0:
            return False

        i += w
        w = 6 - w

    return True


def sieve_of_eratosthenes(n):
    nums = [1] * (n + 1)
    primes = [] 
    n = 2
    while n < len(nums):
        if nums[n] == 1:
            primes.append(n)

            itr = n * 2
            while itr < len(nums):
                nums[itr] = 0
                itr += n
        n+=1
    return primes, nums

def prime_speed_test(n):
    t1s = time.time()
    sieve_of_eratosthenes(n)
    t1e = time.time()

    t2s = time.time()
    for i in range(2, n):
        easy_isprime(i)
    t2e = time.time()

    print("Sieve time: %f" % (t1e -t1s))
    print("easy time: %f" % (t2e -t2s))

if __name__ == "__main__":
    if len(sys.argv) > 1:
        prime_speed_test(int(sys.argv[1]))
