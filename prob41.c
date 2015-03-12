#include <stdio.h>
#include "utils/ctools.h"

int is_pandigital(long n){
    int m = magnitude(n);
    while(m > 0){
        if(num_in(m, n)){
            return 1;
        }
        m--;
    }
    return 0;
}

long largest_pdp(long n){
    long *primes = gen_primes(n);
    long ind = *(primes - 1) - 1;
    while(ind >= 0){
       if(is_pandigital(primes[ind]) == 0){
           return primes[ind];
       }
       ind--;
    }
    return -1;
}

//should be 7652413
main(int argc, char *argv[]){
    if(argc > 1){
        int upperBound = strToLong(argv[1],-1);
        if(upperBound == -1){
            printf("Enter a useful number plz\n");
            return 1;
        }
        int m = largest_pdp(upperBound);
        printf("Largest PDP: %d\n", m);
        return 0;
    }else{
        return 1;
    }
}
