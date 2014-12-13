#include <stdio.h>
#include "utils/ctools.h"

int is_pandigital(long n){
    int m = magnitude(n);
    while(m > 0){
        if(num_in(m, n) == 1){
            return 1;
        }
        m--;
    }
    return 0;
}

long largest_pdp(long n){
    long *primes = gen_primes(n+1);
    long ind = *(primes - 1) - 1;
    printf("largest: %d\n", primes[ind]);
    while(ind >= 0){
       if(is_pandigital(primes[ind]) == 0){
           return primes[ind];
       }
       ind--;
    }
    return -1;
}

main(int argc, char *argv[]){
    if(argc > 1){
        int tst1 = atoi(argv[1]);
        int m = largest_pdp(tst1);
        printf("Is PD?: %d\n", m);
        return 0;
    }else{
        return 1;
    }
}
