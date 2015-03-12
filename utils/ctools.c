#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ctools.h"

long strToLong(char *string, long defaultVal){
    char **end = &string;
    long val = strtol(string, end, 10);

    if((int)strlen(string) > 0 && **end == '\0'){
        return val;
    }
    return defaultVal;
}

//Bit functions
unsigned long zero_nth_bit(unsigned long num, int bit){
    unsigned long max = -1;
    unsigned long mask = 1;
    mask = mask << bit;
    return (~mask) & num;
}

int nth_bit(unsigned long num, int bit){
    return num>>bit & 1;
}

//End Bit Funtions

int num_in(int n, long num){
    if(num < 0){
        num = num * -1;
    }
    for(;;){
        if(num % 10 == n){
            return 0;
        }
        if(num % 10 == num){
            return 1;
        }
        num = num / 10;
    }   
}

int is_palindrome(long n){
    int mag = magnitude(n);
    if(mag == 0){
        return 0;
    }

    int i = 0;
    int j = mag - 1;

    while(i < j){
        if(nth_dig(n,i) != nth_dig(n,j)){
            return 1;
        }
        i++;
        j--;
    }
    return 0;
}

int nth_dig(long num,int n){
    while(n > 0){
        if(num % 10 == num){
            return -1;
        }
        n--;
        num = num/10;
    } 
    return num % 10;
}

int magnitude(long n){
    if(n < 0){
        return -1;
    }

    int m = 0;
    while(n > 0){
        m++;
        n = n / 10;
    }
    return m;
}

//generates list of primes up to, but not including, max val
// and returns a pointer to that list
long *gen_primes(long max){
    long found = 0; //tracks number of primes found
    unsigned long *test_field = malloc(max);
//unsigned long *test_field = malloc(max/sizeof(long) + 1); //list of possible nums to be prime

    unsigned long i,j; //looping vars
    i = 0;
    j = -1;
    for(i;i<max/sizeof(long);i++){
        test_field[i] = -1;
    }

    test_field[0] = zero_nth_bit(test_field[0],0); //one isn't prime, so set it to zero, same with zero
    test_field[0] = zero_nth_bit(test_field[0],1); 

    i = j = 0;
    unsigned long bindex, bnum;
    for(i;i<max;i++){
        bindex = i / (8 * sizeof(long) );
        bnum = i % (8 * sizeof(long));
        if(1 == nth_bit(test_field[bindex], bnum)){
            found++;
            for(j=2*i; j<max; j+=i){
                bindex = j / (8 * sizeof(long) );
                bnum = j % (8 * sizeof(long));
                test_field[bindex] = zero_nth_bit(test_field[bindex], bnum);
            }
        }
    }

    long *primes = malloc((found+1) * sizeof *primes);
    primes[0] = found;
    i = 0;
    j = 1;
    for(i; i<max; i++){
        bindex = i / (8 * sizeof(long) );
        bnum = i % (8 * sizeof(long));
        if(1 == nth_bit(test_field[bindex],bnum)){
            primes[j] = i;
            j++;
        }
    }
    primes++;
    return primes;
}
