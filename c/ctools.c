#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ctools.h"

//string funcs
void str_partial_cpy(char *dest, char *src, int numchars){
    int i = 0;
    for(i; i < numchars; i++){
        if(src[i] == '\0')
            return;
        dest[i] = src[i];
    }
    dest[i] = '\0';
}

int ind_of_next(char *text, char targ){
    int i = 0;
    while(true){
        if(text[i] == targ)
            return i;
        else if(text[i] == '\0')
            return -1;
        i++;
    }
}

//helper function for parsing numbers out of cli-args. Takes a string and
//determines if the entire thing is a number, if so, returns it, else the specified default val
long strToLong(char *string, long defaultVal){
    char **end = &string;
    long val = strtol(string, end, 10);

    if((int)strlen(string) > 0 && **end == '\0'){
        return val;
    }
    return defaultVal;
}

int *genFibb(int upperBound){
    int maxVals = 10;
    int *fibs = malloc(sizeof(int) * (maxVals + 1));

    fibs++;

    fibs[0] = 1;
    fibs[1] = 1;

    int n = 1;

    while(fibs[n] < upperBound){
        n++;
        fibs[n] = fibs[n - 1] + fibs[n - 2];
    }

    *(fibs - 1) = n;
    return fibs;

}

//set the nth bit in a long to 0
unsigned long zero_nth_bit(unsigned long num, int bit){
    unsigned long max = -1;
    unsigned long mask = 1;
    mask = mask << bit;
    return (~mask) & num;
}

//returns the value of the nth bit
int nth_bit(unsigned long num, int bit){
    return num>>bit & 1;
}

//takes a digit and number and returns true if digit is in number
int num_in(int n, long num){
    if(n > 9)
        return 1;

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

//grab the nth digit of a number
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

//should probably be called numberOfdigits, but I wrote this a long time ago so cut me some slack
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
long *gen_primes(long upperBound){
    long found = 0; //tracks number of primes found
    upperBound += upperBound % sizeof(long);
    long max = upperBound / 8; //number of bits in a byte
    unsigned long *test_field = malloc(max); //list of possible nums to be prime

    unsigned long i,j; //looping vars
    i = 0;
    j = -1;
    for(i;i< upperBound / 8;i++){
        test_field[i] = 1;
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
