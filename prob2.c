#include <stdio.h>
#include <stdlib.h>
#include "utils/ctools.h"

//cli arg defines upper bound for sum of even fib numbers below upper bound
//for 4000000, should be 4613732
int main(int argc, char *argv[]){
    int upperBound;
    if(argc > 1){
        upperBound = strToLong(argv[1], -1);
        if(upperBound == -1){
            printf("Enter a real number!\n");
            return -1;
        }
    }else{
        printf("Requires 1 cli arg!\n");
        return 1;
    }
    
    int *fibs = genFibb(upperBound);
    int i, sum=0;

    for(i=0; i< *(fibs-1); i++){
        if(fibs[i] % 2 == 0)
            sum+=fibs[i];   
    }
    fibs--;

    printf("Sum of even fibb numbers below %i: %i\n", upperBound, sum);
    free(fibs);
    return 0;
}
