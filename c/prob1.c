#include <stdio.h>
#include <stdlib.h>
#include "utils/ctools.h"

//cli arg defines upper bound for multiples of 3 and/or 5
//for 1000, should be 233168
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
    
    int i, sum=0;
    for(i=0;i<upperBound; i++){
        if(i % 5 == 0 || i % 3 == 0){
            sum += i;
        }
    }
    
    printf("Sum of multiples of 3 or 5 below %i: %i\n", upperBound, sum);
    return 0;
}
