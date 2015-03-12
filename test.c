#include <stdio.h>
#include <stdlib.h>
#include "utils/ctools.h"

int main(int argc, char *argv[]){
    if(argc > 1){
        int i;
        for(i=1; i<argc; i++)
            printf("%li\n", strToLong(argv[i], -1));
    }
    return 0;
}
