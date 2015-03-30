#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "ctools.h"

int text_iterator(void (*func)(char *, int *), char *text){
    int end = strlen(text);
    int total = 0;
    for(int i = 0; i < end; i ++){
        func(text, &total);
        text++;
    }

    return total;
}

void sort_names(char **names, int len){
   if(len < 2)
      return;

    int i, pivot_ind = 0;
    char *tmpname;

    for(i = 1; i < len; i++){
        if(strcmp(names[i], names[pivot_ind]) < 0){
            //if pivot is greater than target, swap element with element after pivot, then swap next with pivot
            if(i - pivot_ind > 1){
                tmpname = names[pivot_ind + 1];
                names[pivot_ind + 1] = names[pivot_ind]; 
                names[pivot_ind] = names[i];
                names[i] = tmpname;
            }else{
                tmpname = names[i];
                names[i] = names[pivot_ind]; 
                names[pivot_ind] = tmpname;
            }

            pivot_ind++; 
        }
    }

    sort_names(names, pivot_ind);
    sort_names((names + pivot_ind + 1), len - pivot_ind - 1);
}

void letter_score(char *letter, int *val){
    *val += *letter - 64;
}

int name_score(char *word){
    return text_iterator(&letter_score, word);
}

void comma_count(char *chr, int *total){

    if(*chr == ','){
        *total+=1;
    }
}

char ** load_file(char *fname, int *numnames){
    FILE *fp;
    long size;
    char *contents;
    char **names;
    int nameind = 0;
    long i = 0;

    if((fp = fopen(fname, "r")) == NULL) {
        fputs("Error opening file!\n", stderr);
    }

    //load file into memory
    fseek(fp, 0, SEEK_END);
    size = ftell(fp);
    rewind(fp);

    contents = malloc(size + 1);
    fread(contents, 1, size, fp);
    fclose(fp);

    //convert file string to names
    *numnames = text_iterator(&comma_count, contents) + 1;
    names = malloc(sizeof(char *) * *numnames);
    
    while(nameind < *numnames){
        if(contents[i] > 64 && contents[i] < 91){
            int strlen = ind_of_next(&contents[i], '"');
            if(strlen < 0){
                fputs("Error parsing names!\n", stderr);
                exit(1);
            }
            names[nameind] = malloc(strlen + 1);
            str_partial_cpy(names[nameind], &contents[i], strlen);
            i+=strlen;
            nameind++;
        }else{
            i++;
        }
    }

    free(contents);

    return names;
}

int main(int args, char *argv[]){
    int len;
    char **names = load_file(argv[1], &len);
    long total = 0;
    sort_names(names, len);

    for(int i = 0; i < len; i++){
        total += (i + 1) * name_score(names[i]);
    }
    printf("%li\n", total);
    return 0;
}
