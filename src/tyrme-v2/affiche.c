#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]){
	FILE *fichier = NULL;
	int i = 0;
	fichier = fopen(argv[1], "r");
	if(fichier != NULL){
		do{
			i = fgetc(fichier);
			printf("%d\n", i);
		} while(i != EOF);
		fclose(fichier);
	}
	return 0;
}