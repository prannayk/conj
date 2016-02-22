#include <stdio.h>
#include <stdlib.h>
// #include <rts.h>
int main(){
	int a;
	scanf("%d",&a);
	int i = 0;
	int *ptr;
	while (i<10000){
		ptr = malloc(sizeof(int));
		i++;
	}
	printf("%d",a);
	return 0;
}
