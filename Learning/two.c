#include <stdio.h>

int main(){
	int a = 0;
	while (1){
		if(a < 10000000)
			a++;
		else
			a=0;
	}
	return 0;
}
