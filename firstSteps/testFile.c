#include <stdio.h>
int x = 2;

int factorial_rec(int n){
	if(n <= 1) return 1;
	return n * factorial(n-1);
}

int main(){
	int y = x + 2;
	x = y;
	printf("Hello world!\n");
	factorial_rec(x);
}
