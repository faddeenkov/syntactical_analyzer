int f(int a){
    if(a==0) return 0;
    return 1;
}

void main(int num1, int num2){
    f(num1);

    int b;

    if(f(num2)){
        b = num1/num2;
    }

    return f(num2)? b : 0;
}