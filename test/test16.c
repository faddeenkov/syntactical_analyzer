void f(int x){
    return x /2;
}

void main(){
    int i = 1;

    for(int i = 0; i < 10; i++){
        f(i);
        for(int i = 0; i < 15; i++){
            f(i);
        }
    }
    f(i);
}