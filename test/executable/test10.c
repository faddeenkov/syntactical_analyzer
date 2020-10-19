int f(int a, int b){
    return 0;
}

int g (int b){
    return 1;
}

void h(double x, int z){
    f(z, z-1);
}

void main(int z){
    if(g(z) == 1){
    h(1.0, z);
    }
}
