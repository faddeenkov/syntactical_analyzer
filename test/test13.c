int ggt (int a, int b){
    if(a==b) return a;
    else{
        if (a > b){
            return ggt (a-b, b);
        }
        else {
            return ggt(b-a, a);
        }
    }
}

void main(int a, int c){
    int b = c/2;
    int tmp = ggt(a, c);

    if(tmp == ggt(a,b)){
    b = a;
    }
}