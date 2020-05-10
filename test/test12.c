int f (int x){
    return x;
}

int g (int a, int b){
    if (f(b)){
        if(f(a)){
            return 0;
        }
    }
    return 1;
}

void main(int a){
    switch (g(3, a))
    {
    case 1:
        
        break;
    
    default:
        break;
    }
}