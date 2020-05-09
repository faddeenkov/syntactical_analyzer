int f(int a){return 0;}
int g(int a){return f(a) == 2 ? 1 : 0;}

void main(){
    if(f(2)){
        int x = 3;
    }

    if(g(2)){
        int y = 4;
    }

    for(int i = 0; i < f(3); i++){

    }

    switch (f(4)){
        case 1:
        break;
        default:
        break;
    }

    while(f(5) >2){

    }
}