int global = 2;

void f(){
    int j = 1;
    if(global == j){
        return;
    }
}

int main(int x){
    switch(x){
        case 1:
        global++;
        break;
        default:
        while(global < 1){
            global += x;
        }
    }


}