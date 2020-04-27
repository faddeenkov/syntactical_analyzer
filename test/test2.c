char global = '1';

void f(int x){
    global = x;
}

int g(){
    if(global == '2'){
        return 1;
    }
    else{
        return 2;
    }
}

void h(){
    global = '?';
}