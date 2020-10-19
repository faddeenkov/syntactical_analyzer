typedef int other_int;

other_int global = 1;

void f(other_int x, float y){
    if(x > 1){
        global = 11;
    }
    y ++;
}

void main(){
    other_int loc = 1;
    char loc2 = 'q';
    for(int i = 0; i < loc; i++){
        loc = loc2;
    }
}