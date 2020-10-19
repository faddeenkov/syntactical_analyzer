void main(){
    int i = 5;
    
    if(i == 5){
        while(i > 3){
            i--;
        }
    }

    switch(i){
        case 0:
        i++;
        break;
        default:
        i--;
        break;
    }
}