int double_number(int num){
    return num*2;
}

int increase_number(int num){
    return num+1;
}

int too_complicated_calculation (int num){
    return increase_number(double_number(num));
}