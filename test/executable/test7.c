int is_divisible(int a, int b){
    if(a <= 0) return 1;
    if(a < b) return 0;
    else is_divisible (a-b, b);
}

int are_divisible(int arr[10], int b){
    int number = 0;
    for(int i = 0; i < 10; i++){
        number = is_divisible (arr[i], b) ? number+1 :number ;
    }
    return number;
}
