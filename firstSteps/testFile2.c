float a = 1;
double b = 419.9;
int x = 41;

int find_by_index_x(int* arr, int arr_size){
    x++;
if(arr_size > x){
    return arr[x];
}
else return 0;
}