struct var{
    int mem;
};

struct var new_var = {16};

void enlarge(){
    new_var.mem *= 2;
}

void main(){
    enlarge();
    struct var copy_var = {new_var.mem};
}