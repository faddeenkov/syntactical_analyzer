int global = 1;

void niceFunction(float formal){
    float local = 3.14f;
    formal = local/2 + (float) global;
}