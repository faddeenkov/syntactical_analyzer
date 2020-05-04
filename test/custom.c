
typedef unsigned char BYTE; //GType

struct student;

int test;

struct student //GCompTag
 {
  char name[1];
  int roll;
  float marks;
 };

typedef struct uni_tag
{
    char name;
} uni;

struct student me;

double glob_var;

struct mystery; //GCompTagDecl

enum State {Working = 1, Failed = 0}; //GEnumTag

enum anonym; //GEnumTagDecl

BYTE x[] = {'b'};

void useless(){}

int f (BYTE b){
    glob_var = 123.456;
    BYTE c = b-1;
    if(b == 1){
        f(b-1);
    }
    return b;
}


int main(){
    typedef char zeichen;
    union SomeUnion {
    int i;
    char str[4];
}; 
    int i = 0;
    struct student person = {{'a'}, 3, 1.0f};
    struct student *someone = &person;
    char c = (*someone).name[i];

    int *ptr = &i;

    x[0] = 'a';

    BYTE z = x[0];

    int x2;
    x2 = 7;
    
    useless();

    /**
     * __try{
        int j = x2;
        x2 = j*3;
    }
    __finally{

    } **/

    for(int i = 0; i < 10; i++){
       int a[i + 3];
    }

    for(int i = 0; i < 20; i++){

    }
    
    return f(x+4-x2);

    BYTE arr[] = {2};
    switch(x[0]){
        case 1:
            x2 = 1;
        break;
        case 7:
            x[0] = x2;
        break;
        default:
        break;
    }

glob_var = 3.0;

    while(x2 < 17){
        if(x2 == 2){
            break;
        }
        else{
            x2 = x2 + 1;
        }
    }

    if(f(2) == 7){
        return f(x2);
    }
    else{
        return x2 - x[0];
    }

    test = 3;

}
