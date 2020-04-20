typedef unsigned char BYTE; //GType

struct student;

struct student //GCompTag
 {
  char name[100];
  int roll;
  float marks;
 };

typedef struct uni_tag
{
    char name[];
} uni;

struct student me;



struct mystery; //GCompTagDecl

enum State {Working = 1, Failed = 0}; //GEnumTag

enum anonym; //GEnumTagDecl

BYTE x[] = {'b'};

int f (BYTE b){
    return b++;
}

int main(){
    typedef char zeichen;
    union SomeUnion {
    int i;
    char str[4];
}; 

    x[0] = 'a';

    BYTE z = x[0];

    int x2;
    x2 = 7;
    

    /**
     * __try{
        int j = x2;
        x2 = j*3;
    }
    __finally{

    } **/

    int k = f(x+4-x2);

    switch(x2){
        case 1:
            x2 = 1;
        break;
        case 7:
            x[0] = x2;
        break;
        default:
        break;
    }

    while(x2 < 17){
        if(x2 == 2){
            break;
        }
        else{
            x2 = x2 + 1;
        }
    }

    if(x2 == 7){
        return f(x2);
    }
    else{
        return x2 - x[0];
    }

}
