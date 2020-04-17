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

int main(){
    typedef char zeichen;
    union SomeUnion {
    int i;
    char str[4];
}; 
}
