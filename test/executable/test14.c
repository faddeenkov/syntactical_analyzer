struct student //GCompTag
 {
  char name[1];
  int roll;
  float marks;
 };

enum anonym;

enum State {Working = 1, Failed = 0};

union SomeUnion {
    int i;
    char str[4];
}; 