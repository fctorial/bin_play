struct A {
    short a;
    int b;
};

const int x = 1;
int y = x + 2;
const struct A z = {1, 1};

int eho() {
    static int count = 0;
    count++;
    return 11;
}

int rot() {return eho() + x;}

int missing();

int main() {return missing();}

static void f() {}
