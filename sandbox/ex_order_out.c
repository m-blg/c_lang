struct C;

typedef struct C C;

struct B;

typedef struct B B;

struct D;

typedef struct D D;

struct A;

typedef struct A A;

struct C {
    B *bp;
};

struct B {
    A *ap;
    C c;
};

struct D {
    B *bp;
};

struct A {
    B b;
    D d;
};

int g_x;

int main();

void foo();

int main() {
    
    foo();
}

void foo() {
    
    print_fmt(S("test\n"));
}

