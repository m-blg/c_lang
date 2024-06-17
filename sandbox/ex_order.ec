
struct D {
    B *bp;
};

struct A {
    B b;
    D d;
};


int main() {
    foo();
}

struct B {
    A *ap;
    C c;
};

struct C {
    B *bp;
};


int g_x;

void foo() {
    print_fmt(S("test\n"));
}