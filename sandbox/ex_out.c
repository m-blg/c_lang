typedef struct Foo Foo;

struct Foo {
    int x;
};

struct Bar {
    int (*fp)();
};

void foo(Foo x) {}

void bar(struct Bar b) {}

char *s, char foo(int x);

