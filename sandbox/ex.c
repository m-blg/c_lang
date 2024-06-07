
// typedef struct Foo Foo;
// struct Foo {
//     int x;
// };

// int x = (any * expr), *y = 3 + 5 - 1, z[5 * x];

// struct Bar {
//     int (*fp)();
// };

// typedef int Foo2;

// BUG: should trigger unimplemented
void
foo(int x) {
    // int x;
    (int) x + 1;
}

// void
// bar(struct Bar b) {

// }

// char *s, foo(int x);;