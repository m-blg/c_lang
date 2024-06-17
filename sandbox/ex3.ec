@post_include("parsing/c/parsing.c")

typedef struct Foo Foo;
@derive(DebugFormat)
struct Foo {
    int x;
    int y;
    int z;
};

typedef struct Bar Bar;
@derive(DebugFormat)
struct Bar {
    int x;
    int y;
    int z;
};

int main() {
    ctx_init_default();
    struct Foo foo = (struct Foo) {
        .x = 3,
        .y = 4,
        .z = 5,
    };
    dbgp(Foo, &foo);

    struct Bar bar = (struct Bar) {
        .x = 6,
        .y = 7,
        .z = 8,
    };
    dbgp(Bar, &bar);
    ctx_deinit();
}