#include "parsing/c/parsing.c"
typedef struct Foo Foo;

struct Foo {
    int x;
    int y;
    int z;
};

FmtError Foo_dbg_fmt(Foo *self, StringFormatter *fmt, void *_) {
    TRY(string_formatter_write_fmt(fmt, S("Foo:%+\nx: %d\ny: %d\nz: %d\n%-"), self->x, self->y, self->z));
    return FMT_ERROR(OK);
}

typedef struct Bar Bar;

struct Bar {
    int x;
    int y;
    int z;
};

FmtError Bar_dbg_fmt(Bar *self, StringFormatter *fmt, void *_) {
    TRY(string_formatter_write_fmt(fmt, S("Bar:%+\nx: %d\ny: %d\nz: %d\n%-"), self->x, self->y, self->z));
    return FMT_ERROR(OK);
}

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
    ctx_deinit();}

