typedef struct Foo Foo;
struct Foo {
    int x;
};

// int x = (any * expr), *y = 3 + 5 - 1, z[5 * x];

// struct Bar {
//     int (*fp)();
// };

// typedef int Foo2;
typedef int usize_t;

int
foo(int x) {
    int x = -3;
    Foo (*(*f)())[3];
    ((int) x ? 1: 0) * 3;
    0;
    x = y[x++] * (double) b;

    char c = '\t';
    bool b = true;
    switch (b & x++)
    {
    case 1:
        break;
    
    default:
        break;
    }
    for (usize_t i = 0; i < x; i++ + 1) {
        if (b ^ i) {
            label:
            return 3;
        } else  {
            return 0;
        }
    }
    while (some_expr)
    {
        printf("%d\n", 3);
    }
    do
    {
        if (f(b)) {
            break;
        } else {
            continue;
        }
        goto out;
    } while (1);
    
    
    out:
}

// TODO: test this
typedef int Foo2;

void test_type_name() {
    struct A a;
    a.Foo2 = 3;
    int Foo2 = 4;

    (a.Foo2 * Foo2);
    // (Foo2) Foo2;
}

// void
// bar(struct Bar b) {

// }

// char *s, foo(int x);;