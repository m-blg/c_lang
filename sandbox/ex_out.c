struct Foo {
    int x;
    int y;
    int z;
};

FmtError Foo_dbg_fmt(Foo *self, StringFormatter *fmt, void *_) {
    TRY(string_formatter_write_fmt(fmt, S("Foo:%+\nx: %d\ny: %d\nz: %d\n%-"), self->x, self->y, self->z));
    return FMT_ERROR(OK);
}

