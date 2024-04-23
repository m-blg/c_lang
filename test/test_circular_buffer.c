
#include <criterion/criterion.h>
#include "core/string.c"


SLICE_IMPL(int32_t)
CIRCULAR_BUFFER_IMPL(int32_t)

Test(CircularBuffer, basic)
// int main()
{
    printf("start""\n");

    ctx_init_default();

    int32_t x = 3;
    CircularBuffer(int32_t) buff;
    ASSERT_OK(circular_buffer_proc(int32_t, new_in)(8, _ctx.global_alloc, &buff));
    circular_buffer_proc(int32_t, push)(&buff, &x);
    printf("%d" "\n", *circular_buffer_proc(int32_t, last)(&buff));
    circular_buffer_proc(int32_t, free)(&buff);
}