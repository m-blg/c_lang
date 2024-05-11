#include <stdio.h>

int main() {
    char *s = "в";
    __uint32_t n = 'в';
    fwrite(s, sizeof(char), sizeof("в")-1, stdout);
}
