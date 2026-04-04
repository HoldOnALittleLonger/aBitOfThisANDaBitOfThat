#include <stdio.h>

int main(void)
{
        unsigned short v = 0xfe;
        while (v) {
                printf("%u", v & 0x01);
                v >>= 1;
        }
        putchar('\n');
        return 0;
}
