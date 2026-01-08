#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>

void cleanup_free(void *pointer)
{
        fprintf(stderr, "cleanup_free() is called,@pointer : %p\n", pointer);
        void **pp = (void **)pointer;
        free(*pp);
        fprintf(stderr, "freed memory space at address %p\n", *pp);
}

int main(void)
{
        __attribute__((cleanup(cleanup_free))) 
                int *p_int = malloc(sizeof(int));

        if (!p_int) {
                fprintf(stderr, "allocate memory failed\n");
                return -1;
        }

        fprintf(stderr, "allocated memory,pointer : %p,"
                         "address of pointer object : %p\n",
                p_int, &p_int);

        return 0;
}



