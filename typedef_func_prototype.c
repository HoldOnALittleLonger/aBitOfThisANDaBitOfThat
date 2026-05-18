#include <stdio.h>

/**
 * type alias for function signature.
 * @print_something then can be used for
 * function declaration.
 * @print_something => void <func>(void)
 */
typedef void (print_something)(void);
/**
 * function declaration print_msg();
 * @print_something => void <func>(void)
 * @print_msg => <func>
 * result => void print_msg(void)
 */
print_something print_msg;
/**
 * function declaration print_msg2();
 */
print_something print_msg2;

/**
 * @print_msg_fp0 is a function pointer,the function
 * it points to need has the signature that
 * related to type definition print_something.
 */
print_something *print_msg_fp0 = print_msg;

/**
 * function declaration for void print_msg3(void).
 */
void (print_msg3)(void);

/**
 * global function pointer initialized to
 * print_msg().
 */
void (*print_msg_fp)(void) = print_msg;

/**
 * type alias for function pointer has
 * signature void(void) .
 */
typedef void (*print_something_t)(void);

int main(void)
{
        print_msg();
        print_msg2();
        print_msg3();
        print_msg_fp0();

        print_something_t pfp = print_msg;
        print_msg_fp();
        pfp();

        return 0;
}

void print_msg(void)
{
        fprintf(stdout, "function %s is called\n", __FUNCTION__);
}

void print_msg2(void)
{
        fprintf(stdout, "function %s is called\n", __FUNCTION__);
}

void print_msg3(void)
{
        fprintf(stdout, "function %s is called\n", __FUNCTION__);
}

