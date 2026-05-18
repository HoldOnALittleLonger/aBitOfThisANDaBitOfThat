/**
 * This file contains some useful macro definitions.
 */
#ifndef _UTILITY_MACRO_H_
#define _UTILITY_MACRO_H_

#if defined(UM_CONFIG_X_MACRO)

#define X_MACRO_DECLARATION(__type, __name)     \
        __type __name;
#define X_MACRO_DECLARATION_INIT(__type, __name, __initializer) \
        __type __name = {(__initializer)};

#define X_MACRO_DECLARATION_LINKAGE(__linkage, __type, __name) \
        __linkage X_MACRO_DECLARATION(__type, __name)

#define X_MACRO_DECLARATION_INIT_LINKAGE(__linkage, __type, __name, __initializer) \
        __linkage X_MACRO_DECLARATION_INIT(__type, __name, __initializer)

#define X_MACRO_DECLARATION_STATIC(__type, __name)  \
        X_MACRO_DECLARATION_LINKAGE(static, __type, __name)

#define X_MACRO_DECLARATION_INIT_STATIC(__type, __name, __initializer)  \
        X_MACRO_DECLARATION_INIT_LINKAGE(static, __type, __name, __initializer)

#define X_MACRO_DECLARATION_CV(__cv, __type, __name) \
        __cv X_MACRO_DECLARATION(__type, __name)

#define X_MACRO_DECLARATION_INIT_CV(__cv, __type, __name, __initializer) \
        __cv X_MACRO_DECLARATION_INIT(__type, __name, __initializer)

#define X_MACRO_DECLARATION_CONST(__type, __name) \
        X_MACRO_DECLARATION_CV(const, __type, __name)

#define X_MACRO_DECLARATION_INIT_CONST(__type, __name, __initializer) \
        X_MACRO_DECLARATION_INIT_CV(const, __type, __name, __initializer)

#define X__DO0()                                \
        __DO()

#define X__DO1(__DO, __param1)                  \
        __DO((__param1))

#define X__DO2(__DO, __param1, __param2)          \
        X__DO1(__DO, __param1)                   \
        X__DO1(__DO, __param2)

#define X__DO4(__DO, __param1, __param2, __param3, __param4)    \
        X__DO2(__DO, __param1, __param2)                      \
        X__DO2(__DO, __param3, __param4)

#define FOR_LIST_OF_VARIABLES0(__DO)            \
        X__DO0()

#define FOR_LIST_OF_VARIABLES1(__DO, __param1)   \
        X__DO1(__DO, __param1)

#define FOR_LIST_OF_VARIABLES2(__DO, __param1, __param2)  \
        FOR_LIST_OF_VARIABLES1(__DO, __param1)           \
        FOR_LIST_OF_VARIABLES1(__DO, __param2)

#define FOR_LIST_OF_VARIABLES4(__DO, __param1, __param2, __param3, __param4) \
        FOR_LIST_OF_VARIABLES2(__DO, __param1, __param2)                  \
        FOR_LIST_OF_VARIABLES2(__DO, __param3, __param4)

#endif /* X macro */

#if defined(UM_CONFIG_CONTAINER_OF)

/**
 * container_of - get the base address of the structure which contains
 *                the specified object.
 *                this relies on C compiler feature that the offset of
 *                member of a structure to the container can be known
 *                at compile time.
 * @__ptr:        the object's address
 * @__type:       the type contains this object
 * @__member:     the name of that member corresponding to this object
 *                in the structure
 * return:        pointer to the base address
 */
#define container_of(__ptr, __type, __member)                           \
        ({                                                              \
                typeof(((__type *)0)->__member) *mptr = __ptr;          \
                (__type *)((char *)mptr - offsetof(__type, __member));  \
        })

#endif /* container_of */

/**
 * Simple profile.
 */
#if defined(UM_CONFIG_PROFILE)
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <errno.h>

#define PROFILE_TIMESPEC2SECS(__ts_ptr)                         \
        ({                                                      \
                (double)((__ts_ptr)->tv_sec) +                  \
                        (double)((__ts_ptr)->tv_nsec) / 1.0e9f; \
        })

static bool um_profile_err_give_up = false;
#define PROFILE_GET_TIME(__clock_id, __ts_ptr)                      \
        do {                                                        \
                errno = 0;                                          \
                if (clock_gettime(__clock_id, __ts_ptr) < 0) {      \
                        fprintf(stderr,                             \
                                "error: %s PROFILE GET TIME: %s\n", \
                                __FILE__, strerror(errno));         \
                        fprintf(stderr, "PROFILE: Give up\n");      \
                        um_profile_err_give_up = true;              \
                }                                                   \
        } while (0)

#define PROFILE_GET_MONOTONIC_TIME(__ts_ptr)        \
        PROFILE_GET_TIME(CLOCK_MONOTONIC, __ts_ptr)

static double um_profile_time_begin = 0;
#define UM_PROFILE_BEGIN()                                          \
        do {                                                        \
                um_profile_time_begin = 0;                          \
                um_profile_err_give_up = false;                     \
                struct timespec ts = {0};                           \
                PROFILE_GET_MONOTONIC_TIME(&ts);                    \
                um_profile_time_begin = PROFILE_TIMESPEC2SECS(&ts); \
        } while (0)

#define UM_PROFILE_END(__label)                                         \
        do {                                                            \
                if (um_profile_err_give_up) {                           \
                        fprintf(stderr, "PROFILE: Have given up\n");    \
                        break;                                          \
                }                                                       \
                struct timespec ts = {0};                               \
                PROFILE_GET_MONOTONIC_TIME(&ts);                        \
                printf("%s profile: time cost - %fs\n",                 \
                       __label,                                         \
                       PROFILE_TIMESPEC2SECS(&ts) - um_profile_time_begin); \
        } while (0)

#endif /* profile */

#if defined(UM_CONFIG_LIKELY)

#define um_compile_time_test_and_expect(__value, __expect) ({  \
        __builtin_const_p((__value)) ?                         \
        !!(__value) : __builtin_expect(!!(__value), __expect); \
                })

#define um_likely(__value) __builtin_expect(!!(__value), 1)
#define um_unlikely(__value) __builtin_expect(!!(__value), 0)

#endif /* likely */

#endif /* header end */
