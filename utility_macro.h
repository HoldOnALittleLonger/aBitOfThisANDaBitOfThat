/**
 * This file contains some useful macros and inline functions.
 */
#ifndef _UTILITY_MACRO_H_
#define _UTILITY_MACRO_H_

#if defined(UM_CONFIG_X_MACRO)

/* macros for declaration and initialization */
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

/* internal helpers */
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

/* user interfaces */
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

/* internal helpers */
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

/* user interfaces */
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
                struct timespec ts = {0};                               \
                PROFILE_GET_MONOTONIC_TIME(&ts);                        \
                if (um_profile_err_give_up) {                           \
                        fprintf(stderr, "PROFILE: Have given up\n");    \
                        break;                                          \
                }                                                       \
                printf("%s profile: time cost - %fs\n",                 \
                       __label,                                         \
                       PROFILE_TIMESPEC2SECS(&ts) - um_profile_time_begin); \
        } while (0)

#endif /* profile */

#if defined(UM_CONFIG_LIKELY)

/* used compiler feature to similar likely() and unlikely() */
#define um_compile_time_test_and_expect(__value, __expect) ({  \
        __builtin_const_p((__value)) ?                         \
        !!(__value) : __builtin_expect(!!(__value), __expect); \
                })

#define um_likely(__value) __builtin_expect(!!(__value), 1)
#define um_unlikely(__value) __builtin_expect(!!(__value), 0)

#endif /* likely */

#if defined (UM_CONFIG_BIT_OP)
#include <stdint.h>

/**
 * um_highest_ord - use a loop to calculate the order of the
 *                  most significant bit
 * @v:              the value
 * return:          order of most significant bit,return 0
 *                  if @v is 0
 */
static inline uint8_t um_highest_ord(unsigned long long v)
{
        uint8_t ord = 0;
        while (v != 0) v >>=1, ++ord;
        return ord ? ord - 1 : 0;
}

/**
 * UM_NEXT_POWER_OF2 - macro function to calculates the next
 *                     value that is power of 2,start from
 *                     a given value
 * @__value:           value to start
 * return:             unsigned long long value which is
 *                     power of 2 next to @__value
 */
#define UM_NEXT_POWER_OF2(__value) ({           \
        1ULL << (um_highest_ord(__value) + 1);  \
                })

/**
 * UM_ALIGN_TO - macro function to compute the aligned value
 * @__value:     the value want aligns to @__alignment
 * @__alignment: alignment,must be power of 2
 * return:       the value been aligned to @__alignment
 * # @__value and @__alignment should be unsigned integers.
 */
#define UM_ALIGN_TO(__value, __alignment) ({                    \
        ((__value) + (__alignment) - 1U) & ~(__alignment - 1U); \
        })

#ifdef __x86_64__

/**
 * UM_ASM_SWAB - inline assembly to executes "bswap" ins on
 *               x86-64 platform.
 */
#define UM_ASM_SWAB(__io_var, __ins_suffix)          \
        asm volatile(                                \
                "bswap"#__ins_suffix" %0\n\t"        \
                : "=r"(__io_var) : "0"(__io_var))

/**
 * um_swab32 - exchange the bytes of 32-bit integer,
 *             swap first and last
 *             swap second and next to last
 *             ...
 * @__v:       value
 * return:     32-bit integer that been swapped
 */
#define um_swab32(__v) ({                   \
        uint32_t __internal_var = __v;      \
        UM_ASM_SWAB(__internal_var, l);     \
        __internal_var = __internal_var;    \
                })

/* um_swab64 - 64 bit integer version */
#define um_swab64(__v) ({                   \
        uint64_t __internal_var = __v;      \
        UM_ASM_SWAB(__internal_var, q);     \
        __internal_var = __internal_var;    \
                })

#else /* !__x86_64__ */

/* constant versions */
#define um_swab32(__v) ({                           \
        uint32_t __v_32 = __v;                      \
        __v_32 = ((__v_32 >>24) |                   \
                  (__v_32 << 24) |                  \
                  ((__v_32 & (255UL << 8)) << 8) |  \
                  ((__v_32 & (255UL << 16)) >> 8)); \
        })

#define um_swab64(__v) ({                              \
        uint64_t __v_64 = __v;                         \
        __v_64 = ((__v_64 >> 56) |                     \
                  (__v_64 << 56) |                     \
                  ((__v_64 & (255ULL << 8)) << 40) |   \
                  ((__v_64 & (255ULL << 48)) >> 40) |  \
                  ((__v_64 & (255ULL << 16)) << 24) |  \
                  ((__v_64 & (255ULL << 40)) >> 24) |  \
                  ((__v_64 & (255ULL << 24)) << 8) |   \
                  ((__v_64 & (255ULL << 32)) >> 8));   \
        })

#endif /* platform branch */

#endif /* binary op */

#if defined(UM_CONFIG_TIME_COMPARE)

#include <time.h>

/**
 * um_time_after - test whether timepoint @t1 is after timepoint @t2.
 * @t1:            timepoint 1
 * @t2:            timepoint 2
 * return:         true  => @t1 is after than @t2
 *                 false => @t1 is not after than @t2
 * # time after:
 *     if @t1 is after than @t2,then the value of @t1 must be
 *     greater than @t2.
 */
static inline bool um_time_after(const struct timespec *t1, const struct timespec *t2)
{
        return ((long)t2->tv_sec - (long)t1->tv_sec) < 0 ||
                (((long)t2->tv_sec - (long)t1->tv_sec) == 0 &&
                 ((long)t2->tv_nsec - (long)t1->tv_nsec < 0));
}

#define um_time_before(__timespec_t1p, __timespec_t2p) ({   \
        um_time_after(__timespec_t2p, __timespec_t1p);      \
        })

#define um_time_eq(__timespec_t1p, __timespec_t2p) ({   \
        !um_time_after(__timespec_t1p, __timespec_t2p)  \
        &&                                              \
        !um_time_before(__timespec_t1p, __timespec_t2p);\
        })

#define um_time_before_eq(__timespec_t1p, __timespec_t2p) ({ \
        !um_time_after(__timespec_t1p, __timespec_t2p);      \
        })

#define um_time_after_eq(__timespec_t1p, __timespec_t2p) ({ \
        !um_time_before(__timespec_t1p, __timespec_t2p);    \
        })

#define um_time_out(__current_time, __time_deadline) ({ \
        um_time_after(__current_time, __time_deadline); \
        })

/* time_t version */
static inline bool um_time_after_time_t(time_t t1, time_t t2)
{
        return (long)t2 - (long)t1 < 0;
}

#define um_time_before_time_t(__time1, __time2) ({ \
        um_time_after_time_t(__time2, __time1);    \
        })

#define um_time_eq_time_t(__time1, __time2) ({ \
        __time1 == __time2;                    \
        })

#define um_time_after_eq_time_t(__time1, __time2) ({ \
        !um_time_before_time_t(__time1, __time2);    \
        })

#define um_time_before_eq_time_t(__time1, __time2) ({ \
        !um_time_after_time_t(__time1, __time2);      \
        })

#endif /* time compare */

#endif /* header end */
