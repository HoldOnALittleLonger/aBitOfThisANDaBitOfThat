/**
 * This file contains some useful macro definitions.
 */
#ifndef _UTILITY_MACRO_H_
#define _UTILITY_MACRO_H_

#if defined(UM_CONFIG_X_MACRO)

#ifdef X_MACRO_INITIALIZER
#define X_MACRO(__type, __name, __initializer) __type __name = {(__initializer)};
#else /* X_MACRO HAS INITIALIZER */
#define X_MACRO(__type, __name) __type __name;
#endif /* X_MACRO NO INITIALIZER */

#ifndef LIST_VARIABLES
#if defined(X_MACRO_INITIALIZER)
#define LIST_VARIABLESN

#define LIST_VARIABLES1(__type, __initializer, __name1) \
        X_MACRO(__type, __name1, __initializer)

#define LIST_VARIABLES2(__type, __initializer, __name1, __name2)    \
        LIST_VARIABLES1(__type, __initializer, __name1)             \
        LIST_VARIABLES1(__type, __initializer, __name2)

#define LIST_VARIABLES3(__type, __initializer, __name1, __name2, __name3) \
        LIST_VARIABLES1(__type, __initializer, __name1)                 \
        LIST_VARIABLES2(__type, __initializer, __name2, __name3)

#define LIST_VARIABLES4(__type, __initializer, __name1, __name2, __name3, \
                        __name4)                                        \
        LIST_VARIABLES2(__type, __initializer, __name1, __name2)        \
        LIST_VARIABLES2(__type, __initializer, __name3, __name4)

#define LIST_VARIABLES5(__type, __initializer, __name1, __name2, __name3, \
                        __name4, __name5)                               \
        LIST_VARIABLES2(__type, __initializer, __name1, __name2)        \
        LIST_VARIABLES3(__type, __initializer, __name3, __name4, __name5)
        
#define LIST_VARIABLES6(__type, __initializer, __name1, __name2, __name3, \
                        __name4, __name5, __name6)                      \
        LIST_VARIABLES3(__type, __initializer, __name1, __name2, __name3) \
        LIST_VARIABLES3(__type, __initializer, __name4, __name5, __name6)   


#define LIST_VARIABLES7(__type, __initializer, __name1, __name2, __name3, \
                        __name4, __name5, __name6, __name7)             \
        LIST_VARIABLES1(__type, __initializer, __name1)                 \
        LIST_VARIABLES3(__type, __initializer, __name2, __name3, __name4) \
        LIST_VARIABLES3(__type, __initializer, __name5, __name6, __name7)

#define LIST_VARIABLES8(__type, __initializer, __name1, __name2, __name3, \
                        __name4, __name5, __name6, __name7, __name8)    \
        LIST_VARIABLES2(__type, __initializer, __name1, __name2)        \
        LIST_VARIABLES3(__type, __initializer, __name3, __name4, __name5) \
        LIST_VARIABLES3(__type, __initializer, __name6, __name7, __name8)

#endif
#endif /* LIST VARIABLES N */

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

#endif /* header end */
