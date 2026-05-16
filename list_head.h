/**
 * This is the list_head implementation ported from Linux Kernel.
 * This is not full implementations,just a part that maybe used often.
 * Almost all operations need pointer type arguments.
 * And almost operations do not check NULL pointer,caller must ensure
 * NULL pointer is not passed.
 */

#ifndef __LIST_HEAD_H_
#define __LIST_HEAD_H_

#define UM_CONFIG_CONTAINER_OF
#include "utility_macro.h"

#include <stddef.h>

/**
 * list_head - the embedded ring doubly linked list ported from Linux Kernel
 * @prev:      prev list_head
 * @next:      next list_head
 */
struct list_head {
        struct list_head *prev;
        struct list_head *next;
};

/* DECLARE_LIST_HEAD - declare a list_head object has @name */
#define DECLARE_LIST_HEAD(__name)               \
        struct list_head __name

/* INITIALIZE_LIST_HEAD - initialize a list_head object,must pass pointer */
#define INITIALIZE_LIST_HEAD(__head_ptr)                                \
        do {                                                            \
                (__head_ptr)->prev = (__head_ptr)->next = (__head_ptr); \
        } while (0)

/* list_head_init - initialize a given list,pass by pointer */
static inline void list_head_init(struct list_head *head)
{
        INITIALIZE_LIST_HEAD(head);
}

/* list_empty - test whether @head is empty list */
static inline bool list_empty(struct list_head *head)
{
        return head == head->next;
}

/* list_empty_careful - test whether @head is empty list,and not being modified */
static inline bool list_empty_careful(struct list_head *head)
{
        return head == head->next && head->next == head->prev;
}

/* __list_del - helper to splice @prev and @next of the deleting list node */
static inline void __list_del(struct list_head *prev, struct list_head *next)
{
        prev->next = next;
        next->prev = prev;
}

/* list_del - delete a list node from list */
static inline void list_del(struct list_head *head)
{
        __list_del(head->prev, head->next);
}

/* list_del_init - delete a list node and reset it for reuse */
static inline void list_del_init(struct list_head *head)
{
        __list_del(head->prev, head->next);
        list_head_init(head);
}

/**
 * __list_add - add a new list_head into the given list
 * @new:        new node
 * @prev:       list node will become previous node of @new
 * @next:       list node will become next node of @new
 */
static inline void __list_add(struct list_head *new, struct list_head *prev, struct list_head *next)
{
        next->prev = new;
        new->next = next;
        new->prev = prev;
        prev->next = new;
}

/* list_add - add a new list node */
#define list_add(__new, __head)                 \
        __list_add(__new, __head, (__head)->next)

/* list_add_tail - add a new list node to tail */
#define list_add_tail(__new, __head)            \
        __list_add(__new, (__head)->prev, __head)

/**
 * list_move - move list
 * @list:      the list need to moved
 * @head:      the list head @list will be moved into
 * # This routine do not move current list node that represented by @list
 */
static inline void list_move(struct list_head *list, struct list_head *head)
{
        __list_del(list->prev, list->next);
        list_add(list, head);
}

/* list_move_tail - move a list to tail of another list */
static inline void list_move_tail(struct list_head *list, struct list_head *head)
{
        __list_del(list->prev, list->next);
        list_add_tail(list, head);
}

/**
 * This routine excludes current list node from the spliced new list.
 */
static inline void __list_splice(struct list_head *list, struct list_head *prev, struct list_head *next)
{
        list->next->prev = prev;
        prev->next = list->next;
        list->prev->next = next;
        next->prev = list->prev;
}

/**
 * list_splice - splice two list,excludes current list node
 * @list:        the list need to splice,represents current list node
 * @head:        the another list
 */
static inline void list_splice(struct list_head *list, struct list_head *head)
{
        if (!list_empty(list))
                __list_splice(list, head, head->next);
}

/* list_splice_tail - splice two list,the first goes to the tail of another one */
static inline void list_splice_tail(struct list_head *list, struct list_head *head)
{
        if (!list_empty(list))
                __list_splice(list, head->prev, head);
}

/* list_entry - retrieve container entry this list embedded in */
#define list_entry(__ptr, __type, __member) container_of(__ptr, __type, __member)

/* list_for_each - traverse each list node */
#define list_for_each(__iter, __head) \
        for (__iter = (__head)->next; __iter != __head; __iter = __iter->next)

/* list_for_each_entry - traverse each list entry */
#define list_for_each_entry(__entry, __head, __member)                  \
        for (__entry = list_entry((__head)->next, typeof(*__entry), __member); \
             &__entry->__member != __head;                              \
             __entry = list_entry(__entry->__member.next, typeof(*__entry), __member))

/**
 * list_for_each_entry_safe - traver each list entry,safe version,can be used to destroy
 *                            list nodes
 * @__entry_ptr:              local object is type of entry pointer,can be accessed in the
 *                            loop
 * @__next_entry_ptr:         temporary pointer used for save the next entry's address
 * @__head:                   list head
 * @__member:                 member name in the structure corresponds to the list_head object
 */
#define list_for_each_entry_safe(__entry_ptr, __next_entry_ptr, __head, __member) \
        for (__entry_ptr = list_entry((__head)->next, typeof(*__entry_ptr), __member), \
                     __next_entry_ptr = list_entry(__entry_ptr->__member.next, typeof(*__entry_ptr), __member); \
             &__entry_ptr->__member != __head;                          \
             __entry_ptr = __next_entry_ptr,                            \
                     __next_entry_ptr = list_entry(__entry_ptr->__member.next, typeof(*__entry_ptr), __member))


#endif
