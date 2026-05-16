/**
 * This program iterate file content to match a pattern string.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include "list_head.h"

/**
 * match_result - structure used to represents the matched information,
 *                the information is the file offset at where pattern
 *                is matched
 * @file_offset:  file offset
 * @list:         a list collects all matched results
 * # the head match_result also will be used for store information.
 */
struct match_result {
        long file_offset;
        struct list_head list;
};

static inline void __match_result_init(struct match_result *r)
{
        r->file_offset = -1;
        INITIALIZE_LIST_HEAD(&r->list);
}

static struct match_result *match_result_alloc_init(void)
{
        struct match_result *rp = malloc(sizeof(struct match_result));
        if (rp) 
                __match_result_init(rp);
        return rp;
}

static inline void match_result_add(struct match_result *new, struct match_result *head)
{
        list_add(&new->list, &head->list);
}

static void match_result_print_all(struct match_result *head)
{
        struct match_result *next_item = NULL;

        fprintf(stdout, "file offset : %ld\n", head->file_offset);
        list_for_each_entry(next_item, &head->list, list) {
                fprintf(stdout, "file offset : %ld\n", next_item->file_offset);
        }
}

/* destroy all match results */
static void match_result_destroy_all(struct match_result *head)
{
        struct match_result *next_item, *temp_item;
        next_item = temp_item = NULL;

        list_for_each_entry_safe(next_item, temp_item,
                                 &head->list, list) {
                list_del(&next_item->list);
                free(next_item);
        }
        free(head);
}

/**
 * pattern - structure used to represents pattern,contains been
 *           pre-processed pattern string
 * @buffer_capacity:
 *           size in bytes of buffer contains null byte
 * @pattern_buffer:
 *           buffer to store pattern string
 */
struct pattern {
        size_t buffer_capacity;
        char *pattern_buffer;
};

/* alloc buffer for pattern structure and copy pattern string */
static bool set_pattern(struct pattern *p, const char *pattern_string)
{
        p->buffer_capacity = strlen(pattern_string) + 1;
        p->pattern_buffer = malloc(sizeof(char) * p->buffer_capacity);
        if (!p->pattern_buffer)
                return false;
        memcpy(p->pattern_buffer, pattern_string, p->buffer_capacity);
        return true;
}

/* dealloc the allocated pattern buffer */
static inline void destroy_pattern(struct pattern *p)
{
        free(p->pattern_buffer);
        p->buffer_capacity = 0;
        p->pattern_buffer = NULL;
}

/**
 * match_space - structure used to store the cut piece from text stream,
 *               pattern will try to matching on this text piece
 * @buffer_capacity:
 *               size in bytes of buffer contains null byte
 * @space_buffer:
 *               buffer to store text piece
 */
struct match_space {
        size_t buffer_capacity;
        char *space_buffer;
};

/* set space buffer,the size of space buffer should same as pattern's buffer size */
static bool set_match_space(struct match_space *m, size_t matching_string_length)
{
        m->buffer_capacity = matching_string_length + 1;
        m->space_buffer = malloc(sizeof(char) * m->buffer_capacity);
        return m->space_buffer != NULL;
}

/* destroy the allocated memory for the match space */
static inline void destroy_match_space(struct match_space *m)
{
        free(m->space_buffer);
        m->buffer_capacity = 0;
        m->space_buffer = NULL;
}

/* move text piece to match space */
static bool move_to_match_space(char *datum, size_t d_size, struct match_space *m)
{
        if (d_size >= m->buffer_capacity)
                return false;
        memset(m->space_buffer, 0, m->buffer_capacity);
        memmove(m->space_buffer, datum, d_size);
        return true;
}

typedef char (*preprocess_routine)(char);

/* pre-process lowercast alphabet */
static void preprocess_lowercast_alphabet(char *string, preprocess_routine f)
{
        for ( ; *string != '\0'; ++string) {
                if (*string >= 'a' && *string <= 'z')
                        *string = f(*string);
        }
}

/* not change */
static inline char no_upper_cast(char c)
{
        return c;
}

/* to upper cast */
static inline char upper_cast(char c)
{
        return c &= ~0x20;
}

static bool CAST_SENSITIVE = false;

/* match pattern on match space */
static bool match_it(struct pattern *p, struct match_space *m, preprocess_routine f)
{
        preprocess_lowercast_alphabet(m->space_buffer, f);
        return strncmp(p->pattern_buffer, m->space_buffer, p->buffer_capacity) == 0;
}

static void print_help_msg(void)
{
        fprintf(stdout, "usage: @program [ -s ] pattern file-path\n");
}

int main(int argc, char *argv[])
{
        /**
         * Only option "-s" is valid,so we do not use getopt() interface.
         * Just process simply checking is OK.
         */
        if (argc < 3) {
                print_help_msg();
                return -1;
        }

        const char *match_string = NULL;
        const char *file_path = NULL;

        switch (argc) {
        case 4:
                if (strncmp(argv[1], "-s", 3) == 0) {
                        CAST_SENSITIVE = true;
                        match_string = argv[2];
                        file_path = argv[3];
                } else {
                        print_help_msg();
                        return -1;
                }
                break;
        case 3:
                match_string = argv[1];
                file_path = argv[2];
                break;
        default:
                print_help_msg();
                return -1;
        }

        /* open file */
        FILE *fd = fopen(file_path, "r");
        if (!fd) {
                fprintf(stderr, "error: open file failed - %s\n", file_path);
                return -1;
        }

        /**
         * we initialize resource descriptors before allocate memory,then
         * we can simply jump to cleaning position when allocating failed.
         * free NULL pointer no segment fault.
         */
        int ret = 0;
        struct pattern p = {0};
        struct match_space m = {0};

        bool matched = false;
        struct match_result *mrs = NULL;

        size_t buffer_length = 4096;
        char *text_buffer = NULL;
        
        /* setup pattern buffer and match space buffer */
        if (!set_pattern(&p, match_string)) {
                fprintf(stderr, "error: set pattern failed - %s\n", match_string);
                ret = -1;
                goto exit_err_clean;
        }

        if (!set_match_space(&m, strlen(match_string))) {
                fprintf(stderr, "error: set match space failed\n");
                ret = -1;
                goto exit_err_clean;
        }

        /* match result collection */
        mrs = match_result_alloc_init();
        if (!mrs) {
                fprintf(stderr, "error: alloc match_result failed\n");
                ret = -1;
                goto exit_err_clean;
        }

        /* buffer initialization,once read operation at most 4kB */
        text_buffer = malloc(sizeof(char) * buffer_length);
        if (!text_buffer) {
                ret = -1;
                goto exit_err_clean;
        }
        memset(text_buffer, 0, buffer_length);
        
        char *to_store = text_buffer;
        size_t to_read = buffer_length;
        size_t valid_length = 0;
        size_t last_remain = 0;

        /* preprocess pattern early */
        if (!CAST_SENSITIVE)
                preprocess_lowercast_alphabet(p.pattern_buffer, upper_cast);

        /**
         * read text from file stream,and try match pattern.
         * the level-0 loop take charge of read from file to buffer.
         * the level-1 loop take charge of iterate text in buffer try
         * to match pattern.
         * if there is some data remained after level-1 loop ends,the
         * level-0 loop must move the remaining data to the head of
         * buffer,and next read operation should continue after the
         * remained data.
         */
        do {
                long current_offset = ftell(fd);

                ssize_t readed = fread(to_store, sizeof(char), to_read, fd);
                if (readed <= 0)
                        break;

                valid_length = readed + last_remain;
                if (valid_length < p.buffer_capacity - 1)
                        break;

                char *start = text_buffer;
                const char *end = text_buffer + valid_length;

                while (start < end) {
                        if (!move_to_match_space(start, p.buffer_capacity - 1, &m)) {
                                fprintf(stderr, "error: move content failed\n");
                                ret = -1;
                                goto exit_err_clean;
                        }

                        /**
                         * if we matched once,then we skip the matched text rather than
                         * start from the next byte of @start.
                         */
                        if (match_it(&p, &m, CAST_SENSITIVE ? no_upper_cast : upper_cast)) {
                                long the_offset = current_offset + (start - text_buffer);

                                if (!matched) {
                                        mrs->file_offset = the_offset;
                                        matched = true;
                                } else {
                                        struct match_result *mr = match_result_alloc_init();
                                        if (!mr) {
                                                fprintf(stderr, "error: alloc match result failed\n");
                                                ret = -1;
                                                goto exit_err_clean;
                                        }
                                        mr->file_offset = the_offset;
                                        match_result_add(mr, mrs);
                                }

                                start += p.buffer_capacity - 1;
                                valid_length -= p.buffer_capacity - 1;

                                if (valid_length < p.buffer_capacity - 1)
                                        break;

                                continue;
                        }
                        ++start;
                        --valid_length;

                        if (valid_length < p.buffer_capacity - 1)
                                break;

                }

                last_remain = valid_length;
                if (last_remain) {
                        /* splice the rest */
                        memmove(text_buffer, start, last_remain);
                        to_store = text_buffer + last_remain;
                        to_read = buffer_length - last_remain;
                } else {
                        to_store = text_buffer;
                        to_read = buffer_length;
                }

        } while (1);

        if (ferror(fd)) {
                fprintf(stderr, "error: read file failed.\n");
                ret = -1;
        } else if (matched)
                match_result_print_all(mrs);

exit_err_clean:
        free(text_buffer);
        destroy_match_space(&m);
        destroy_pattern(&p);

        if (mrs)
                match_result_destroy_all(mrs);

        fclose(fd);
        return ret;
}
