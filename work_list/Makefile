all: main

CC := gcc
CFLAGS := -g
SRC := work.c work_list.c test_main.c
LIB := pthread
HEADER_FLAGS := -I. -I..

main: $(SRC)
	$(CC) $(CFLAGS) -o $@ $^ $(HEADER_FLAGS) -l$(LIB)

.PHONY: clean
clean:
	rm -f *.o
