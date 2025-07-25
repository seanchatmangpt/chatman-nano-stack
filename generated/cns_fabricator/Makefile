CC = gcc
CFLAGS = -O3 -march=native -Wall -Wextra -std=c11
LDFLAGS = -lm

all: cns_fabricator_test

cns_fabricator_test: cns_fabricator.c
	$(CC) $(CFLAGS) $< -o $@ $(LDFLAGS)

clean:
	rm -f cns_fabricator_test

.PHONY: all clean
