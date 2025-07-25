CC = gcc
CFLAGS = -O3 -march=native -Wall -Wextra -std=c11
LDFLAGS = -lm

all: cns_litigator_test

cns_litigator_test: cns_litigator.c
	$(CC) $(CFLAGS) $< -o $@ $(LDFLAGS)

clean:
	rm -f cns_litigator_test

.PHONY: all clean
