CC = gcc
CFLAGS = -O3 -march=native -Wall -Wextra -std=c11
LDFLAGS = -lm

all: cns_clinician_test

cns_clinician_test: cns_clinician.c
	$(CC) $(CFLAGS) $< -o $@ $(LDFLAGS)

clean:
	rm -f cns_clinician_test

.PHONY: all clean
