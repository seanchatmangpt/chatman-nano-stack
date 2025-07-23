# Generated Makefile for OWL AOT Compilation
# Timestamp: 2025-07-23T10:39:24.232599

CC = gcc
CFLAGS = -std=c11 -Wall -Wextra -O2 -g
LDFLAGS = 
INCLUDES = -I.

# Source files
SOURCES = shacl_ontology.c
OBJECTS = $(SOURCES:.c=.o)
HEADERS = shacl_ontology.h

# Target
TARGET = owl_ontology

# Build rules
all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CC) $(OBJECTS) -o $@ $(LDFLAGS)

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) $(INCLUDES) -c $< -o $@

clean:
	rm -f $(OBJECTS) $(TARGET)

install: $(TARGET)
	cp $(TARGET) /usr/local/bin/
	cp $(HEADERS) /usr/local/include/

uninstall:
	rm -f /usr/local/bin/$(TARGET)
	rm -f /usr/local/include/owl_ontology.h

# Testing
test: $(TARGET)
	./$(TARGET) --self-test

# Documentation
docs:
	doxygen Doxyfile

.PHONY: all clean install uninstall test docs

# Dependencies
shacl_ontology.o: shacl_ontology.c $(HEADERS)
