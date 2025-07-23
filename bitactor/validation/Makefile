# BitActor Validation Suite Makefile
CC = gcc
CFLAGS = -O3 -march=native -std=c11 -Wall -Wextra -mavx2 -DNDEBUG
LDFLAGS = -lm -lpthread

# Directories
SRC_DIR = ..
RUNTIME_DIR = $(SRC_DIR)/runtime
INTEGRATION_DIR = $(SRC_DIR)/integration
INCLUDE_DIR = $(SRC_DIR)/include

# Include paths  
INCLUDES = -I$(INCLUDE_DIR) -I$(RUNTIME_DIR) -I$(INTEGRATION_DIR)

# Source files
VALIDATOR_SRCS = tick_validator.c
RUNTIME_SRCS = $(RUNTIME_DIR)/bytecode_loader.c
INTEGRATION_SRCS = $(INTEGRATION_DIR)/cns_integration.c

# Object files
VALIDATOR_OBJS = $(VALIDATOR_SRCS:.c=.o)
RUNTIME_OBJS = $(RUNTIME_SRCS:.c=.o)
INTEGRATION_OBJS = $(INTEGRATION_SRCS:.c=.o)

# Targets
TARGET = bitactor_validator
ALL_OBJS = $(VALIDATOR_OBJS) $(RUNTIME_OBJS) $(INTEGRATION_OBJS)

.PHONY: all clean test benchmark

all: $(TARGET)

$(TARGET): $(ALL_OBJS)
	$(CC) $(ALL_OBJS) -o $@ $(LDFLAGS)

%.o: %.c
	$(CC) $(CFLAGS) $(INCLUDES) -c $< -o $@

test: $(TARGET)
	@echo "Running BitActor 8-tick validation..."
	./$(TARGET)

benchmark: $(TARGET)
	@echo "Running performance benchmarks..."
	./$(TARGET) --benchmark

clean:
	rm -f $(ALL_OBJS) $(TARGET)
	rm -f *.o core

# Development targets
debug: CFLAGS = -O0 -g -std=c11 -Wall -Wextra -mavx2 -DDEBUG
debug: $(TARGET)

profile: CFLAGS += -pg
profile: LDFLAGS += -pg  
profile: $(TARGET)

install: $(TARGET)
	cp $(TARGET) /usr/local/bin/

# Documentation
docs:
	@echo "BitActor Validation Suite"
	@echo "========================"
	@echo "Usage: make [target]"
	@echo ""
	@echo "Targets:"
	@echo "  all       - Build validator (default)"
	@echo "  test      - Run validation tests"  
	@echo "  benchmark - Run performance benchmarks"
	@echo "  debug     - Build with debug symbols"
	@echo "  profile   - Build with profiling support"
	@echo "  clean     - Remove build artifacts"
	@echo "  install   - Install to /usr/local/bin"