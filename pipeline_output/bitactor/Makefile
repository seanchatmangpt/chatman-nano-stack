CC = gcc
CFLAGS = -Wall -Wextra -O2
TARGET = pipeline_bitactor
OBJS = pipeline_bitactor.o

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(OBJS) -o $(TARGET)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJS) $(TARGET)

.PHONY: all clean
