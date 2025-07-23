/*
 * Test C code with error handling that will be removed
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char* data;
    size_t size;
} buffer_t;

/* Original version with error handling */
buffer_t* create_buffer(size_t size) {
    buffer_t* buf = malloc(sizeof(buffer_t));
    if (!buf) {
        fprintf(stderr, "Failed to allocate buffer struct\n");
        return NULL;
    }
    
    buf->data = malloc(size);
    if (!buf->data) {
        fprintf(stderr, "Failed to allocate buffer data\n");
        free(buf);
        return NULL;
    }
    
    buf->size = size;
    return buf;
}

int process_data(const char* input) {
    if (!input) {
        fprintf(stderr, "NULL input provided\n");
        return -1;
    }
    
    size_t len = strlen(input);
    if (len == 0) {
        fprintf(stderr, "Empty input string\n");
        return -1;
    }
    
    buffer_t* buf = create_buffer(len + 1);
    if (!buf) {
        return -1;
    }
    
    strcpy(buf->data, input);
    printf("Processed: %s\n", buf->data);
    
    free(buf->data);
    free(buf);
    return 0;
}

/* Version without error handling - let it crash */
buffer_t* create_buffer_no_checks(size_t size) {
    buffer_t* buf = malloc(sizeof(buffer_t));
    /* NO NULL CHECK - will crash if malloc fails */
    
    buf->data = malloc(size);
    /* NO NULL CHECK - will crash if malloc fails */
    
    buf->size = size;
    return buf;
}

int process_data_no_checks(const char* input) {
    /* NO NULL CHECK - will crash if input is NULL */
    
    size_t len = strlen(input);
    /* NO EMPTY CHECK - process anyway */
    
    buffer_t* buf = create_buffer_no_checks(len + 1);
    /* NO NULL CHECK - will crash if allocation failed */
    
    strcpy(buf->data, input);
    printf("Processed: %s\n", buf->data);
    
    free(buf->data);
    free(buf);
    return 0;
}

int main() {
    printf("Testing error handling vs let-it-crash\n");
    
    /* Test with error handling */
    printf("\nWith error handling:\n");
    process_data("Hello");
    process_data(NULL);  /* Handled gracefully */
    process_data("");    /* Handled gracefully */
    
    /* Test without error handling */
    printf("\nWithout error handling (will crash on NULL):\n");
    process_data_no_checks("Hello");
    /* process_data_no_checks(NULL); */ /* WOULD CRASH - commented out */
    
    return 0;
}