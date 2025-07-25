#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main(int argc, char** argv) {
    if (argc < 2) {
        return 1;
    }
    
    // Validate input length
    if (strlen(argv[1]) > 1000) {
        return 1;  // Reject too long input
    }
    
    // Check for null bytes
    for (int i = 0; i < strlen(argv[1]); i++) {
        if (argv[1][i] == 0) {
            return 1;  // Reject null bytes
        }
    }
    
    // Survived validation
    return 0;
}