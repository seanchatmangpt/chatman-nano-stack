#include "test_service.h"
#include <string.h>

bool test_service_init(test_service_bitactor_t* actor) {
    memset(actor, 0, sizeof(test_service_bitactor_t));
    actor->state = 1;
    return true;
}

bool test_service_tick(test_service_bitactor_t* actor) {
    actor->tick_count++;
    return true;
}

int main() {
    test_service_bitactor_t actor;
    test_service_init(&actor);
    
    for (int i = 0; i < 100; i++) {
        test_service_tick(&actor);
    }
    
    return actor.tick_count == 100 ? 0 : 1;
}