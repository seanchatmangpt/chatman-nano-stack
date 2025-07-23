#include "bitfiber.h"
#include <string.h>

// Initialize fiber scheduler
void fiber_scheduler_init(fiber_scheduler_t* sched) {
    memset(sched, 0, sizeof(fiber_scheduler_t));
    
    // Mark all fibers as ready
    for (int i = 0; i < BITFIBER_MAX_FIBERS; i++) {
        sched->fibers[i].state = FIBER_STATE_READY;
    }
}

// Spawn a new fiber
uint32_t fiber_spawn(fiber_scheduler_t* sched, uint32_t pc) {
    // Find free fiber slot
    for (uint32_t i = 0; i < BITFIBER_MAX_FIBERS; i++) {
        if (sched->fibers[i].state == FIBER_STATE_READY && 
            !(sched->schedule_mask & (1ULL << i))) {
            
            fiber_t* fiber = &sched->fibers[i];
            fiber->pc = pc;
            fiber->tick_count = 0;
            fiber->yield_point = 0;
            memset(fiber->regs, 0, sizeof(fiber->regs));
            
            // Add to schedule
            sched->schedule_mask |= (1ULL << i);
            sched->active_count++;
            
            return i;
        }
    }
    
    return UINT32_MAX; // No free slots
}

// Yield current fiber
void fiber_yield(fiber_scheduler_t* sched) {
    fiber_t* fiber = fiber_current(sched);
    fiber->state = FIBER_STATE_YIELDED;
    
    // Save yield point
    fiber->yield_point = fiber->pc;
}

// Complete current fiber
void fiber_complete(fiber_scheduler_t* sched) {
    fiber_t* fiber = fiber_current(sched);
    fiber->state = FIBER_STATE_COMPLETE;
    
    // Remove from schedule
    sched->schedule_mask &= ~(1ULL << sched->current_fiber);
    sched->active_count--;
}

// Schedule and execute fibers for one tick
__attribute__((hot))
void fiber_schedule_tick(fiber_scheduler_t* sched, bitactor_t* ba) {
    if (!fiber_has_ready(sched)) {
        return;
    }
    
    // Get next ready fiber
    uint32_t next = fiber_next_ready(sched);
    sched->current_fiber = next;
    
    fiber_t* fiber = &sched->fibers[next];
    fiber->state = FIBER_STATE_RUNNING;
    
    // Execute fiber bytecode for up to BITACTOR_TICK_BUDGET cycles
    uint32_t cycles = 0;
    uint32_t pc = fiber->yield_point ? fiber->yield_point : fiber->pc;
    
    while (cycles < BITACTOR_TICK_BUDGET && 
           pc < ba->bytecode_size && 
           fiber->state == FIBER_STATE_RUNNING) {
        
        bitinstr_t instr = ba->bytecode[pc];
        
        // Execute instruction on fiber registers
        switch (instr.opcode) {
            case 0x00: // NOP
                break;
                
            case 0x01: // AND
                fiber->regs[instr.dst] = fiber->regs[instr.src1] & fiber->regs[instr.src2];
                break;
                
            case 0x02: // OR
                fiber->regs[instr.dst] = fiber->regs[instr.src1] | fiber->regs[instr.src2];
                break;
                
            case 0x03: // XOR
                fiber->regs[instr.dst] = fiber->regs[instr.src1] ^ fiber->regs[instr.src2];
                break;
                
            case 0x04: // NOT
                fiber->regs[instr.dst] = ~fiber->regs[instr.src1];
                break;
                
            case 0x05: // SHL
                fiber->regs[instr.dst] = fiber->regs[instr.src1] << (fiber->regs[instr.src2] & 63);
                break;
                
            case 0x06: // SHR
                fiber->regs[instr.dst] = fiber->regs[instr.src1] >> (fiber->regs[instr.src2] & 63);
                break;
                
            case 0x0F: // YIELD
                fiber_yield(sched);
                return;
                
            case 0x10: // COMPLETE
                fiber_complete(sched);
                return;
        }
        
        pc++;
        cycles++;
    }
    
    // Update fiber state
    fiber->pc = pc;
    fiber->tick_count += cycles;
    
    // Auto-yield if tick budget exhausted
    if (cycles >= BITACTOR_TICK_BUDGET) {
        fiber_yield(sched);
    } else if (pc >= ba->bytecode_size) {
        fiber_complete(sched);
    }
}