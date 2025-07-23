"""
SIMD Bytecode Optimizer for BitActor
Optimizes constraint checking using AVX2 256-bit vector operations
"""
import os
from typing import Dict, List, Optional, Tuple, Any, Set
from enum import Enum
from dataclasses import dataclass

from .ir import BitcodeIR, Opcode, BitInstruction, BitcodeProgram
from .advanced_constraints import AdvancedConstraintCompiler, SIMDOperation

class SIMDPattern(Enum):
    """SIMD optimization patterns"""
    PARALLEL_CMP = "parallel_cmp"        # Multiple comparisons in parallel
    BATCH_RANGE = "batch_range"          # Batch range checking
    VECTOR_LOGIC = "vector_logic"        # Parallel logic operations
    MASKED_STORE = "masked_store"        # Conditional stores with masks
    REDUCTION = "reduction"              # Horizontal reductions (sum, min, max)

@dataclass
class SIMDInstruction:
    """SIMD instruction with vector width and lane info"""
    opcode: Opcode
    dst: int
    src1: int
    src2: int
    vector_width: int = 8  # 256-bit / 32-bit = 8 lanes
    lane_mask: int = 0xFF  # All lanes active by default
    
    def to_bit_instruction(self) -> BitInstruction:
        """Convert to standard bit instruction"""
        return BitInstruction(self.opcode, self.dst, self.src1, self.src2)

class SIMDOptimizer:
    """SIMD bytecode optimizer for high-performance constraint checking"""
    
    def __init__(self):
        self.vector_regs = list(range(16, 32))  # v0-v15 vector registers
        self.current_vreg = 0
        self.optimization_stats = {
            'scalar_ops_vectorized': 0,
            'instructions_eliminated': 0,
            'estimated_speedup': 1.0,
            'simd_patterns_applied': {}
        }
        
        # SIMD instruction tick costs (optimized for AVX2)
        self.simd_tick_costs = {
            Opcode.VLOAD: 1,
            Opcode.VSTORE: 1,
            Opcode.VADD: 1,
            Opcode.VSUB: 1,
            Opcode.VMUL: 1,
            Opcode.VAND: 1,
            Opcode.VOR: 1,
            Opcode.VXOR: 1,
            Opcode.VCMP: 1,
            Opcode.VMIN: 1,
            Opcode.VMAX: 1,
            Opcode.VMASK: 1,
            Opcode.VPACK: 1,
            Opcode.VUNPACK: 1,
            Opcode.VRANGE: 2,    # min + max check
            Opcode.VPATTERN: 3,  # Pattern matching complexity
            Opcode.VEXCL: 1,     # Exclusive adjustment
            Opcode.VCOUNT: 2,    # Count with reduction
        }
    
    def optimize_program(self, program: BitcodeProgram) -> BitcodeProgram:
        """Apply SIMD optimizations to entire program"""
        optimized_blocks = {}
        
        for label, block in program.blocks.items():
            optimized_blocks[label] = self._optimize_block(block)
        
        # Update program with optimized blocks
        optimized_program = BitcodeProgram(
            entry_point=program.entry_point,
            blocks=optimized_blocks,
            constants=program.constants,
            metadata={**program.metadata, 'simd_optimized': True, 
                     'optimization_stats': self.optimization_stats}
        )
        
        return optimized_program
    
    def _optimize_block(self, block) -> Any:
        """Optimize a single basic block for SIMD execution"""
        instructions = block.instructions.copy()
        optimized_instructions = []
        i = 0
        
        while i < len(instructions):
            # Look for optimization patterns
            pattern_match = self._find_simd_pattern(instructions, i)
            
            if pattern_match:
                pattern_type, pattern_length, simd_instructions = pattern_match
                
                # Replace scalar instructions with SIMD equivalent
                optimized_instructions.extend(simd_instructions)
                
                # Update statistics
                self.optimization_stats['scalar_ops_vectorized'] += pattern_length
                self.optimization_stats['instructions_eliminated'] += pattern_length - len(simd_instructions)
                
                if pattern_type.value not in self.optimization_stats['simd_patterns_applied']:
                    self.optimization_stats['simd_patterns_applied'][pattern_type.value] = 0
                self.optimization_stats['simd_patterns_applied'][pattern_type.value] += 1
                
                i += pattern_length
            else:
                # No optimization possible, keep original instruction
                optimized_instructions.append(instructions[i])
                i += 1
        
        # Create optimized block
        optimized_block = type(block)(
            label=block.label,
            instructions=optimized_instructions,
            next_blocks=block.next_blocks.copy()
        )
        
        return optimized_block
    
    def _find_simd_pattern(self, instructions: List[BitInstruction], 
                          start_idx: int) -> Optional[Tuple[SIMDPattern, int, List[BitInstruction]]]:
        """Find SIMD optimization patterns in instruction sequence"""
        
        # Pattern 1: Parallel comparisons
        pattern = self._match_parallel_cmp(instructions, start_idx)
        if pattern:
            return pattern
        
        # Pattern 2: Batch range checking
        pattern = self._match_batch_range(instructions, start_idx)
        if pattern:
            return pattern
        
        # Pattern 3: Vector logic operations
        pattern = self._match_vector_logic(instructions, start_idx)
        if pattern:
            return pattern
        
        # Pattern 4: Reduction operations
        pattern = self._match_reduction(instructions, start_idx)
        if pattern:
            return pattern
        
        return None
    
    def _match_parallel_cmp(self, instructions: List[BitInstruction], 
                           start_idx: int) -> Optional[Tuple[SIMDPattern, int, List[BitInstruction]]]:
        """Match pattern: multiple CMP instructions with same operation"""
        if start_idx + 3 >= len(instructions):
            return None
        
        # Look for sequence of CMP instructions
        cmp_sequence = []
        i = start_idx
        
        while (i < len(instructions) and 
               instructions[i].opcode == Opcode.CMP and
               len(cmp_sequence) < 8):  # Max 8 lanes in AVX2
            cmp_sequence.append(instructions[i])
            i += 1
        
        if len(cmp_sequence) >= 2:
            # Generate SIMD equivalent
            vreg = self._alloc_vector_register()
            result_reg = cmp_sequence[0].dst
            
            # Pack comparison operands
            operand_regs = [instr.src1 for instr in cmp_sequence]
            compare_values = [instr.src2 for instr in cmp_sequence]
            
            simd_instructions = [
                # Pack source operands into vector
                BitInstruction(Opcode.VPACK, vreg, operand_regs[0], 
                              self._encode_reg_list(operand_regs)),
                
                # Pack comparison values
                BitInstruction(Opcode.VPACK, vreg + 1, compare_values[0],
                              self._encode_reg_list(compare_values)),
                
                # Vector compare
                BitInstruction(Opcode.VCMP, vreg + 2, vreg, vreg + 1),
                
                # Unpack results
                BitInstruction(Opcode.VUNPACK, result_reg, vreg + 2, 
                              self._encode_reg_list([instr.dst for instr in cmp_sequence])),
            ]
            
            self._free_vector_register(vreg)
            self._free_vector_register(vreg + 1)
            self._free_vector_register(vreg + 2)
            
            return (SIMDPattern.PARALLEL_CMP, len(cmp_sequence), simd_instructions)
        
        return None
    
    def _match_batch_range(self, instructions: List[BitInstruction], 
                          start_idx: int) -> Optional[Tuple[SIMDPattern, int, List[BitInstruction]]]:
        """Match pattern: range checking (min + max operations)"""
        if start_idx + 5 >= len(instructions):
            return None
        
        # Look for MIN followed by MAX pattern
        if (instructions[start_idx].opcode == Opcode.MIN and
            start_idx + 1 < len(instructions) and
            instructions[start_idx + 1].opcode == Opcode.MAX):
            
            min_instr = instructions[start_idx]
            max_instr = instructions[start_idx + 1]
            
            # Check if they operate on same register
            if min_instr.dst == max_instr.src1:
                vreg = self._alloc_vector_register()
                
                simd_instructions = [
                    # Load range bounds into vector register
                    BitInstruction(Opcode.VLOAD, vreg, min_instr.src2, max_instr.src2),
                    
                    # Parallel range check
                    BitInstruction(Opcode.VRANGE, min_instr.dst, min_instr.src1, vreg),
                ]
                
                self._free_vector_register(vreg)
                
                return (SIMDPattern.BATCH_RANGE, 2, simd_instructions)
        
        return None
    
    def _match_vector_logic(self, instructions: List[BitInstruction], 
                           start_idx: int) -> Optional[Tuple[SIMDPattern, int, List[BitInstruction]]]:
        """Match pattern: logic operations (AND, OR, XOR) on multiple operands"""
        if start_idx + 3 >= len(instructions):
            return None
        
        # Look for sequence of same logic operation
        logic_ops = [Opcode.AND, Opcode.OR, Opcode.XOR]
        current_op = instructions[start_idx].opcode
        
        if current_op not in logic_ops:
            return None
        
        logic_sequence = []
        i = start_idx
        
        while (i < len(instructions) and 
               instructions[i].opcode == current_op and
               len(logic_sequence) < 8):
            logic_sequence.append(instructions[i])
            i += 1
        
        if len(logic_sequence) >= 2:
            vreg1 = self._alloc_vector_register()
            vreg2 = self._alloc_vector_register()
            vreg_result = self._alloc_vector_register()
            
            # Map scalar opcode to vector opcode
            vector_opcode_map = {
                Opcode.AND: Opcode.VAND,
                Opcode.OR: Opcode.VOR,
                Opcode.XOR: Opcode.VXOR
            }
            
            src1_regs = [instr.src1 for instr in logic_sequence]
            src2_regs = [instr.src2 for instr in logic_sequence]
            dst_regs = [instr.dst for instr in logic_sequence]
            
            simd_instructions = [
                # Pack operands
                BitInstruction(Opcode.VPACK, vreg1, src1_regs[0], 
                              self._encode_reg_list(src1_regs)),
                BitInstruction(Opcode.VPACK, vreg2, src2_regs[0],
                              self._encode_reg_list(src2_regs)),
                
                # Vector operation
                BitInstruction(vector_opcode_map[current_op], vreg_result, vreg1, vreg2),
                
                # Unpack results
                BitInstruction(Opcode.VUNPACK, dst_regs[0], vreg_result,
                              self._encode_reg_list(dst_regs)),
            ]
            
            self._free_vector_register(vreg1)
            self._free_vector_register(vreg2)
            self._free_vector_register(vreg_result)
            
            return (SIMDPattern.VECTOR_LOGIC, len(logic_sequence), simd_instructions)
        
        return None
    
    def _match_reduction(self, instructions: List[BitInstruction], 
                        start_idx: int) -> Optional[Tuple[SIMDPattern, int, List[BitInstruction]]]:
        """Match pattern: reduction operations (horizontal operations)"""
        if start_idx + 4 >= len(instructions):
            return None
        
        # Look for accumulation pattern: ADD r1, r1, r2; ADD r1, r1, r3; etc.
        if instructions[start_idx].opcode != Opcode.ADD:
            return None
        
        first_instr = instructions[start_idx]
        if first_instr.dst != first_instr.src1:  # Not accumulating
            return None
        
        accumulation_sequence = [first_instr]
        accumulator_reg = first_instr.dst
        i = start_idx + 1
        
        while (i < len(instructions) and
               instructions[i].opcode == Opcode.ADD and
               instructions[i].dst == accumulator_reg and
               instructions[i].src1 == accumulator_reg and
               len(accumulation_sequence) < 8):
            accumulation_sequence.append(instructions[i])
            i += 1
        
        if len(accumulation_sequence) >= 3:  # Worth vectorizing
            vreg = self._alloc_vector_register()
            
            # Get all the values being accumulated
            values_to_sum = [first_instr.src2] + [instr.src2 for instr in accumulation_sequence[1:]]
            
            simd_instructions = [
                # Pack values into vector
                BitInstruction(Opcode.VPACK, vreg, values_to_sum[0],
                              self._encode_reg_list(values_to_sum)),
                
                # Horizontal sum (reduction)
                BitInstruction(Opcode.VADD, vreg, vreg, 0),  # Special encoding for horizontal add
                
                # Extract result to accumulator
                BitInstruction(Opcode.VUNPACK, accumulator_reg, vreg, 0),
            ]
            
            self._free_vector_register(vreg)
            
            return (SIMDPattern.REDUCTION, len(accumulation_sequence), simd_instructions)
        
        return None
    
    def _alloc_vector_register(self) -> int:
        """Allocate a vector register"""
        if self.current_vreg >= len(self.vector_regs):
            # Fallback - reuse registers (not optimal but prevents overflow)
            return self.vector_regs[0]
        
        reg = self.vector_regs[self.current_vreg]
        self.current_vreg += 1
        return reg
    
    def _free_vector_register(self, reg: int):
        """Free a vector register"""
        if reg in self.vector_regs and self.current_vreg > 0:
            self.current_vreg -= 1
    
    def _encode_reg_list(self, regs: List[int]) -> int:
        """Encode register list into single immediate value"""
        # Simplified encoding - in practice would use more sophisticated packing
        encoded = 0
        for i, reg in enumerate(regs[:4]):  # Max 4 registers in immediate
            encoded |= (reg & 0xFF) << (i * 8)
        return encoded
    
    def estimate_speedup(self, original_program: BitcodeProgram, 
                        optimized_program: BitcodeProgram) -> float:
        """Estimate performance speedup from SIMD optimization"""
        original_ticks = self._count_ticks(original_program)
        optimized_ticks = self._count_ticks(optimized_program, use_simd_costs=True)
        
        if optimized_ticks == 0:
            return 1.0
        
        speedup = original_ticks / optimized_ticks
        self.optimization_stats['estimated_speedup'] = speedup
        
        return speedup
    
    def _count_ticks(self, program: BitcodeProgram, use_simd_costs: bool = False) -> int:
        """Count total execution ticks for program"""
        total_ticks = 0
        
        for block in program.blocks.values():
            for instr in block.instructions:
                if use_simd_costs and instr.opcode in self.simd_tick_costs:
                    total_ticks += self.simd_tick_costs[instr.opcode]
                else:
                    # Default scalar costs
                    if instr.opcode == Opcode.HASH:
                        total_ticks += 2
                    else:
                        total_ticks += 1
        
        return total_ticks
    
    def generate_optimization_report(self) -> str:
        """Generate human-readable optimization report"""
        lines = [
            "SIMD Optimization Report",
            "=" * 40,
            f"Scalar operations vectorized: {self.optimization_stats['scalar_ops_vectorized']}",
            f"Instructions eliminated: {self.optimization_stats['instructions_eliminated']}",
            f"Estimated speedup: {self.optimization_stats['estimated_speedup']:.2f}x",
            "",
            "SIMD patterns applied:"
        ]
        
        for pattern, count in self.optimization_stats['simd_patterns_applied'].items():
            lines.append(f"  {pattern}: {count} times")
        
        return "\n".join(lines)

class TickBudgetAnalyzer:
    """Analyzes and optimizes for 8-tick execution budget"""
    
    def __init__(self):
        self.max_ticks = 8
        self.critical_path_ticks = 0
        self.optimization_opportunities = []
    
    def analyze_program(self, program: BitcodeProgram) -> Dict[str, Any]:
        """Analyze program for tick budget compliance"""
        analysis = {
            'compliant': True,
            'critical_path_ticks': 0,
            'hotspots': [],
            'optimization_suggestions': []
        }
        
        # Analyze each block
        for label, block in program.blocks.items():
            block_ticks = self._analyze_block(block)
            
            if block_ticks > self.max_ticks:
                analysis['compliant'] = False
                analysis['hotspots'].append({
                    'block': label,
                    'ticks': block_ticks,
                    'excess': block_ticks - self.max_ticks
                })
        
        # Find critical path
        analysis['critical_path_ticks'] = self._find_critical_path(program)
        
        # Generate optimization suggestions
        if not analysis['compliant']:
            analysis['optimization_suggestions'] = self._generate_optimization_suggestions(
                analysis['hotspots']
            )
        
        return analysis
    
    def _analyze_block(self, block) -> int:
        """Analyze single block for tick count"""
        total_ticks = 0
        
        for instr in block.instructions:
            if instr.opcode == Opcode.HASH:
                total_ticks += 2
            elif instr.opcode in [Opcode.VRANGE, Opcode.VCOUNT]:
                total_ticks += 2
            elif instr.opcode == Opcode.VPATTERN:
                total_ticks += 3
            else:
                total_ticks += 1
        
        return total_ticks
    
    def _find_critical_path(self, program: BitcodeProgram) -> int:
        """Find longest execution path through program"""
        # Simplified critical path analysis
        max_path_ticks = 0
        
        for block in program.blocks.values():
            block_ticks = self._analyze_block(block)
            max_path_ticks = max(max_path_ticks, block_ticks)
        
        return max_path_ticks
    
    def _generate_optimization_suggestions(self, hotspots: List[Dict]) -> List[str]:
        """Generate suggestions for reducing tick count"""
        suggestions = []
        
        for hotspot in hotspots:
            excess_ticks = hotspot['excess']
            
            if excess_ticks <= 2:
                suggestions.append(f"Block '{hotspot['block']}': Replace HASH with simpler operations")
            elif excess_ticks <= 4:
                suggestions.append(f"Block '{hotspot['block']}': Use SIMD vectorization to parallelize operations")
            else:
                suggestions.append(f"Block '{hotspot['block']}': Split into multiple handlers or simplify constraints")
        
        return suggestions