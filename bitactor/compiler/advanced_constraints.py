"""
Advanced SHACL Constraint Compilation Engine
Compiles complex SHACL constraints to optimized bytecode with SIMD support
"""
import os
from typing import Dict, List, Optional, Tuple, Any, Set
from enum import Enum
import rdflib
from rdflib import Graph, URIRef, Literal, BNode
from rdflib.namespace import RDF, RDFS, OWL, SH, XSD

from .ir import BitcodeIR, Opcode, BitcodeProgram

class ConstraintType(Enum):
    """SHACL constraint categories for compilation strategy"""
    VALUE_RANGE = "value_range"      # min/max inclusive/exclusive  
    PATTERN = "pattern"              # regex pattern matching
    DATATYPE = "datatype"           # type validation
    CLASS = "class"                 # class membership
    PROPERTY_PATH = "property_path"  # property path constraints
    LOGIC = "logic"                 # and/or/not combinations
    QUANTIFIED = "quantified"       # min/max/exactly count
    UNIQUE = "unique"               # uniqueness constraints

class SIMDOperation(Enum):
    """SIMD-optimized operations for parallel constraint checking"""
    VECTOR_CMP = "vcmp"         # Parallel comparison
    VECTOR_RANGE = "vrange"     # Parallel range check
    VECTOR_PATTERN = "vpattern" # Parallel pattern match
    VECTOR_MASK = "vmask"       # Parallel masking operation

class AdvancedConstraintCompiler:
    """Advanced SHACL constraint compilation with SIMD optimization"""
    
    def __init__(self, ir: BitcodeIR):
        self.ir = ir
        self.constraint_cache: Dict[str, Tuple[ConstraintType, List]] = {}
        self.simd_register_pool = list(range(8, 16))  # r8-r15 for SIMD
        self.current_simd_reg = 0
        self.tick_costs = {
            # Base instruction costs (CPU ticks)
            Opcode.LOAD: 1,
            Opcode.CMP: 1, 
            Opcode.AND: 1,
            Opcode.OR: 1,
            Opcode.XOR: 1,
            Opcode.MIN: 1,
            Opcode.MAX: 1,
            Opcode.HASH: 2,
            # SIMD costs (parallel operations)
            SIMDOperation.VECTOR_CMP: 1,      # AVX2 parallel compare
            SIMDOperation.VECTOR_RANGE: 2,    # Two compares + AND
            SIMDOperation.VECTOR_PATTERN: 3,  # Pattern matching overhead
            SIMDOperation.VECTOR_MASK: 1      # Masking operation
        }
        
    def compile_constraint(self, constraint_node: Any, graph: Graph, 
                          target_reg: int, scratch_reg: int) -> Tuple[int, List[str]]:
        """
        Compile a SHACL constraint to optimized bytecode
        Returns: (tick_count, generated_instructions)
        """
        constraint_type = self._classify_constraint(constraint_node, graph)
        
        # Check cache first
        cache_key = str(constraint_node) + str(constraint_type)
        if cache_key in self.constraint_cache:
            cached_type, cached_instructions = self.constraint_cache[cache_key]
            return self._replay_cached_constraint(cached_instructions, target_reg)
        
        # Generate optimized bytecode based on constraint type
        tick_count = 0
        instructions = []
        
        if constraint_type == ConstraintType.VALUE_RANGE:
            tick_count, instructions = self._compile_value_range(
                constraint_node, graph, target_reg, scratch_reg
            )
        elif constraint_type == ConstraintType.PATTERN:
            tick_count, instructions = self._compile_pattern_constraint(
                constraint_node, graph, target_reg, scratch_reg
            )
        elif constraint_type == ConstraintType.PROPERTY_PATH:
            tick_count, instructions = self._compile_property_path(
                constraint_node, graph, target_reg, scratch_reg
            )
        elif constraint_type == ConstraintType.LOGIC:
            tick_count, instructions = self._compile_logic_constraint(
                constraint_node, graph, target_reg, scratch_reg
            )
        elif constraint_type == ConstraintType.QUANTIFIED:
            tick_count, instructions = self._compile_quantified_constraint(
                constraint_node, graph, target_reg, scratch_reg
            )
        else:
            # Fallback to basic constraint
            tick_count, instructions = self._compile_basic_constraint(
                constraint_node, graph, target_reg, scratch_reg
            )
        
        # Cache for reuse
        self.constraint_cache[cache_key] = (constraint_type, instructions)
        
        return tick_count, instructions
    
    def _compile_value_range(self, constraint_node: Any, graph: Graph,
                            target_reg: int, scratch_reg: int) -> Tuple[int, List[str]]:
        """Compile value range constraints with SIMD optimization"""
        instructions = []
        tick_count = 0
        
        # Extract range bounds
        min_val = None
        max_val = None
        min_exclusive = False
        max_exclusive = False
        
        for p, o in graph.predicate_objects(constraint_node):
            if p == SH.minInclusive:
                min_val = self._literal_to_int(o)
            elif p == SH.maxInclusive:
                max_val = self._literal_to_int(o)
            elif p == SH.minExclusive:
                min_val = self._literal_to_int(o)
                min_exclusive = True
            elif p == SH.maxExclusive:
                max_val = self._literal_to_int(o)
                max_exclusive = True
        
        if min_val is not None and max_val is not None:
            # SIMD-optimized range check: parallel min/max operations
            simd_reg = self._alloc_simd_register()
            
            # Load constants into SIMD register
            min_const = self.ir.add_constant(min_val)
            max_const = self.ir.add_constant(max_val)
            
            instructions.extend([
                f"VLOAD {simd_reg}, {min_const}, {max_const}",  # Load min/max bounds
                f"VRANGE {target_reg}, {target_reg}, {simd_reg}",  # Parallel range check
            ])
            
            # Handle exclusive bounds with conditional adjustment
            if min_exclusive or max_exclusive:
                adj_reg = scratch_reg
                adj_mask = (1 if min_exclusive else 0) | (2 if max_exclusive else 0)
                adj_const = self.ir.add_constant(adj_mask)
                
                instructions.extend([
                    f"LOAD {adj_reg}, 0, {adj_const}",
                    f"VEXCL {target_reg}, {target_reg}, {adj_reg}",  # Exclusive adjustment
                ])
                tick_count += 1
            
            tick_count += self.tick_costs[SIMDOperation.VECTOR_RANGE]
            self._free_simd_register(simd_reg)
            
        elif min_val is not None:
            # Single bound check
            const_id = self.ir.add_constant(min_val)
            instructions.extend([
                f"LOAD {scratch_reg}, 0, {const_id}",
                f"MIN {target_reg}, {target_reg}, {scratch_reg}" if not min_exclusive 
                else f"CMP {target_reg}, {target_reg}, {scratch_reg}",
            ])
            tick_count += 2
            
        elif max_val is not None:
            # Single bound check
            const_id = self.ir.add_constant(max_val)
            instructions.extend([
                f"LOAD {scratch_reg}, 0, {const_id}",
                f"MAX {target_reg}, {target_reg}, {scratch_reg}" if not max_exclusive
                else f"CMP {target_reg}, {target_reg}, {scratch_reg}",
            ])
            tick_count += 2
        
        return tick_count, instructions
    
    def _compile_pattern_constraint(self, constraint_node: Any, graph: Graph,
                                   target_reg: int, scratch_reg: int) -> Tuple[int, List[str]]:
        """Compile regex pattern constraints with SIMD optimization"""
        instructions = []
        tick_count = 0
        
        pattern = None
        flags = 0
        
        for p, o in graph.predicate_objects(constraint_node):
            if p == SH.pattern:
                pattern = str(o)
            elif p == SH.flags:
                flags = self._parse_regex_flags(str(o))
        
        if pattern:
            # Convert regex to finite automaton for SIMD execution
            pattern_hash = self._pattern_to_hash(pattern)
            pattern_const = self.ir.add_constant(pattern_hash)
            
            # SIMD pattern matching using parallel state transitions
            simd_reg = self._alloc_simd_register()
            
            instructions.extend([
                f"LOAD {scratch_reg}, 0, {pattern_const}",
                f"VPATTERN {simd_reg}, {target_reg}, {scratch_reg}",  # SIMD pattern match
                f"VMASK {target_reg}, {simd_reg}, 0xFF",  # Extract result mask
            ])
            
            tick_count += self.tick_costs[SIMDOperation.VECTOR_PATTERN]
            self._free_simd_register(simd_reg)
        
        return tick_count, instructions
    
    def _compile_property_path(self, constraint_node: Any, graph: Graph,
                              target_reg: int, scratch_reg: int) -> Tuple[int, List[str]]:
        """Compile SHACL property path constraints"""
        instructions = []
        tick_count = 0
        
        # Parse property path expression
        path_expr = None
        for p, o in graph.predicate_objects(constraint_node):
            if p == SH.path:
                path_expr = o
                break
        
        if path_expr:
            # Analyze path complexity and generate optimized traversal
            path_ops = self._parse_property_path(path_expr, graph)
            
            # Generate path traversal bytecode
            for op_type, op_data in path_ops:
                if op_type == "direct":
                    # Direct property access
                    prop_hash = hash(str(op_data)) & 0xFFFFFFFF
                    prop_const = self.ir.add_constant(prop_hash)
                    
                    instructions.extend([
                        f"LOAD {scratch_reg}, 0, {prop_const}",
                        f"HASH {target_reg}, {target_reg}, {scratch_reg}",  # Property lookup
                    ])
                    tick_count += 2
                    
                elif op_type == "sequence":
                    # Path sequence: p1/p2/p3
                    for prop in op_data:
                        prop_hash = hash(str(prop)) & 0xFFFFFFFF
                        prop_const = self.ir.add_constant(prop_hash)
                        
                        instructions.extend([
                            f"LOAD {scratch_reg}, 0, {prop_const}",
                            f"HASH {target_reg}, {target_reg}, {scratch_reg}",
                        ])
                        tick_count += 2
                        
                elif op_type == "alternative":
                    # Path alternative: p1|p2|p3 - use SIMD parallel checking
                    simd_reg = self._alloc_simd_register()
                    alt_hashes = [hash(str(prop)) & 0xFFFFFFFF for prop in op_data[:4]]
                    
                    # Pack alternatives into SIMD register
                    alt_const = self.ir.add_constant_array(alt_hashes)
                    
                    instructions.extend([
                        f"VLOAD {simd_reg}, 0, {alt_const}",
                        f"VCMP {target_reg}, {target_reg}, {simd_reg}",  # Parallel compare
                        f"VMASK {target_reg}, {target_reg}, 0x0F",  # OR results
                    ])
                    tick_count += self.tick_costs[SIMDOperation.VECTOR_CMP] + 1
                    self._free_simd_register(simd_reg)
        
        return tick_count, instructions
    
    def _compile_logic_constraint(self, constraint_node: Any, graph: Graph,
                                 target_reg: int, scratch_reg: int) -> Tuple[int, List[str]]:
        """Compile logical constraints (sh:and, sh:or, sh:not)"""
        instructions = []
        tick_count = 0
        
        logic_op = None
        operands = []
        
        for p, o in graph.predicate_objects(constraint_node):
            if p == SH['and']:
                logic_op = "AND"
                operands = list(graph.objects(o, RDF.rest))
            elif p == SH['or']:
                logic_op = "OR" 
                operands = list(graph.objects(o, RDF.rest))
            elif p == SH['not']:
                logic_op = "NOT"
                operands = [o]
        
        if logic_op and operands:
            if len(operands) <= 4 and logic_op in ["AND", "OR"]:
                # SIMD-optimized parallel logic operations
                simd_reg = self._alloc_simd_register()
                result_regs = []
                
                # Compile each operand constraint
                for i, operand in enumerate(operands[:4]):
                    operand_reg = self.ir.alloc_reg()
                    result_regs.append(operand_reg)
                    
                    # Recursively compile operand constraint
                    operand_ticks, operand_instr = self.compile_constraint(
                        operand, graph, operand_reg, scratch_reg
                    )
                    instructions.extend(operand_instr)
                    tick_count += operand_ticks
                
                # Pack results into SIMD register and perform parallel operation
                pack_const = self.ir.add_constant_array([0] * 4)  # Packing template
                
                instructions.extend([
                    f"VPACK {simd_reg}, {','.join(map(str, result_regs))}, {pack_const}",
                    f"V{logic_op} {target_reg}, {simd_reg}, 0",  # Parallel AND/OR
                ])
                tick_count += 2
                
                # Free temporary registers
                for reg in result_regs:
                    self.ir.free_reg(reg)
                self._free_simd_register(simd_reg)
                
            else:
                # Sequential logic for complex cases
                for i, operand in enumerate(operands):
                    operand_reg = scratch_reg if i == 0 else target_reg
                    
                    operand_ticks, operand_instr = self.compile_constraint(
                        operand, graph, operand_reg, scratch_reg
                    )
                    instructions.extend(operand_instr)
                    tick_count += operand_ticks
                    
                    if i > 0:
                        instructions.append(f"{logic_op} {target_reg}, {target_reg}, {operand_reg}")
                        tick_count += 1
                    elif i == 0 and operand_reg != target_reg:
                        instructions.append(f"MOV {target_reg}, {operand_reg}")
                        tick_count += 1
        
        return tick_count, instructions
    
    def _compile_quantified_constraint(self, constraint_node: Any, graph: Graph,
                                      target_reg: int, scratch_reg: int) -> Tuple[int, List[str]]:
        """Compile quantified constraints (sh:minCount, sh:maxCount, sh:qualifiedValueShape)"""
        instructions = []
        tick_count = 0
        
        min_count = None
        max_count = None
        qualified_shape = None
        
        for p, o in graph.predicate_objects(constraint_node):
            if p == SH.minCount:
                min_count = int(o)
            elif p == SH.maxCount:
                max_count = int(o)
            elif p == SH.qualifiedValueShape:
                qualified_shape = o
        
        if min_count is not None or max_count is not None:
            # Count-based validation with SIMD acceleration
            count_reg = self.ir.alloc_reg()
            
            # Initialize counter
            instructions.extend([
                f"LOAD {count_reg}, 0, 0",  # counter = 0
            ])
            tick_count += 1
            
            if qualified_shape:
                # Count items matching qualified shape
                # This would involve iterating over collection and applying shape
                # For now, simplified to direct counting
                shape_hash = hash(str(qualified_shape)) & 0xFFFFFFFF
                shape_const = self.ir.add_constant(shape_hash)
                
                instructions.extend([
                    f"LOAD {scratch_reg}, 0, {shape_const}",
                    f"COUNT {count_reg}, {target_reg}, {scratch_reg}",  # Count matching items
                ])
                tick_count += 2
            else:
                # Simple count
                instructions.extend([
                    f"COUNT {count_reg}, {target_reg}, 0",  # Count all items
                ])
                tick_count += 1
            
            # Validate count constraints
            if min_count is not None and max_count is not None:
                # Range check on count
                min_const = self.ir.add_constant(min_count)
                max_const = self.ir.add_constant(max_count)
                
                instructions.extend([
                    f"LOAD {scratch_reg}, 0, {min_const}",
                    f"MIN {count_reg}, {count_reg}, {scratch_reg}",  # count >= min
                    f"LOAD {scratch_reg}, 0, {max_const}",
                    f"MAX {target_reg}, {count_reg}, {scratch_reg}",  # result = count <= max
                ])
                tick_count += 4
                
            elif min_count is not None:
                min_const = self.ir.add_constant(min_count)
                instructions.extend([
                    f"LOAD {scratch_reg}, 0, {min_const}",
                    f"CMP {target_reg}, {count_reg}, {scratch_reg}",  # count >= min
                ])
                tick_count += 2
                
            elif max_count is not None:
                max_const = self.ir.add_constant(max_count)
                instructions.extend([
                    f"LOAD {scratch_reg}, 0, {max_const}",
                    f"CMP {target_reg}, {count_reg}, {scratch_reg}",  # count <= max
                ])
                tick_count += 2
            
            self.ir.free_reg(count_reg)
        
        return tick_count, instructions
    
    def _compile_basic_constraint(self, constraint_node: Any, graph: Graph,
                                 target_reg: int, scratch_reg: int) -> Tuple[int, List[str]]:
        """Fallback for basic constraint compilation"""
        instructions = []
        tick_count = 0
        
        # Simple hash-based constraint check
        constraint_hash = hash(str(constraint_node)) & 0xFFFFFFFF
        const_id = self.ir.add_constant(constraint_hash)
        
        instructions.extend([
            f"LOAD {scratch_reg}, 0, {const_id}",
            f"HASH {target_reg}, {target_reg}, {scratch_reg}",
        ])
        tick_count += 2
        
        return tick_count, instructions
    
    def _classify_constraint(self, constraint_node: Any, graph: Graph) -> ConstraintType:
        """Classify SHACL constraint for optimal compilation strategy"""
        # Check for specific constraint predicates
        predicates = set(graph.predicates(constraint_node))
        
        if any(p in predicates for p in [SH.minInclusive, SH.maxInclusive, SH.minExclusive, SH.maxExclusive]):
            return ConstraintType.VALUE_RANGE
        elif SH.pattern in predicates:
            return ConstraintType.PATTERN
        elif SH.datatype in predicates or SH['class'] in predicates:
            return ConstraintType.DATATYPE
        elif SH.path in predicates:
            return ConstraintType.PROPERTY_PATH
        elif any(p in predicates for p in [SH['and'], SH['or'], SH['not']]):
            return ConstraintType.LOGIC
        elif any(p in predicates for p in [SH.minCount, SH.maxCount, SH.qualifiedValueShape]):
            return ConstraintType.QUANTIFIED
        else:
            return ConstraintType.VALUE_RANGE  # Default fallback
    
    def _alloc_simd_register(self) -> int:
        """Allocate SIMD register for vector operations"""
        if self.current_simd_reg >= len(self.simd_register_pool):
            # Fallback to scalar operations if SIMD registers exhausted
            return self.ir.alloc_reg()
        
        reg = self.simd_register_pool[self.current_simd_reg]
        self.current_simd_reg += 1
        return reg
    
    def _free_simd_register(self, reg: int):
        """Free SIMD register"""
        if reg in self.simd_register_pool:
            # Find and remove from allocation
            if self.current_simd_reg > 0:
                self.current_simd_reg -= 1
    
    def _pattern_to_hash(self, pattern: str) -> int:
        """Convert regex pattern to deterministic hash for matching"""
        # Simplified pattern hashing - in reality would compile to DFA
        return hash(pattern) & 0xFFFFFFFF
    
    def _parse_regex_flags(self, flags: str) -> int:
        """Parse regex flags to bit mask"""
        flag_map = {'i': 1, 'm': 2, 's': 4, 'x': 8}
        result = 0
        for char in flags.lower():
            if char in flag_map:
                result |= flag_map[char]
        return result
    
    def _parse_property_path(self, path_expr: Any, graph: Graph) -> List[Tuple[str, Any]]:
        """Parse SHACL property path expression"""
        # Simplified property path parsing
        # In reality would handle complex path expressions with operators
        if isinstance(path_expr, URIRef):
            return [("direct", path_expr)]
        
        # Handle list-based paths
        path_items = list(graph.objects(path_expr, RDF.rest))
        if path_items:
            return [("sequence", path_items)]
        
        return [("direct", path_expr)]
    
    def _literal_to_int(self, literal: Any) -> int:
        """Convert RDF literal to integer value"""
        if isinstance(literal, Literal):
            try:
                return int(literal)
            except ValueError:
                return hash(str(literal)) & 0xFFFFFFFF
        elif isinstance(literal, (int, float)):
            return int(literal)
        else:
            return hash(str(literal)) & 0xFFFFFFFF
    
    def _replay_cached_constraint(self, cached_instructions: List[str], 
                                 target_reg: int) -> Tuple[int, List[str]]:
        """Replay cached constraint with register renaming"""
        # Simplified cache replay - in reality would do register renaming
        return len(cached_instructions), cached_instructions
    
    def estimate_total_ticks(self, constraint_graph: Graph) -> int:
        """Estimate total tick count for constraint graph"""
        total_ticks = 0
        
        # Analyze all constraints in graph
        for constraint in graph.subjects(RDF.type, SH.PropertyShape):
            constraint_type = self._classify_constraint(constraint, graph)
            
            # Estimate ticks based on constraint complexity
            if constraint_type == ConstraintType.VALUE_RANGE:
                total_ticks += 2  # SIMD range check
            elif constraint_type == ConstraintType.PATTERN:
                total_ticks += 3  # Pattern matching
            elif constraint_type == ConstraintType.PROPERTY_PATH:
                total_ticks += 4  # Path traversal
            elif constraint_type == ConstraintType.LOGIC:
                total_ticks += 2  # Parallel logic
            elif constraint_type == ConstraintType.QUANTIFIED:
                total_ticks += 5  # Counting operations
            else:
                total_ticks += 2  # Basic constraint
        
        return total_ticks