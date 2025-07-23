"""
BitActor Intermediate Representation (IR)
SSA-style bytecode for deterministic execution
"""
from dataclasses import dataclass
from enum import IntEnum
from typing import List, Optional, Dict, Any
import struct

class Opcode(IntEnum):
    """Bytecode opcodes - fixed size for predictable execution"""
    # Scalar operations (1 tick each)
    NOP = 0x00
    LOAD = 0x01      # Load from memory
    STORE = 0x02     # Store to memory
    ADD = 0x03       # Addition
    SUB = 0x04       # Subtraction
    MUL = 0x05       # Multiplication
    XOR = 0x06       # XOR operation
    AND = 0x07       # AND operation
    OR = 0x08        # OR operation
    SHL = 0x09       # Shift left
    SHR = 0x0A       # Shift right
    CMP = 0x0B       # Compare
    JMP = 0x0C       # Unconditional jump
    CALL = 0x0D      # Call handler
    RET = 0x0E       # Return
    SIGNAL = 0x0F    # Signal operation
    TRACE = 0x10     # Trace operation
    HASH = 0x11      # Hash operation (2 ticks)
    MIN = 0x12       # Branchless min
    MAX = 0x13       # Branchless max
    MOV = 0x14       # Register move
    COUNT = 0x15     # Count operation
    
    # SIMD operations (AVX2 256-bit, 1-3 ticks each)
    VLOAD = 0x20     # Vector load (1 tick)
    VSTORE = 0x21    # Vector store (1 tick)
    VADD = 0x22      # Vector addition (1 tick)
    VSUB = 0x23      # Vector subtraction (1 tick)
    VMUL = 0x24      # Vector multiplication (1 tick)
    VAND = 0x25      # Vector AND (1 tick)
    VOR = 0x26       # Vector OR (1 tick)
    VXOR = 0x27      # Vector XOR (1 tick)
    VCMP = 0x28      # Vector compare (1 tick)
    VMIN = 0x29      # Vector min (1 tick)
    VMAX = 0x2A      # Vector max (1 tick)
    VMASK = 0x2B     # Vector mask operation (1 tick)
    VPACK = 0x2C     # Pack scalars to vector (1 tick)
    VUNPACK = 0x2D   # Unpack vector to scalars (1 tick)
    
    # Specialized SIMD constraint operations (2-3 ticks)
    VRANGE = 0x30    # Parallel range check (2 ticks)
    VPATTERN = 0x31  # Parallel pattern match (3 ticks)
    VEXCL = 0x32     # Exclusive bounds adjustment (1 tick)
    VCOUNT = 0x33    # Vector count operation (2 ticks)
    
    # Advanced operations
    PERFHASH = 0x40  # Perfect hash lookup (1 tick)
    RDTSC = 0x41     # Read timestamp counter (1 tick)
    PREFETCH = 0x42  # Cache prefetch (0 ticks, overlapped)

@dataclass
class BitInstruction:
    """Single bytecode instruction - 4 bytes packed"""
    opcode: Opcode
    dst: int  # Destination register (0-255)
    src1: int # Source register 1 (0-255)
    src2: int # Source register 2 or immediate (0-255)
    
    def pack(self) -> bytes:
        """Pack instruction into 4 bytes"""
        return struct.pack('BBBB', self.opcode, self.dst, self.src1, self.src2)
    
    @classmethod
    def unpack(cls, data: bytes) -> 'BitInstruction':
        """Unpack instruction from bytes"""
        op, dst, src1, src2 = struct.unpack('BBBB', data[:4])
        return cls(Opcode(op), dst, src1, src2)
    
    def __str__(self) -> str:
        return f"{self.opcode.name} r{self.dst}, r{self.src1}, r{self.src2}"

@dataclass
class BitcodeBlock:
    """Basic block of instructions"""
    label: str
    instructions: List[BitInstruction]
    next_blocks: List[str]  # Control flow targets
    
    def size(self) -> int:
        """Size in bytes"""
        return len(self.instructions) * 4

@dataclass
class BitcodeProgram:
    """Complete bytecode program"""
    entry_point: str
    blocks: Dict[str, BitcodeBlock]
    constants: Dict[int, int]  # Constant pool
    metadata: Dict[str, Any]   # Program metadata
    
    def serialize(self) -> bytes:
        """Serialize to binary format"""
        output = bytearray()
        
        # Header: magic number + version
        output.extend(b'BITC')  # Magic
        output.extend(struct.pack('<H', 1))  # Version
        
        # Entry point offset (will update later)
        entry_offset_pos = len(output)
        output.extend(struct.pack('<I', 0))
        
        # Constants pool
        output.extend(struct.pack('<I', len(self.constants)))
        for addr, value in sorted(self.constants.items()):
            output.extend(struct.pack('<II', addr, value))
        
        # Blocks
        block_offsets = {}
        output.extend(struct.pack('<I', len(self.blocks)))
        
        for label, block in self.blocks.items():
            block_offsets[label] = len(output)
            
            # Block header
            output.extend(struct.pack('<I', len(block.instructions)))
            
            # Instructions
            for instr in block.instructions:
                output.extend(instr.pack())
        
        # Update entry point
        if self.entry_point in block_offsets:
            output[entry_offset_pos:entry_offset_pos+4] = struct.pack(
                '<I', block_offsets[self.entry_point]
            )
        
        return bytes(output)

class BitcodeIR:
    """IR builder for BitActor bytecode"""
    
    def __init__(self):
        self.blocks: Dict[str, BitcodeBlock] = {}
        self.current_block: Optional[str] = None
        self.constants: Dict[int, int] = {}
        self.next_reg = 0
        self.metadata = {}
    
    def new_block(self, label: str) -> str:
        """Create a new basic block"""
        self.blocks[label] = BitcodeBlock(label, [], [])
        return label
    
    def set_block(self, label: str):
        """Set current block for instruction emission"""
        if label not in self.blocks:
            self.new_block(label)
        self.current_block = label
    
    def emit(self, opcode: Opcode, dst: int = 0, src1: int = 0, src2: int = 0):
        """Emit instruction to current block"""
        if not self.current_block:
            raise ValueError("No current block set")
        
        instr = BitInstruction(opcode, dst, src1, src2)
        self.blocks[self.current_block].instructions.append(instr)
    
    def alloc_reg(self) -> int:
        """Allocate a new register"""
        reg = self.next_reg
        self.next_reg += 1
        if self.next_reg > 255:
            raise ValueError("Register allocation overflow")
        return reg
    
    def add_constant(self, value: int) -> int:
        """Add constant to pool, return address"""
        addr = len(self.constants)
        self.constants[addr] = value
        return addr
    
    def link_blocks(self, from_label: str, to_label: str):
        """Add control flow edge"""
        if from_label in self.blocks:
            self.blocks[from_label].next_blocks.append(to_label)
    
    def optimize(self):
        """Basic optimizations - dead code elimination, constant folding"""
        # Simple dead code elimination
        reachable = self._find_reachable_blocks()
        self.blocks = {k: v for k, v in self.blocks.items() if k in reachable}
        
        # TODO: Add more optimizations
    
    def _find_reachable_blocks(self, start: str = "entry") -> set:
        """Find all reachable blocks from entry"""
        visited = set()
        stack = [start] if start in self.blocks else []
        
        while stack:
            block = stack.pop()
            if block in visited:
                continue
            visited.add(block)
            stack.extend(self.blocks[block].next_blocks)
        
        return visited
    
    def to_program(self, entry_point: str = "entry") -> BitcodeProgram:
        """Convert to final program"""
        self.optimize()
        return BitcodeProgram(
            entry_point=entry_point,
            blocks=self.blocks,
            constants=self.constants,
            metadata=self.metadata
        )
    
    def dump(self) -> str:
        """Dump IR in human-readable format"""
        lines = ["BitActor IR Dump", "=" * 40]
        
        lines.append(f"\nConstants ({len(self.constants)}):")
        for addr, val in sorted(self.constants.items()):
            lines.append(f"  [{addr}] = 0x{val:08X}")
        
        lines.append(f"\nBlocks ({len(self.blocks)}):")
        for label, block in self.blocks.items():
            lines.append(f"\n{label}:")
            for i, instr in enumerate(block.instructions):
                lines.append(f"  {i:3d}: {instr}")
            if block.next_blocks:
                lines.append(f"  -> {', '.join(block.next_blocks)}")
        
        return '\n'.join(lines)