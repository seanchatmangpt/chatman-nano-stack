#!/usr/bin/env python3
"""
BitActor Type Definitions - 80/20 Implementation
Complete Python type system for BitActor with TTL constraints
"""

from typing import TypedDict, Optional, List, Dict, Union, Literal, Any
from pydantic import BaseModel, Field, validator, conint, confloat
from datetime import datetime
from enum import Enum
import uuid


# TTL Constraint Types
class TTLPrecision(str, Enum):
    """Time precision levels for TTL constraints"""
    NANOSECOND = "nanosecond"
    MICROSECOND = "microsecond"
    MILLISECOND = "millisecond"
    SECOND = "second"


class TTLConstraint(BaseModel):
    """TTL (Time-To-Live) constraint with nanosecond precision"""
    budget_ns: conint(gt=0) = Field(..., description="TTL budget in nanoseconds")
    precision: TTLPrecision = Field(default=TTLPrecision.NANOSECOND)
    max_budget_ms: conint(gt=0) = Field(default=8, description="Maximum budget in milliseconds")
    
    @validator('budget_ns')
    def validate_budget(cls, v, values):
        """Ensure budget doesn't exceed max_budget_ms when converted"""
        max_ns = values.get('max_budget_ms', 8) * 1_000_000
        if v > max_ns:
            raise ValueError(f"TTL budget {v}ns exceeds maximum {max_ns}ns")
        return v
    
    @property
    def budget_ms(self) -> float:
        """Get budget in milliseconds"""
        return self.budget_ns / 1_000_000
    
    @property
    def budget_us(self) -> float:
        """Get budget in microseconds"""
        return self.budget_ns / 1_000


# Signal Types
class SignalType(str, Enum):
    """Types of signals that can be processed"""
    DATA = "data"
    CONTROL = "control"
    TELEMETRY = "telemetry"
    HEARTBEAT = "heartbeat"
    ERROR = "error"


class SignalPriority(int, Enum):
    """Signal processing priority levels"""
    LOW = 1
    MEDIUM = 2
    HIGH = 3
    CRITICAL = 4
    EMERGENCY = 5


class Signal(BaseModel):
    """Signal model for BitActor communication"""
    id: uuid.UUID = Field(default_factory=uuid.uuid4)
    type: SignalType
    payload: Dict[str, Any] = Field(default_factory=dict)
    priority: SignalPriority = Field(default=SignalPriority.MEDIUM)
    created_at: datetime = Field(default_factory=datetime.utcnow)
    source_actor_id: Optional[uuid.UUID] = None
    target_actor_id: Optional[uuid.UUID] = None
    ttl_constraint: Optional[TTLConstraint] = None
    
    class Config:
        json_encoders = {
            uuid.UUID: str,
            datetime: lambda v: v.isoformat()
        }


# BitActor Types
class ActorStatus(str, Enum):
    """BitActor operational status"""
    INACTIVE = "inactive"
    ACTIVE = "active"
    PROCESSING = "processing"
    SUSPENDED = "suspended"
    ERROR = "error"
    TERMINATED = "terminated"


class BitActor(BaseModel):
    """BitActor model with TTL constraints and signal processing"""
    id: uuid.UUID = Field(default_factory=uuid.uuid4)
    name: str = Field(..., min_length=1, max_length=255)
    status: ActorStatus = Field(default=ActorStatus.INACTIVE)
    ttl_budget: int = Field(default=8, ge=1, le=100, description="TTL budget in milliseconds")
    ttl_constraint: TTLConstraint = Field(default_factory=lambda: TTLConstraint(budget_ns=8_000_000))
    
    # Performance tracking
    last_signal_id: Optional[uuid.UUID] = None
    processing_time_ns: Optional[int] = None
    signals_processed: int = Field(default=0)
    signals_failed: int = Field(default=0)
    
    # Timestamps
    created_at: datetime = Field(default_factory=datetime.utcnow)
    updated_at: Optional[datetime] = None
    last_active_at: Optional[datetime] = None
    
    # Relationships
    parent_actor_id: Optional[uuid.UUID] = None
    child_actor_ids: List[uuid.UUID] = Field(default_factory=list)
    
    @validator('ttl_constraint', pre=True, always=True)
    def set_ttl_constraint(cls, v, values):
        """Ensure TTL constraint matches ttl_budget"""
        if v is None:
            ttl_budget_ms = values.get('ttl_budget', 8)
            return TTLConstraint(budget_ns=ttl_budget_ms * 1_000_000)
        return v
    
    def can_process_within_ttl(self, estimated_time_ns: int) -> bool:
        """Check if operation can complete within TTL budget"""
        return estimated_time_ns <= self.ttl_constraint.budget_ns
    
    class Config:
        json_encoders = {
            uuid.UUID: str,
            datetime: lambda v: v.isoformat()
        }


# Telemetry Types
class MetricUnit(str, Enum):
    """Units for telemetry metrics"""
    COUNT = "count"
    MILLISECONDS = "ms"
    NANOSECONDS = "ns"
    BYTES = "bytes"
    PERCENTAGE = "percent"
    RATE = "rate"


class TelemetryFrame(BaseModel):
    """Telemetry data frame for monitoring"""
    id: uuid.UUID = Field(default_factory=uuid.uuid4)
    bitactor_id: uuid.UUID
    metric_name: str = Field(..., min_length=1, max_length=100)
    value: confloat(ge=0)
    unit: MetricUnit = Field(default=MetricUnit.COUNT)
    timestamp: datetime = Field(default_factory=datetime.utcnow)
    
    # Additional context
    tags: Dict[str, str] = Field(default_factory=dict)
    metadata: Dict[str, Any] = Field(default_factory=dict)
    
    class Config:
        json_encoders = {
            uuid.UUID: str,
            datetime: lambda v: v.isoformat()
        }


# Inter-service Communication Types
class ServiceMessage(TypedDict):
    """TypedDict for inter-service communication"""
    message_id: str
    service_name: str
    action: str
    payload: Dict[str, Any]
    timestamp: str
    ttl_remaining_ns: Optional[int]


class BitActorCommand(TypedDict):
    """Commands that can be sent to BitActors"""
    command: Literal["process_signal", "suspend", "resume", "terminate", "health_check"]
    actor_id: str
    signal_id: Optional[str]
    ttl_constraint_ns: Optional[int]


class BitActorResponse(TypedDict):
    """Response from BitActor operations"""
    success: bool
    actor_id: str
    command: str
    result: Optional[Dict[str, Any]]
    error: Optional[str]
    execution_time_ns: int
    ttl_remaining_ns: Optional[int]


# Swarm Coordination Types
class SwarmTopology(str, Enum):
    """Swarm network topology types"""
    HIERARCHICAL = "hierarchical"
    MESH = "mesh"
    RING = "ring"
    STAR = "star"


class SwarmConfiguration(BaseModel):
    """Configuration for BitActor swarm deployment"""
    swarm_id: uuid.UUID = Field(default_factory=uuid.uuid4)
    topology: SwarmTopology
    max_actors: conint(ge=1, le=1000) = Field(default=100)
    strategy: Literal["balanced", "specialized", "adaptive"] = "balanced"
    
    # TTL coordination
    global_ttl_budget_ms: conint(gt=0) = Field(default=5000)
    actor_ttl_budget_ms: conint(gt=0) = Field(default=8)
    
    # Scaling policies
    auto_scale: bool = Field(default=True)
    min_actors: conint(ge=1) = Field(default=1)
    target_cpu_percent: conint(ge=1, le=100) = Field(default=70)
    
    # K8s integration
    namespace: str = Field(default="bitactor-system")
    service_name: str = Field(default="bitactor-swarm")
    image: str = Field(default="bitactor:latest")
    replicas: conint(ge=1) = Field(default=3)


# Validation Models
class TTLViolation(BaseModel):
    """Model for TTL constraint violations"""
    actor_id: uuid.UUID
    signal_id: uuid.UUID
    expected_ttl_ns: int
    actual_time_ns: int
    violation_amount_ns: int
    timestamp: datetime = Field(default_factory=datetime.utcnow)
    
    @property
    def violation_percentage(self) -> float:
        """Calculate violation as percentage of expected TTL"""
        return (self.violation_amount_ns / self.expected_ttl_ns) * 100


# Factory Functions
def create_bitactor(name: str, ttl_budget_ms: int = 8, **kwargs) -> BitActor:
    """Factory function to create a BitActor with proper TTL constraints"""
    ttl_constraint = TTLConstraint(budget_ns=ttl_budget_ms * 1_000_000)
    return BitActor(
        name=name,
        ttl_budget=ttl_budget_ms,
        ttl_constraint=ttl_constraint,
        **kwargs
    )


def create_signal(
    signal_type: SignalType,
    payload: Dict[str, Any],
    priority: SignalPriority = SignalPriority.MEDIUM,
    ttl_ms: Optional[int] = None,
    **kwargs
) -> Signal:
    """Factory function to create a Signal with optional TTL constraint"""
    ttl_constraint = None
    if ttl_ms:
        ttl_constraint = TTLConstraint(budget_ns=ttl_ms * 1_000_000)
    
    return Signal(
        type=signal_type,
        payload=payload,
        priority=priority,
        ttl_constraint=ttl_constraint,
        **kwargs
    )


def create_telemetry_frame(
    bitactor_id: uuid.UUID,
    metric_name: str,
    value: float,
    unit: MetricUnit = MetricUnit.COUNT,
    **kwargs
) -> TelemetryFrame:
    """Factory function to create a TelemetryFrame"""
    return TelemetryFrame(
        bitactor_id=bitactor_id,
        metric_name=metric_name,
        value=value,
        unit=unit,
        **kwargs
    )


# Type Aliases for common patterns
ActorID = uuid.UUID
SignalID = uuid.UUID
ProcessingTimeNS = int
TTLBudgetNS = int

# Response type unions
ProcessingResult = Union[Dict[str, Any], None]
ErrorMessage = Optional[str]

# Swarm coordination types
ActorGraph = Dict[ActorID, List[ActorID]]
SignalRoute = List[ActorID]
SwarmMetrics = Dict[str, Union[int, float]]