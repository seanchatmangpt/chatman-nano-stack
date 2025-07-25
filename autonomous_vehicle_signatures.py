"""
DSPy Signatures generated from Turtle ontology
Generated by ttl2dspy.py on 2025-07-24T08:26:19.215798

Ontology URI: 
Signatures generated: 11
"""

import dspy
from typing import Union

# Type aliases for better IDE support
Text = str
Number = Union[int, float]
Boolean = bool

__all__ = ["VehicleSignature", "EmergencyVehicleSignature", "PositionSignature", "LaneSignature", "CollisionRiskSignature", "EmergencyBrakeWarningSignature", "TrajectorySignature", "EmergencyManeuverSignature", "VelocitySignature", "V2VMessageSignature", "BasicSafetyMessageSignature"]

class VehicleSignature(dspy.Signature):
    """DSPy Signature for Vehicle
    
    Generated from: http://cns.ai/ontology/autonomous-vehicle#Vehicle
    Timestamp: 2025-07-24T08:26:19.214305"""
    
    speed = dspy.InputField(desc="Vehicle speed must be between 0 and 200 km/h", dtype=float)
    vehicle_id = dspy.InputField(desc="Vehicle ID must be 6-32 alphanumeric characters", dtype=str)
    has_velocity = dspy.InputField(desc="Every vehicle must have exactly one velocity", dtype=str)
    has_position = dspy.InputField(desc="Every vehicle must have exactly one position", dtype=str)
    result = dspy.OutputField(desc="Generated result", dtype=str)

class EmergencyVehicleSignature(dspy.Signature):
    """DSPy Signature for EmergencyVehicle
    
    Generated from: http://cns.ai/ontology/autonomous-vehicle#EmergencyVehicle
    Timestamp: 2025-07-24T08:26:19.214445"""
    
    priority = dspy.InputField(desc="Emergency vehicles must have priority 7-10", dtype=int)
    speed = dspy.InputField(desc="Emergency vehicles have extended speed limits", dtype=float)
    result = dspy.OutputField(desc="Generated result", dtype=str)

class PositionSignature(dspy.Signature):
    """DSPy Signature for Position
    
    Generated from: http://cns.ai/ontology/autonomous-vehicle#Position
    Timestamp: 2025-07-24T08:26:19.214592"""
    
    latitude = dspy.InputField(desc="Latitude must be valid WGS84 coordinate", dtype=float)
    longitude = dspy.InputField(desc="Longitude must be valid WGS84 coordinate", dtype=float)
    position_accuracy = dspy.InputField(desc="Position accuracy must be between 1cm and 10m", dtype=float)
    result = dspy.OutputField(desc="Generated result", dtype=str)

class LaneSignature(dspy.Signature):
    """DSPy Signature for Lane
    
    Generated from: http://cns.ai/ontology/autonomous-vehicle#Lane
    Timestamp: 2025-07-24T08:26:19.214716"""
    
    speed_limit = dspy.InputField(desc="Lane speed limit must be within reasonable bounds", dtype=float)
    lane_width = dspy.InputField(desc="Lane width must be between 2.5m and 4.5m", dtype=float)
    result = dspy.OutputField(desc="Generated result", dtype=str)

class CollisionRiskSignature(dspy.Signature):
    """DSPy Signature for CollisionRisk
    
    Generated from: http://cns.ai/ontology/autonomous-vehicle#CollisionRisk
    Timestamp: 2025-07-24T08:26:19.214883"""
    
    distance_to_collision = dspy.InputField(desc="Distance to collision must be within sensor range", dtype=float)
    risk_level = dspy.InputField(desc="Risk level must be normalized between 0.0 and 1.0", dtype=float)
    threat_vehicle = dspy.InputField(desc="Collision risk must identify threat vehicle", dtype=str)
    time_to_collision = dspy.InputField(desc="Time to collision must be within planning horizon", dtype=float)
    result = dspy.OutputField(desc="Generated result", dtype=str)

class EmergencyBrakeWarningSignature(dspy.Signature):
    """DSPy Signature for EmergencyBrakeWarning
    
    Generated from: http://cns.ai/ontology/autonomous-vehicle#EmergencyBrakeWarning
    Timestamp: 2025-07-24T08:26:19.215025"""
    
    priority = dspy.InputField(desc="Emergency brake warnings must have highest priority", dtype=str)
    transmission_range = dspy.InputField(desc="Emergency messages must have extended range", dtype=float)
    processing_latency = dspy.InputField(desc="Emergency messages must process within 100μs", dtype=float)
    result = dspy.OutputField(desc="Generated result", dtype=str)

class TrajectorySignature(dspy.Signature):
    """DSPy Signature for Trajectory
    
    Generated from: http://cns.ai/ontology/autonomous-vehicle#Trajectory
    Timestamp: 2025-07-24T08:26:19.215167"""
    
    prediction_horizon = dspy.InputField(desc="Prediction horizon must be between 100ms and 10s", dtype=float)
    confidence = dspy.InputField(desc="Prediction confidence must be between 0.0 and 1.0", dtype=float)
    trajectory_points = dspy.InputField(desc="Trajectory must contain prediction points", dtype=str)
    result = dspy.OutputField(desc="Generated result", dtype=str)

class EmergencyManeuverSignature(dspy.Signature):
    """DSPy Signature for EmergencyManeuver
    
    Generated from: http://cns.ai/ontology/autonomous-vehicle#EmergencyManeuver
    Timestamp: 2025-07-24T08:26:19.215306"""
    
    execution_time = dspy.InputField(desc="Maneuver execution time must be between 1ms and 5s", dtype=float)
    maneuver_urgency = dspy.InputField(desc="Maneuver urgency must be rated 1-10", dtype=int)
    maneuver_type = dspy.InputField(desc="Maneuver type must be from approved list", dtype=str)
    result = dspy.OutputField(desc="Generated result", dtype=str)

class VelocitySignature(dspy.Signature):
    """DSPy Signature for Velocity
    
    Generated from: http://cns.ai/ontology/autonomous-vehicle#Velocity
    Timestamp: 2025-07-24T08:26:19.215450"""
    
    velocity_z = dspy.InputField(desc="Z velocity component must be limited for ground vehicles", dtype=float)
    velocity_y = dspy.InputField(desc="Y velocity component must be within safe limits", dtype=float)
    velocity_x = dspy.InputField(desc="X velocity component must be within safe limits", dtype=float)
    result = dspy.OutputField(desc="Generated result", dtype=str)

class V2VMessageSignature(dspy.Signature):
    """DSPy Signature for V2VMessage
    
    Generated from: http://cns.ai/ontology/autonomous-vehicle#V2VMessage
    Timestamp: 2025-07-24T08:26:19.215650"""
    
    processing_latency = dspy.InputField(desc="Message processing must complete within 1ms", dtype=float)
    timestamp = dspy.InputField(desc="All messages must have a timestamp", dtype=str)
    priority = dspy.InputField(desc="Message priority must be 0-7 (802.11p standard)", dtype=int)
    sender_id = dspy.InputField(desc="Sender ID must be valid vehicle identifier", dtype=str)
    message_id = dspy.InputField(desc="Message ID must be 8-64 alphanumeric characters", dtype=str)
    ticks_used = dspy.InputField(desc="Message processing must use ≤8 CPU ticks", dtype=int)
    result = dspy.OutputField(desc="Generated result", dtype=str)

class BasicSafetyMessageSignature(dspy.Signature):
    """DSPy Signature for BasicSafetyMessage
    
    Generated from: http://cns.ai/ontology/autonomous-vehicle#BasicSafetyMessage
    Timestamp: 2025-07-24T08:26:19.215790"""
    
    transmission_range = dspy.InputField(desc="BSM transmission range must be 150-300m", dtype=float)
    priority = dspy.InputField(desc="Basic Safety Messages must have priority 3", dtype=str)
    processing_latency = dspy.InputField(desc="BSM processing must complete within 500μs", dtype=float)
    result = dspy.OutputField(desc="Generated result", dtype=str)


# Auto-generated signature registry
SIGNATURES = {
    "VehicleSignature": VehicleSignature,
    "EmergencyVehicleSignature": EmergencyVehicleSignature,
    "PositionSignature": PositionSignature,
    "LaneSignature": LaneSignature,
    "CollisionRiskSignature": CollisionRiskSignature,
    "EmergencyBrakeWarningSignature": EmergencyBrakeWarningSignature,
    "TrajectorySignature": TrajectorySignature,
    "EmergencyManeuverSignature": EmergencyManeuverSignature,
    "VelocitySignature": VelocitySignature,
    "V2VMessageSignature": V2VMessageSignature,
    "BasicSafetyMessageSignature": BasicSafetyMessageSignature,
}

def get_signature(name: str) -> dspy.Signature:
    """Get signature by name"""
    if name not in SIGNATURES:
        raise ValueError(f"Unknown signature: {name}. Available: {list(SIGNATURES.keys())}")
    return SIGNATURES[name]

def list_signatures() -> list:
    """List all available signature names"""
    return list(SIGNATURES.keys())
