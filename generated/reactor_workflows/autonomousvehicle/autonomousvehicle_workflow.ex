
defmodule AutonomousVehicle.Workflow do
  @moduledoc """
  High-performance parallel Reactor workflow
  Optimized for concurrent semantic processing
  """
  
  use Reactor

  input :raw_data
  input :config

  step :validate_input do
    argument :raw_data, input(:raw_data)
    async? false  # Input validation is critical path
    run fn %{raw_data: data}, _context ->
      {:ok, data}
    end
  end


  step :process_vehicle do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AutonomousVehicle.Steps.VehicleStep
  end

  step :process_emergencyvehicle do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AutonomousVehicle.Steps.EmergencyVehicleStep
  end

  step :process_passengervehicle do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AutonomousVehicle.Steps.PassengerVehicleStep
  end

  step :process_commercialvehicle do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AutonomousVehicle.Steps.CommercialVehicleStep
  end

  step :process_motorcyclevehicle do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AutonomousVehicle.Steps.MotorcycleVehicleStep
  end

  step :process_position do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AutonomousVehicle.Steps.PositionStep
  end

  step :process_velocity do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AutonomousVehicle.Steps.VelocityStep
  end

  step :process_trajectory do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AutonomousVehicle.Steps.TrajectoryStep
  end

  step :process_lane do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AutonomousVehicle.Steps.LaneStep
  end

  step :process_intersection do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AutonomousVehicle.Steps.IntersectionStep
  end

  step :process_trafficsignal do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AutonomousVehicle.Steps.TrafficSignalStep
  end

  step :process_v2vmessage do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AutonomousVehicle.Steps.V2VMessageStep
  end

  step :process_basicsafetymessage do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AutonomousVehicle.Steps.BasicSafetyMessageStep
  end

  step :process_emergencybrakewarning do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AutonomousVehicle.Steps.EmergencyBrakeWarningStep
  end

  step :process_intersectionmovementassist do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AutonomousVehicle.Steps.IntersectionMovementAssistStep
  end

  step :process_blindspotwarning do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AutonomousVehicle.Steps.BlindSpotWarningStep
  end

  step :process_collisionrisk do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AutonomousVehicle.Steps.CollisionRiskStep
  end

  step :process_safetyconstraint do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AutonomousVehicle.Steps.SafetyConstraintStep
  end

  step :process_emergencymaneuver do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AutonomousVehicle.Steps.EmergencyManeuverStep
  end

  collect :aggregate_results do
    argument :results, [result(:process_semantic_concept_1), result(:process_semantic_concept_2)]
    transform fn inputs ->
      %{
        combined_results: inputs,
        processing_complete: true,
        timestamp: DateTime.utc_now()
      }
    end
  end

  return :processed_result
end
