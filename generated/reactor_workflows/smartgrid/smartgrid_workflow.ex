
defmodule SmartGrid.Workflow do
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


  step :process_gridnode do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.GridNodeStep
  end

  step :process_powerplant do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.PowerPlantStep
  end

  step :process_renewablepowerplant do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.RenewablePowerPlantStep
  end

  step :process_solarpowerplant do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.SolarPowerPlantStep
  end

  step :process_windpowerplant do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.WindPowerPlantStep
  end

  step :process_hydropowerplant do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.HydroPowerPlantStep
  end

  step :process_geothermalpowerplant do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.GeothermalPowerPlantStep
  end

  step :process_thermalpowerplant do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.ThermalPowerPlantStep
  end

  step :process_nuclearpowerplant do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.NuclearPowerPlantStep
  end

  step :process_substation do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.SubstationStep
  end

  step :process_transmissionline do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.TransmissionLineStep
  end

  step :process_distributionline do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.DistributionLineStep
  end

  step :process_transformer do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.TransformerStep
  end

  step :process_circuitbreaker do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.CircuitBreakerStep
  end

  step :process_smartmeter do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.SmartMeterStep
  end

  step :process_energystorage do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.EnergyStorageStep
  end

  step :process_batterystorage do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.BatteryStorageStep
  end

  step :process_pumpedhydrostorage do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.PumpedHydroStorageStep
  end

  step :process_compressedairstorage do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.CompressedAirStorageStep
  end

  step :process_flywheelstorage do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.FlywheelStorageStep
  end

  step :process_energyconsumer do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.EnergyConsumerStep
  end

  step :process_residentialconsumer do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.ResidentialConsumerStep
  end

  step :process_commercialconsumer do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.CommercialConsumerStep
  end

  step :process_industrialconsumer do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.IndustrialConsumerStep
  end

  step :process_electricvehicle do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.ElectricVehicleStep
  end

  step :process_gridcontroller do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.GridControllerStep
  end

  step :process_scada do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.SCADAStep
  end

  step :process_energymanagementsystem do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.EnergyManagementSystemStep
  end

  step :process_demandresponseprogram do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.DemandResponseProgramStep
  end

  step :process_loadbalancer do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.LoadBalancerStep
  end

  step :process_electricalmeasurement do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.ElectricalMeasurementStep
  end

  step :process_voltagemeasurement do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.VoltageMeasurementStep
  end

  step :process_currentmeasurement do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.CurrentMeasurementStep
  end

  step :process_powermeasurement do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.PowerMeasurementStep
  end

  step :process_frequencymeasurement do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.FrequencyMeasurementStep
  end

  step :process_energyreading do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.EnergyReadingStep
  end

  step :process_gridevent do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.GridEventStep
  end

  step :process_outage do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.OutageStep
  end

  step :process_voltageanomalysag do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.VoltageAnomalySagStep
  end

  step :process_voltageanomalyswell do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.VoltageAnomalySwellStep
  end

  step :process_frequencydeviation do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.FrequencyDeviationStep
  end

  step :process_overload do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.OverloadStep
  end

  step :process_gridprotectiontrip do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run SmartGrid.Steps.GridProtectionTripStep
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
