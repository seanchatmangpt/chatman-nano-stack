
defmodule AegisFabric.Workflow do
  @moduledoc """
  Saga-based Reactor workflow with full compensation support
  Ensures distributed transaction semantics for CNS/BitActor operations
  """
  
  use Reactor

  input :raw_data
  input :config

  step :validate_input do
    argument :raw_data, input(:raw_data)
    run fn %{raw_data: data}, _context ->
      {:ok, data}
    end
  end

  group :saga_transaction do
    before_all &setup_transaction/3
    after_all &cleanup_transaction/1


  step :process_aegisfabric do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AegisFabric.Steps.AegisFabricStep
  end

  step :process_bitactor do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AegisFabric.Steps.BitActorStep
  end

  step :process_protectionmodule do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AegisFabric.Steps.ProtectionModuleStep
  end

  step :process_servicemesh do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AegisFabric.Steps.ServiceMeshStep
  end

  step :process_threatgossipprotocol do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AegisFabric.Steps.ThreatGossipProtocolStep
  end

  step :process_threatsignature do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AegisFabric.Steps.ThreatSignatureStep
  end

  step :process_kubernetesdeployment do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AegisFabric.Steps.KubernetesDeploymentStep
  end

  step :process_networkpolicy do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AegisFabric.Steps.NetworkPolicyStep
  end

  step :process_performancerequirements do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AegisFabric.Steps.PerformanceRequirementsStep
  end

  step :process_validationgauntlet do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AegisFabric.Steps.ValidationGauntletStep
  end

  step :process_securitypolicy do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AegisFabric.Steps.SecurityPolicyStep
  end

  step :process_observability do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AegisFabric.Steps.ObservabilityStep
  end

  step :process_threatresponse do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AegisFabric.Steps.ThreatResponseStep
  end

  step :process_operations do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run AegisFabric.Steps.OperationsStep
  end
  end

  return :processed_result
  
  defp setup_transaction(inputs, context, _options) do
    # Initialize distributed transaction context
    transaction_id = UUID.uuid4()
    {:ok, Map.put(context, :transaction_id, transaction_id)}
  end
  
  defp cleanup_transaction(_result) do
    # Final cleanup
    :ok
  end
end
