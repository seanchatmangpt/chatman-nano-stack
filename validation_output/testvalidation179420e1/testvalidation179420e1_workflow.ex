
defmodule TestValidation179420e1.Workflow do
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


  step :process_asset do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.AssetStep
  end

  step :process_networkasset do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.NetworkAssetStep
  end

  step :process_computeasset do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.ComputeAssetStep
  end

  step :process_dataasset do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.DataAssetStep
  end

  step :process_userasset do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.UserAssetStep
  end

  step :process_applicationasset do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.ApplicationAssetStep
  end

  step :process_networkdevice do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.NetworkDeviceStep
  end

  step :process_router do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.RouterStep
  end

  step :process_switch do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.SwitchStep
  end

  step :process_firewall do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.FirewallStep
  end

  step :process_loadbalancer do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.LoadBalancerStep
  end

  step :process_vpn do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.VPNStep
  end

  step :process_networksegment do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.NetworkSegmentStep
  end

  step :process_dmz do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.DMZStep
  end

  step :process_internalnetwork do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.InternalNetworkStep
  end

  step :process_publicnetwork do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.PublicNetworkStep
  end

  step :process_threat do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.ThreatStep
  end

  step :process_malware do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.MalwareStep
  end

  step :process_virus do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.VirusStep
  end

  step :process_worm do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.WormStep
  end

  step :process_trojan do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.TrojanStep
  end

  step :process_ransomware do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.RansomwareStep
  end

  step :process_rootkit do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.RootkitStep
  end

  step :process_spyware do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.SpywareStep
  end

  step :process_botnet do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.BotnetStep
  end

  step :process_attack do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.AttackStep
  end

  step :process_networkattack do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.NetworkAttackStep
  end

  step :process_ddosattack do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.DDoSAttackStep
  end

  step :process_maninthemiddleattack do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.ManInTheMiddleAttackStep
  end

  step :process_packetsniffing do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.PacketSniffingStep
  end

  step :process_portscan do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.PortScanStep
  end

  step :process_webattack do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.WebAttackStep
  end

  step :process_sqlinjection do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.SQLInjectionStep
  end

  step :process_xss do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.XSSStep
  end

  step :process_csrf do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.CSRFStep
  end

  step :process_socialengineering do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.SocialEngineeringStep
  end

  step :process_phishingattack do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.PhishingAttackStep
  end

  step :process_spearphishing do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.SpearPhishingStep
  end

  step :process_privilegeescalation do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.PrivilegeEscalationStep
  end

  step :process_lateralmovement do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.LateralMovementStep
  end

  step :process_dataexfiltration do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.DataExfiltrationStep
  end

  step :process_securityevent do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.SecurityEventStep
  end

  step :process_securityincident do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.SecurityIncidentStep
  end

  step :process_vulnerability do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.VulnerabilityStep
  end

  step :process_zerodayvulnerability do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.ZeroDayVulnerabilityStep
  end

  step :process_alert do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.AlertStep
  end

  step :process_threatintelligence do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.ThreatIntelligenceStep
  end

  step :process_ioc do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.IOCStep
  end

  step :process_securitycontrol do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.SecurityControlStep
  end

  step :process_preventivecontrol do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.PreventiveControlStep
  end

  step :process_detectivecontrol do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.DetectiveControlStep
  end

  step :process_correctivecontrol do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.CorrectiveControlStep
  end

  step :process_ids do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.IDSStep
  end

  step :process_ips do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.IPSStep
  end

  step :process_siem do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.SIEMStep
  end

  step :process_antivirus do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.AntivirusStep
  end

  step :process_edr do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.EDRStep
  end

  step :process_soar do
    argument :input_data, input(:raw_data)
    async? false  # Critical path - synchronous execution
    max_retries 0  # No retries for ultra-low latency
    run TestValidation179420e1.Steps.SOARStep
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
