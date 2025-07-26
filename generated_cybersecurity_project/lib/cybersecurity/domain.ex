defmodule Cybersecurity.Domain do
  @moduledoc """
  Main Ash.Domain for cybersecurity
  Generated from ontology: cybersecurity
  """

  # 80/20: Plain Elixir domain module instead of Ash.Domain
  
  @resources [
    Cybersecurity.Resources.Asset,
    Cybersecurity.Resources.NetworkAsset,
    Cybersecurity.Resources.ComputeAsset,
    Cybersecurity.Resources.DataAsset,
    Cybersecurity.Resources.UserAsset,
    Cybersecurity.Resources.ApplicationAsset,
    Cybersecurity.Resources.NetworkDevice,
    Cybersecurity.Resources.Router,
    Cybersecurity.Resources.Switch,
    Cybersecurity.Resources.Firewall,
    Cybersecurity.Resources.LoadBalancer,
    Cybersecurity.Resources.VPN,
    Cybersecurity.Resources.NetworkSegment,
    Cybersecurity.Resources.DMZ,
    Cybersecurity.Resources.InternalNetwork,
    Cybersecurity.Resources.PublicNetwork,
    Cybersecurity.Resources.Threat,
    Cybersecurity.Resources.Malware,
    Cybersecurity.Resources.Virus,
    Cybersecurity.Resources.Worm,
    Cybersecurity.Resources.Trojan,
    Cybersecurity.Resources.Ransomware,
    Cybersecurity.Resources.Rootkit,
    Cybersecurity.Resources.Spyware,
    Cybersecurity.Resources.Botnet,
    Cybersecurity.Resources.Attack,
    Cybersecurity.Resources.NetworkAttack,
    Cybersecurity.Resources.DDoSAttack,
    Cybersecurity.Resources.ManInTheMiddleAttack,
    Cybersecurity.Resources.PacketSniffing,
    Cybersecurity.Resources.PortScan,
    Cybersecurity.Resources.WebAttack,
    Cybersecurity.Resources.SQLInjection,
    Cybersecurity.Resources.XSS,
    Cybersecurity.Resources.CSRF,
    Cybersecurity.Resources.SocialEngineering,
    Cybersecurity.Resources.PhishingAttack,
    Cybersecurity.Resources.SpearPhishing,
    Cybersecurity.Resources.PrivilegeEscalation,
    Cybersecurity.Resources.LateralMovement,
    Cybersecurity.Resources.DataExfiltration,
    Cybersecurity.Resources.SecurityEvent,
    Cybersecurity.Resources.SecurityIncident,
    Cybersecurity.Resources.Vulnerability,
    Cybersecurity.Resources.ZeroDayVulnerability,
    Cybersecurity.Resources.Alert,
    Cybersecurity.Resources.ThreatIntelligence,
    Cybersecurity.Resources.IOC,
    Cybersecurity.Resources.SecurityControl,
    Cybersecurity.Resources.PreventiveControl,
    Cybersecurity.Resources.DetectiveControl,
    Cybersecurity.Resources.CorrectiveControl,
    Cybersecurity.Resources.IDS,
    Cybersecurity.Resources.IPS,
    Cybersecurity.Resources.SIEM,
    Cybersecurity.Resources.Antivirus,
    Cybersecurity.Resources.EDR,
    Cybersecurity.Resources.SOAR
  ]
  
  def resources, do: @resources
  
  def resource_names do
    @resources
    |> Enum.map(fn module ->
      module
      |> Module.split()
      |> List.last()
      |> String.downcase()
    end)
  end
end
