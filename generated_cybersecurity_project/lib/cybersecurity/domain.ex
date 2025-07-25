defmodule Cybersecurity.Domain do
  @moduledoc """
  Main Ash.Domain for cybersecurity
  Generated from ontology: cybersecurity
  """

  use Ash.Domain

  resources do
    resource Cybersecurity.Resources.Asset
    resource Cybersecurity.Resources.NetworkAsset
    resource Cybersecurity.Resources.ComputeAsset
    resource Cybersecurity.Resources.DataAsset
    resource Cybersecurity.Resources.UserAsset
    resource Cybersecurity.Resources.ApplicationAsset
    resource Cybersecurity.Resources.NetworkDevice
    resource Cybersecurity.Resources.Router
    resource Cybersecurity.Resources.Switch
    resource Cybersecurity.Resources.Firewall
    resource Cybersecurity.Resources.LoadBalancer
    resource Cybersecurity.Resources.VPN
    resource Cybersecurity.Resources.NetworkSegment
    resource Cybersecurity.Resources.DMZ
    resource Cybersecurity.Resources.InternalNetwork
    resource Cybersecurity.Resources.PublicNetwork
    resource Cybersecurity.Resources.Threat
    resource Cybersecurity.Resources.Malware
    resource Cybersecurity.Resources.Virus
    resource Cybersecurity.Resources.Worm
    resource Cybersecurity.Resources.Trojan
    resource Cybersecurity.Resources.Ransomware
    resource Cybersecurity.Resources.Rootkit
    resource Cybersecurity.Resources.Spyware
    resource Cybersecurity.Resources.Botnet
    resource Cybersecurity.Resources.Attack
    resource Cybersecurity.Resources.NetworkAttack
    resource Cybersecurity.Resources.DDoSAttack
    resource Cybersecurity.Resources.ManInTheMiddleAttack
    resource Cybersecurity.Resources.PacketSniffing
    resource Cybersecurity.Resources.PortScan
    resource Cybersecurity.Resources.WebAttack
    resource Cybersecurity.Resources.SQLInjection
    resource Cybersecurity.Resources.XSS
    resource Cybersecurity.Resources.CSRF
    resource Cybersecurity.Resources.SocialEngineering
    resource Cybersecurity.Resources.PhishingAttack
    resource Cybersecurity.Resources.SpearPhishing
    resource Cybersecurity.Resources.PrivilegeEscalation
    resource Cybersecurity.Resources.LateralMovement
    resource Cybersecurity.Resources.DataExfiltration
    resource Cybersecurity.Resources.SecurityEvent
    resource Cybersecurity.Resources.SecurityIncident
    resource Cybersecurity.Resources.Vulnerability
    resource Cybersecurity.Resources.ZeroDayVulnerability
    resource Cybersecurity.Resources.Alert
    resource Cybersecurity.Resources.ThreatIntelligence
    resource Cybersecurity.Resources.IOC
    resource Cybersecurity.Resources.SecurityControl
    resource Cybersecurity.Resources.PreventiveControl
    resource Cybersecurity.Resources.DetectiveControl
    resource Cybersecurity.Resources.CorrectiveControl
    resource Cybersecurity.Resources.IDS
    resource Cybersecurity.Resources.IPS
    resource Cybersecurity.Resources.SIEM
    resource Cybersecurity.Resources.Antivirus
    resource Cybersecurity.Resources.EDR
    resource Cybersecurity.Resources.SOAR
  end
end
