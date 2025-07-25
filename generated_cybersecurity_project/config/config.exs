import Config

# Configure Ash
config :ash, :validate_domain_resource_inclusion?, false
config :ash, :validate_domain_config_inclusion?, false

# Configure the main domain
config :cybersecurity, Cybersecurity.Domain,
  resources: [
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
  ]

# Database configuration
config :cybersecurity, Cybersecurity.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "cybersecurity_dev",
  stacktrace: true,
  show_sensitive_data_on_connection_error: true,
  pool_size: 10

# Telemetry configuration
config :telemetry_poller, :default, period: :timer.seconds(5)

# Logger configuration
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Environment-specific configuration
import_config "#{config_env()}.exs"
