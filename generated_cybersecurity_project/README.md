# Cybersecurity

An Ash.Reactor application generated from the `cybersecurity` ontology.

## Overview

This project demonstrates a complete Ash.Reactor implementation that transforms ontological concepts into working Elixir code. It includes:

- **Domain Resources**: Ash resources for each ontology class
- **Reactor Workflows**: Business logic orchestration using Ash.Reactor
- **Complete CRUD Operations**: Full create, read, update, delete functionality
- **Comprehensive Tests**: Full test coverage for all components

## Generated Components

### Resources
- `Asset` - Security Asset: Any system, device, or resource that requires protection
- `NetworkAsset` - Network Asset: Network infrastructure requiring security monitoring
- `ComputeAsset` - Compute Asset: Computing device or system
- `DataAsset` - Data Asset: Information or data requiring protection
- `UserAsset` - User Asset: Human user account or identity
- `ApplicationAsset` - Application Asset: Software application or service
- `NetworkDevice` - Network Device: Physical or virtual network infrastructure device
- `Router` - Network Router: None
- `Switch` - Network Switch: None
- `Firewall` - Network Firewall: None
- `LoadBalancer` - Load Balancer: None
- `VPN` - VPN Gateway: None
- `NetworkSegment` - Network Segment: Logical or physical network subdivision
- `DMZ` - Demilitarized Zone: None
- `InternalNetwork` - Internal Network: None
- `PublicNetwork` - Public Network: None
- `Threat` - Cybersecurity Threat: Potential danger to security assets
- `Malware` - Malware: Malicious software threat
- `Virus` - Computer Virus: None
- `Worm` - Computer Worm: None
- `Trojan` - Trojan Horse: None
- `Ransomware` - Ransomware: None
- `Rootkit` - Rootkit: None
- `Spyware` - Spyware: None
- `Botnet` - Botnet: None
- `Attack` - Cyber Attack: Active exploitation attempt or malicious activity
- `NetworkAttack` - Network Attack: None
- `DDoSAttack` - Distributed Denial of Service Attack: None
- `ManInTheMiddleAttack` - Man-in-the-Middle Attack: None
- `PacketSniffing` - Packet Sniffing: None
- `PortScan` - Port Scanning: None
- `WebAttack` - Web Application Attack: None
- `SQLInjection` - SQL Injection: None
- `XSS` - Cross-Site Scripting: None
- `CSRF` - Cross-Site Request Forgery: None
- `SocialEngineering` - Social Engineering: None
- `PhishingAttack` - Phishing Attack: None
- `SpearPhishing` - Spear Phishing: None
- `PrivilegeEscalation` - Privilege Escalation: None
- `LateralMovement` - Lateral Movement: None
- `DataExfiltration` - Data Exfiltration: None
- `SecurityEvent` - Security Event: Observed security-relevant occurrence
- `SecurityIncident` - Security Incident: Confirmed security breach or compromise
- `Vulnerability` - Security Vulnerability: Weakness that can be exploited by threats
- `ZeroDayVulnerability` - Zero-Day Vulnerability: None
- `Alert` - Security Alert: Automated security notification
- `ThreatIntelligence` - Threat Intelligence: Information about current and emerging threats
- `IOC` - Indicator of Compromise: Observable evidence of security breach
- `SecurityControl` - Security Control: Protective measure or safeguard
- `PreventiveControl` - Preventive Control: None
- `DetectiveControl` - Detective Control: None
- `CorrectiveControl` - Corrective Control: None
- `IDS` - Intrusion Detection System: None
- `IPS` - Intrusion Prevention System: None
- `SIEM` - Security Information and Event Management: None
- `Antivirus` - Antivirus Software: None
- `EDR` - Endpoint Detection and Response: None
- `SOAR` - Source Reliability (0.0-1.0): None

### Workflows
- `MainWorkflow` - Main domain orchestration
- `AssetWorkflow` - Operations for Asset resources
- `NetworkAssetWorkflow` - Operations for NetworkAsset resources
- `ComputeAssetWorkflow` - Operations for ComputeAsset resources
- `DataAssetWorkflow` - Operations for DataAsset resources
- `UserAssetWorkflow` - Operations for UserAsset resources
- `ApplicationAssetWorkflow` - Operations for ApplicationAsset resources
- `NetworkDeviceWorkflow` - Operations for NetworkDevice resources
- `RouterWorkflow` - Operations for Router resources
- `SwitchWorkflow` - Operations for Switch resources
- `FirewallWorkflow` - Operations for Firewall resources
- `LoadBalancerWorkflow` - Operations for LoadBalancer resources
- `VPNWorkflow` - Operations for VPN resources
- `NetworkSegmentWorkflow` - Operations for NetworkSegment resources
- `DMZWorkflow` - Operations for DMZ resources
- `InternalNetworkWorkflow` - Operations for InternalNetwork resources
- `PublicNetworkWorkflow` - Operations for PublicNetwork resources
- `ThreatWorkflow` - Operations for Threat resources
- `MalwareWorkflow` - Operations for Malware resources
- `VirusWorkflow` - Operations for Virus resources
- `WormWorkflow` - Operations for Worm resources
- `TrojanWorkflow` - Operations for Trojan resources
- `RansomwareWorkflow` - Operations for Ransomware resources
- `RootkitWorkflow` - Operations for Rootkit resources
- `SpywareWorkflow` - Operations for Spyware resources
- `BotnetWorkflow` - Operations for Botnet resources
- `AttackWorkflow` - Operations for Attack resources
- `NetworkAttackWorkflow` - Operations for NetworkAttack resources
- `DDoSAttackWorkflow` - Operations for DDoSAttack resources
- `ManInTheMiddleAttackWorkflow` - Operations for ManInTheMiddleAttack resources
- `PacketSniffingWorkflow` - Operations for PacketSniffing resources
- `PortScanWorkflow` - Operations for PortScan resources
- `WebAttackWorkflow` - Operations for WebAttack resources
- `SQLInjectionWorkflow` - Operations for SQLInjection resources
- `XSSWorkflow` - Operations for XSS resources
- `CSRFWorkflow` - Operations for CSRF resources
- `SocialEngineeringWorkflow` - Operations for SocialEngineering resources
- `PhishingAttackWorkflow` - Operations for PhishingAttack resources
- `SpearPhishingWorkflow` - Operations for SpearPhishing resources
- `PrivilegeEscalationWorkflow` - Operations for PrivilegeEscalation resources
- `LateralMovementWorkflow` - Operations for LateralMovement resources
- `DataExfiltrationWorkflow` - Operations for DataExfiltration resources
- `SecurityEventWorkflow` - Operations for SecurityEvent resources
- `SecurityIncidentWorkflow` - Operations for SecurityIncident resources
- `VulnerabilityWorkflow` - Operations for Vulnerability resources
- `ZeroDayVulnerabilityWorkflow` - Operations for ZeroDayVulnerability resources
- `AlertWorkflow` - Operations for Alert resources
- `ThreatIntelligenceWorkflow` - Operations for ThreatIntelligence resources
- `IOCWorkflow` - Operations for IOC resources
- `SecurityControlWorkflow` - Operations for SecurityControl resources
- `PreventiveControlWorkflow` - Operations for PreventiveControl resources
- `DetectiveControlWorkflow` - Operations for DetectiveControl resources
- `CorrectiveControlWorkflow` - Operations for CorrectiveControl resources
- `IDSWorkflow` - Operations for IDS resources
- `IPSWorkflow` - Operations for IPS resources
- `SIEMWorkflow` - Operations for SIEM resources
- `AntivirusWorkflow` - Operations for Antivirus resources
- `EDRWorkflow` - Operations for EDR resources
- `SOARWorkflow` - Operations for SOAR resources

## Getting Started

### Prerequisites
- Elixir 1.15+
- PostgreSQL 12+

### Installation

1. Install dependencies:
   ```bash
   mix deps.get
   ```

2. Set up the database:
   ```bash
   mix ecto.setup
   ```

3. Run tests:
   ```bash
   mix test
   ```

### Usage

#### Using Resources Directly

```elixir
# Create a resource
MyApp.Resources.SomeResource.init_storage()
{:ok, resource} = MyApp.Resources.SomeResource.create(%{
  name: "Example",
  description: "Created directly"
})

# Read resources
{:ok, resources} = MyApp.Resources.SomeResource.list()

# Update a resource
{:ok, updated} = MyApp.Resources.SomeResource.update(resource.id, %{name: "Updated Name"})
```

#### Using Reactor Workflows

```elixir
# Run the main workflow
{:ok, result} = Cybersecurity.Workflows.MainWorkflow.execute(:process, %{key: "value"})

# Run a resource-specific workflow  
{:ok, result} = Cybersecurity.Workflows.SomeResourceWorkflow.execute(:create, %{name: "Example"})
```

## Architecture

This application follows a simple 80/20 approach:

1. **Resources** are plain Elixir structs with CRUD operations using ETS
2. **Workflows** are plain Elixir modules with simple function-based orchestration
3. **Domain** coordinates all resources and provides a unified interface

### Key Features

- **Simple Structs**: Domain entities as plain Elixir structs
- **ETS Storage**: In-memory storage with no database dependencies  
- **Type Safety**: Comprehensive validation and error handling
- **Minimal Dependencies**: Only Jason and UUID for maximum compatibility
- **Test Coverage**: Comprehensive test suite

## Development

### Running Tests
```bash
# Run all tests
mix test

# Run with coverage
mix coveralls

# Run specific test file
mix test test/path/to/test_file.exs
```

### Code Quality
```bash
# Format code
mix format

# Static analysis
mix credo

# Type checking  
mix dialyzer
```

## Generated from Ontology

This project was automatically generated from the `cybersecurity` ontology using the Ontology to Ash.Reactor Generator.

**Generated at**: 2025-07-25T14:21:20.246653

## License

Generated code - modify as needed for your use case.
