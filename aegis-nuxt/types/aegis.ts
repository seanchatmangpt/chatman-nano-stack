// Generated TypeScript types from TTL ontology
// Generated on 2025-07-24T20:14:10.068298

export interface Alert {
  id: string
  uri: string
  label: string
}

export interface Antivirus {
  id: string
  uri: string
  label: string
}

export interface ApplicationAsset {
  id: string
  uri: string
  label: string
status: 'healthy' | 'warning' | 'critical' | 'offline'
  lastSeen: Date
}

export interface Asset {
  id: string
  uri: string
  label: string
assetID: string
assetType: string
assetOwner: string
criticality: number
confidentialityLevel: string
hasVulnerability: string
generatesEvent: string
bitactorID: string
status: 'healthy' | 'warning' | 'critical' | 'offline'
  lastSeen: Date
}

export interface Attack {
  id: string
  uri: string
  label: string
attackID: string
attackVector: string
sourceIP: string
destinationIP: string
sourcePort: number
destinationPort: number
attackTimestamp: Date
duration: string
exploits: string
affects: string
severity: 'low' | 'medium' | 'high' | 'critical'
  status: 'active' | 'neutralized' | 'investigating'
  targetType: string
}

export interface Botnet {
  id: string
  uri: string
  label: string
}

export interface CSRF {
  id: string
  uri: string
  label: string
}

export interface ComputeAsset {
  id: string
  uri: string
  label: string
status: 'healthy' | 'warning' | 'critical' | 'offline'
  lastSeen: Date
}

export interface CorrectiveControl {
  id: string
  uri: string
  label: string
}

export interface DDoSAttack {
  id: string
  uri: string
  label: string
severity: 'low' | 'medium' | 'high' | 'critical'
  status: 'active' | 'neutralized' | 'investigating'
  targetType: string
}

export interface DMZ {
  id: string
  uri: string
  label: string
}

export interface DataAsset {
  id: string
  uri: string
  label: string
status: 'healthy' | 'warning' | 'critical' | 'offline'
  lastSeen: Date
}

export interface DataExfiltration {
  id: string
  uri: string
  label: string
}

export interface DetectiveControl {
  id: string
  uri: string
  label: string
}

export interface EDR {
  id: string
  uri: string
  label: string
}

export interface Firewall {
  id: string
  uri: string
  label: string
}

export interface IDS {
  id: string
  uri: string
  label: string
}

export interface IOC {
  id: string
  uri: string
  label: string
iocType: string
iocValue: string
reputation: string
}

export interface IPS {
  id: string
  uri: string
  label: string
}

export interface InternalNetwork {
  id: string
  uri: string
  label: string
}

export interface LateralMovement {
  id: string
  uri: string
  label: string
}

export interface LoadBalancer {
  id: string
  uri: string
  label: string
}

export interface Malware {
  id: string
  uri: string
  label: string
threatFamily: string
md5Hash: string
sha256Hash: string
fileSize: number
}

export interface ManInTheMiddleAttack {
  id: string
  uri: string
  label: string
severity: 'low' | 'medium' | 'high' | 'critical'
  status: 'active' | 'neutralized' | 'investigating'
  targetType: string
}

export interface NetworkAsset {
  id: string
  uri: string
  label: string
ipAddress: string
macAddress: string
port: number
protocol: string
networkSegment: string
isOnline: boolean
communicatesWith: string
status: 'healthy' | 'warning' | 'critical' | 'offline'
  lastSeen: Date
}

export interface NetworkAttack {
  id: string
  uri: string
  label: string
packetCount: number
byteCount: number
severity: 'low' | 'medium' | 'high' | 'critical'
  status: 'active' | 'neutralized' | 'investigating'
  targetType: string
}

export interface NetworkDevice {
  id: string
  uri: string
  label: string
}

export interface NetworkSegment {
  id: string
  uri: string
  label: string
}

export interface PacketSniffing {
  id: string
  uri: string
  label: string
}

export interface PhishingAttack {
  id: string
  uri: string
  label: string
severity: 'low' | 'medium' | 'high' | 'critical'
  status: 'active' | 'neutralized' | 'investigating'
  targetType: string
}

export interface PortScan {
  id: string
  uri: string
  label: string
}

export interface PreventiveControl {
  id: string
  uri: string
  label: string
}

export interface PrivilegeEscalation {
  id: string
  uri: string
  label: string
}

export interface PublicNetwork {
  id: string
  uri: string
  label: string
}

export interface Ransomware {
  id: string
  uri: string
  label: string
}

export interface Rootkit {
  id: string
  uri: string
  label: string
}

export interface Router {
  id: string
  uri: string
  label: string
}

export interface SIEM {
  id: string
  uri: string
  label: string
}

export interface SOAR {
  id: string
  uri: string
  label: string
}

export interface SQLInjection {
  id: string
  uri: string
  label: string
}

export interface SecurityControl {
  id: string
  uri: string
  label: string
protects: string
realTimeProcessing: boolean
detectionAccuracy: string
responseTime: string
}

export interface SecurityEvent {
  id: string
  uri: string
  label: string
eventID: string
eventType: string
eventTimestamp: Date
eventSeverity: number
riskScore: string
falsePositiveProbability: string
processingLatency: string
ticksUsed: number
simdOptimized: boolean
}

export interface SecurityIncident {
  id: string
  uri: string
  label: string
incidentID: string
incidentStatus: string
containmentTime: string
recoveryTime: string
businessImpact: string
assignedAnalyst: string
}

export interface SocialEngineering {
  id: string
  uri: string
  label: string
}

export interface SpearPhishing {
  id: string
  uri: string
  label: string
}

export interface Spyware {
  id: string
  uri: string
  label: string
}

export interface Switch {
  id: string
  uri: string
  label: string
}

export interface Threat {
  id: string
  uri: string
  label: string
threatID: string
severity: number
confidence: string
firstSeen: Date
lastSeen: Date
targets: string
detectedBy: string
mitigatedBy: string
severity: 'low' | 'medium' | 'high' | 'critical'
  status: 'active' | 'neutralized' | 'investigating'
  targetType: string
}

export interface ThreatIntelligence {
  id: string
  uri: string
  label: string
source: string
reliability: string
severity: 'low' | 'medium' | 'high' | 'critical'
  status: 'active' | 'neutralized' | 'investigating'
  targetType: string
}

export interface Trojan {
  id: string
  uri: string
  label: string
}

export interface UserAsset {
  id: string
  uri: string
  label: string
status: 'healthy' | 'warning' | 'critical' | 'offline'
  lastSeen: Date
}

export interface VPN {
  id: string
  uri: string
  label: string
}

export interface Virus {
  id: string
  uri: string
  label: string
}

export interface Vulnerability {
  id: string
  uri: string
  label: string
cveID: string
cvssScore: string
exploitability: string
impactScore: string
disclosureDate: Date
patchAvailable: boolean
}

export interface WebAttack {
  id: string
  uri: string
  label: string
severity: 'low' | 'medium' | 'high' | 'critical'
  status: 'active' | 'neutralized' | 'investigating'
  targetType: string
}

export interface Worm {
  id: string
  uri: string
  label: string
}

export interface XSS {
  id: string
  uri: string
  label: string
}

export interface ZeroDayVulnerability {
  id: string
  uri: string
  label: string
}


export interface ThreatStats {
  total: number
  critical: number
  high: number
  medium: number
  low: number
  neutralized: number
}

export interface NetworkNode {
  id: string
  label: string
  type: string
  x?: number
  y?: number
  status: 'healthy' | 'warning' | 'critical' | 'offline'
}

export interface NetworkEdge {
  id: string
  source: string
  target: string
  type: string
  bandwidth?: number
  latency?: number
}

export interface AegisFabricMessage {
  type: string
  payload: any
  timestamp: number
}