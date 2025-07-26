import { d as defineWebSocketHandler } from '../../../nitro/nitro.mjs';
import 'node:http';
import 'node:https';
import 'node:crypto';
import 'stream';
import 'events';
import 'http';
import 'crypto';
import 'buffer';
import 'zlib';
import 'https';
import 'net';
import 'tls';
import 'url';
import 'node:events';
import 'node:buffer';
import 'node:fs';
import 'node:path';
import 'node:url';

const clients = /* @__PURE__ */ new Map();
const ws = defineWebSocketHandler({
  open(peer) {
    const client = peer;
    client.id = Math.random().toString(36).substring(7);
    client.subscriptions = /* @__PURE__ */ new Set();
    clients.set(client.id, client);
    console.log(`Client ${client.id} connected`);
    sendInitialState(client);
  },
  message(peer, message) {
    const client = peer;
    const data = JSON.parse(message.text());
    console.log(`Message from ${client.id}:`, data);
    switch (data.type) {
      case "subscribe":
        handleSubscribe(client, data.channels);
        break;
      case "unsubscribe":
        handleUnsubscribe(client, data.channels);
        break;
      case "neutralizeThreat":
        handleNeutralizeThreat(client, data.threatId);
        break;
      default:
        console.warn("Unknown message type:", data.type);
    }
  },
  close(peer) {
    const client = peer;
    clients.delete(client.id);
    console.log(`Client ${client.id} disconnected`);
  },
  error(peer, error) {
    console.error("WebSocket error:", error);
  }
});
function sendInitialState(client) {
  client.send(JSON.stringify({
    type: "AlertUpdate",
    payload: getMockAlerts()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "AntivirusUpdate",
    payload: getMockAntiviruss()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "ApplicationAssetUpdate",
    payload: getMockApplicationAssets()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "AssetUpdate",
    payload: getMockAssets()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "AttackUpdate",
    payload: getMockAttacks()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "BotnetUpdate",
    payload: getMockBotnets()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "CSRFUpdate",
    payload: getMockCSRFs()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "ComputeAssetUpdate",
    payload: getMockComputeAssets()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "CorrectiveControlUpdate",
    payload: getMockCorrectiveControls()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "DDoSAttackUpdate",
    payload: getMockDDoSAttacks()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "DMZUpdate",
    payload: getMockDMZs()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "DataAssetUpdate",
    payload: getMockDataAssets()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "DataExfiltrationUpdate",
    payload: getMockDataExfiltrations()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "DetectiveControlUpdate",
    payload: getMockDetectiveControls()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "EDRUpdate",
    payload: getMockEDRs()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "FirewallUpdate",
    payload: getMockFirewalls()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "IDSUpdate",
    payload: getMockIDSs()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "IOCUpdate",
    payload: getMockIOCs()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "IPSUpdate",
    payload: getMockIPSs()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "InternalNetworkUpdate",
    payload: getMockInternalNetworks()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "LateralMovementUpdate",
    payload: getMockLateralMovements()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "LoadBalancerUpdate",
    payload: getMockLoadBalancers()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "MalwareUpdate",
    payload: getMockMalwares()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "ManInTheMiddleAttackUpdate",
    payload: getMockManInTheMiddleAttacks()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "NetworkAssetUpdate",
    payload: getMockNetworkAssets()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "NetworkAttackUpdate",
    payload: getMockNetworkAttacks()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "NetworkDeviceUpdate",
    payload: getMockNetworkDevices()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "NetworkSegmentUpdate",
    payload: getMockNetworkSegments()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "PacketSniffingUpdate",
    payload: getMockPacketSniffings()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "PhishingAttackUpdate",
    payload: getMockPhishingAttacks()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "PortScanUpdate",
    payload: getMockPortScans()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "PreventiveControlUpdate",
    payload: getMockPreventiveControls()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "PrivilegeEscalationUpdate",
    payload: getMockPrivilegeEscalations()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "PublicNetworkUpdate",
    payload: getMockPublicNetworks()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "RansomwareUpdate",
    payload: getMockRansomwares()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "RootkitUpdate",
    payload: getMockRootkits()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "RouterUpdate",
    payload: getMockRouters()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "SIEMUpdate",
    payload: getMockSIEMs()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "SOARUpdate",
    payload: getMockSOARs()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "SQLInjectionUpdate",
    payload: getMockSQLInjections()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "SecurityControlUpdate",
    payload: getMockSecurityControls()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "SecurityEventUpdate",
    payload: getMockSecurityEvents()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "SecurityIncidentUpdate",
    payload: getMockSecurityIncidents()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "SocialEngineeringUpdate",
    payload: getMockSocialEngineerings()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "SpearPhishingUpdate",
    payload: getMockSpearPhishings()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "SpywareUpdate",
    payload: getMockSpywares()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "SwitchUpdate",
    payload: getMockSwitchs()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "ThreatUpdate",
    payload: getMockThreats()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "ThreatIntelligenceUpdate",
    payload: getMockThreatIntelligences()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "TrojanUpdate",
    payload: getMockTrojans()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "UserAssetUpdate",
    payload: getMockUserAssets()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "VPNUpdate",
    payload: getMockVPNs()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "VirusUpdate",
    payload: getMockViruss()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "VulnerabilityUpdate",
    payload: getMockVulnerabilitys()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "WebAttackUpdate",
    payload: getMockWebAttacks()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "WormUpdate",
    payload: getMockWorms()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "XSSUpdate",
    payload: getMockXSSs()
    // TODO: Replace with real data from BitActor
  }));
  client.send(JSON.stringify({
    type: "ZeroDayVulnerabilityUpdate",
    payload: getMockZeroDayVulnerabilitys()
    // TODO: Replace with real data from BitActor
  }));
}
function handleSubscribe(client, channels) {
  channels.forEach((channel) => {
    client.subscriptions.add(channel);
  });
}
function handleUnsubscribe(client, channels) {
  channels.forEach((channel) => {
    client.subscriptions.delete(channel);
  });
}
function handleNeutralizeThreat(client, threatId) {
  console.log(`Neutralizing threat ${threatId}`);
  broadcast({
    type: "threatNeutralized",
    threatId,
    timestamp: Date.now()
  });
}
function broadcast(data) {
  const message = JSON.stringify(data);
  clients.forEach((client) => {
    client.send(message);
  });
}
function getMockAlerts() {
  return [];
}
function getMockAntiviruss() {
  return [];
}
function getMockApplicationAssets() {
  return [];
}
function getMockAssets() {
  return [];
}
function getMockAttacks() {
  return [];
}
function getMockBotnets() {
  return [];
}
function getMockCSRFs() {
  return [];
}
function getMockComputeAssets() {
  return [];
}
function getMockCorrectiveControls() {
  return [];
}
function getMockDDoSAttacks() {
  return [];
}
function getMockDMZs() {
  return [];
}
function getMockDataAssets() {
  return [];
}
function getMockDataExfiltrations() {
  return [];
}
function getMockDetectiveControls() {
  return [];
}
function getMockEDRs() {
  return [];
}
function getMockFirewalls() {
  return [];
}
function getMockIDSs() {
  return [];
}
function getMockIOCs() {
  return [];
}
function getMockIPSs() {
  return [];
}
function getMockInternalNetworks() {
  return [];
}
function getMockLateralMovements() {
  return [];
}
function getMockLoadBalancers() {
  return [];
}
function getMockMalwares() {
  return [];
}
function getMockManInTheMiddleAttacks() {
  return [];
}
function getMockNetworkAssets() {
  return [];
}
function getMockNetworkAttacks() {
  return [];
}
function getMockNetworkDevices() {
  return [];
}
function getMockNetworkSegments() {
  return [];
}
function getMockPacketSniffings() {
  return [];
}
function getMockPhishingAttacks() {
  return [];
}
function getMockPortScans() {
  return [];
}
function getMockPreventiveControls() {
  return [];
}
function getMockPrivilegeEscalations() {
  return [];
}
function getMockPublicNetworks() {
  return [];
}
function getMockRansomwares() {
  return [];
}
function getMockRootkits() {
  return [];
}
function getMockRouters() {
  return [];
}
function getMockSIEMs() {
  return [];
}
function getMockSOARs() {
  return [];
}
function getMockSQLInjections() {
  return [];
}
function getMockSecurityControls() {
  return [];
}
function getMockSecurityEvents() {
  return [];
}
function getMockSecurityIncidents() {
  return [];
}
function getMockSocialEngineerings() {
  return [];
}
function getMockSpearPhishings() {
  return [];
}
function getMockSpywares() {
  return [];
}
function getMockSwitchs() {
  return [];
}
function getMockThreats() {
  return [];
}
function getMockThreatIntelligences() {
  return [];
}
function getMockTrojans() {
  return [];
}
function getMockUserAssets() {
  return [];
}
function getMockVPNs() {
  return [];
}
function getMockViruss() {
  return [];
}
function getMockVulnerabilitys() {
  return [];
}
function getMockWebAttacks() {
  return [];
}
function getMockWorms() {
  return [];
}
function getMockXSSs() {
  return [];
}
function getMockZeroDayVulnerabilitys() {
  return [];
}
setInterval(() => {
}, 100);

export { ws as default };
//# sourceMappingURL=ws.mjs.map
