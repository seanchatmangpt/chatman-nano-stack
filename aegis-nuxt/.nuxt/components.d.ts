
import type { DefineComponent, SlotsType } from 'vue'
type IslandComponent<T extends DefineComponent> = T & DefineComponent<{}, {refresh: () => Promise<void>}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, SlotsType<{ fallback: { error: unknown } }>>
type HydrationStrategies = {
  hydrateOnVisible?: IntersectionObserverInit | true
  hydrateOnIdle?: number | true
  hydrateOnInteraction?: keyof HTMLElementEventMap | Array<keyof HTMLElementEventMap> | true
  hydrateOnMediaQuery?: string
  hydrateAfter?: number
  hydrateWhen?: boolean
  hydrateNever?: true
}
type LazyComponent<T> = (T & DefineComponent<HydrationStrategies, {}, {}, {}, {}, {}, {}, { hydrated: () => void }>)
interface _GlobalComponents {
      'StatCard': typeof import("../components/StatCard.vue")['default']
    'ThreatCard': typeof import("../components/ThreatCard.vue")['default']
    'ThreatMap': typeof import("../components/ThreatMap.vue")['default']
    'AegisAlert': typeof import("../components/aegis/Alert.vue")['default']
    'AegisAntivirus': typeof import("../components/aegis/Antivirus.vue")['default']
    'AegisApplicationAsset': typeof import("../components/aegis/ApplicationAsset.vue")['default']
    'AegisAsset': typeof import("../components/aegis/Asset.vue")['default']
    'AegisAssetMonitor': typeof import("../components/aegis/AssetMonitor.vue")['default']
    'AegisAttack': typeof import("../components/aegis/Attack.vue")['default']
    'AegisBotnet': typeof import("../components/aegis/Botnet.vue")['default']
    'AegisCSRF': typeof import("../components/aegis/CSRF.vue")['default']
    'AegisComputeAsset': typeof import("../components/aegis/ComputeAsset.vue")['default']
    'AegisCorrectiveControl': typeof import("../components/aegis/CorrectiveControl.vue")['default']
    'AegisDDoSAttack': typeof import("../components/aegis/DDoSAttack.vue")['default']
    'AegisDMZ': typeof import("../components/aegis/DMZ.vue")['default']
    'AegisDataAsset': typeof import("../components/aegis/DataAsset.vue")['default']
    'AegisDataExfiltration': typeof import("../components/aegis/DataExfiltration.vue")['default']
    'AegisDetectiveControl': typeof import("../components/aegis/DetectiveControl.vue")['default']
    'AegisEDR': typeof import("../components/aegis/EDR.vue")['default']
    'AegisFirewall': typeof import("../components/aegis/Firewall.vue")['default']
    'AegisIDS': typeof import("../components/aegis/IDS.vue")['default']
    'AegisIOC': typeof import("../components/aegis/IOC.vue")['default']
    'AegisIPS': typeof import("../components/aegis/IPS.vue")['default']
    'AegisInternalNetwork': typeof import("../components/aegis/InternalNetwork.vue")['default']
    'AegisLateralMovement': typeof import("../components/aegis/LateralMovement.vue")['default']
    'AegisLoadBalancer': typeof import("../components/aegis/LoadBalancer.vue")['default']
    'AegisMalware': typeof import("../components/aegis/Malware.vue")['default']
    'AegisManInTheMiddleAttack': typeof import("../components/aegis/ManInTheMiddleAttack.vue")['default']
    'AegisNetworkAsset': typeof import("../components/aegis/NetworkAsset.vue")['default']
    'AegisNetworkAttack': typeof import("../components/aegis/NetworkAttack.vue")['default']
    'AegisNetworkDevice': typeof import("../components/aegis/NetworkDevice.vue")['default']
    'AegisNetworkSegment': typeof import("../components/aegis/NetworkSegment.vue")['default']
    'AegisNetworkTopology': typeof import("../components/aegis/NetworkTopology.vue")['default']
    'AegisPacketSniffing': typeof import("../components/aegis/PacketSniffing.vue")['default']
    'AegisPhishingAttack': typeof import("../components/aegis/PhishingAttack.vue")['default']
    'AegisPortScan': typeof import("../components/aegis/PortScan.vue")['default']
    'AegisPreventiveControl': typeof import("../components/aegis/PreventiveControl.vue")['default']
    'AegisPrivilegeEscalation': typeof import("../components/aegis/PrivilegeEscalation.vue")['default']
    'AegisPublicNetwork': typeof import("../components/aegis/PublicNetwork.vue")['default']
    'AegisRansomware': typeof import("../components/aegis/Ransomware.vue")['default']
    'AegisRootkit': typeof import("../components/aegis/Rootkit.vue")['default']
    'AegisRouter': typeof import("../components/aegis/Router.vue")['default']
    'AegisSIEM': typeof import("../components/aegis/SIEM.vue")['default']
    'AegisSOAR': typeof import("../components/aegis/SOAR.vue")['default']
    'AegisSQLInjection': typeof import("../components/aegis/SQLInjection.vue")['default']
    'AegisSecurityControl': typeof import("../components/aegis/SecurityControl.vue")['default']
    'AegisSecurityEvent': typeof import("../components/aegis/SecurityEvent.vue")['default']
    'AegisSecurityIncident': typeof import("../components/aegis/SecurityIncident.vue")['default']
    'AegisSocialEngineering': typeof import("../components/aegis/SocialEngineering.vue")['default']
    'AegisSpearPhishing': typeof import("../components/aegis/SpearPhishing.vue")['default']
    'AegisSpyware': typeof import("../components/aegis/Spyware.vue")['default']
    'AegisSwitch': typeof import("../components/aegis/Switch.vue")['default']
    'AegisThreat': typeof import("../components/aegis/Threat.vue")['default']
    'AegisThreatDashboard': typeof import("../components/aegis/ThreatDashboard.vue")['default']
    'AegisThreatIntelligence': typeof import("../components/aegis/ThreatIntelligence.vue")['default']
    'AegisTrojan': typeof import("../components/aegis/Trojan.vue")['default']
    'AegisUserAsset': typeof import("../components/aegis/UserAsset.vue")['default']
    'AegisVPN': typeof import("../components/aegis/VPN.vue")['default']
    'AegisVirus': typeof import("../components/aegis/Virus.vue")['default']
    'AegisVulnerability': typeof import("../components/aegis/Vulnerability.vue")['default']
    'AegisWebAttack': typeof import("../components/aegis/WebAttack.vue")['default']
    'AegisWorm': typeof import("../components/aegis/Worm.vue")['default']
    'AegisXSS': typeof import("../components/aegis/XSS.vue")['default']
    'AegisZeroDayVulnerability': typeof import("../components/aegis/ZeroDayVulnerability.vue")['default']
    'NuxtWelcome': typeof import("../node_modules/nuxt/dist/app/components/welcome.vue")['default']
    'NuxtLayout': typeof import("../node_modules/nuxt/dist/app/components/nuxt-layout")['default']
    'NuxtErrorBoundary': typeof import("../node_modules/nuxt/dist/app/components/nuxt-error-boundary.vue")['default']
    'ClientOnly': typeof import("../node_modules/nuxt/dist/app/components/client-only")['default']
    'DevOnly': typeof import("../node_modules/nuxt/dist/app/components/dev-only")['default']
    'ServerPlaceholder': typeof import("../node_modules/nuxt/dist/app/components/server-placeholder")['default']
    'NuxtLink': typeof import("../node_modules/nuxt/dist/app/components/nuxt-link")['default']
    'NuxtLoadingIndicator': typeof import("../node_modules/nuxt/dist/app/components/nuxt-loading-indicator")['default']
    'NuxtTime': typeof import("../node_modules/nuxt/dist/app/components/nuxt-time.vue")['default']
    'NuxtRouteAnnouncer': typeof import("../node_modules/nuxt/dist/app/components/nuxt-route-announcer")['default']
    'NuxtImg': typeof import("../node_modules/nuxt/dist/app/components/nuxt-stubs")['NuxtImg']
    'NuxtPicture': typeof import("../node_modules/nuxt/dist/app/components/nuxt-stubs")['NuxtPicture']
    'NuxtPage': typeof import("../node_modules/nuxt/dist/pages/runtime/page-placeholder")['default']
    'NoScript': typeof import("../node_modules/nuxt/dist/head/runtime/components")['NoScript']
    'Link': typeof import("../node_modules/nuxt/dist/head/runtime/components")['Link']
    'Base': typeof import("../node_modules/nuxt/dist/head/runtime/components")['Base']
    'Title': typeof import("../node_modules/nuxt/dist/head/runtime/components")['Title']
    'Meta': typeof import("../node_modules/nuxt/dist/head/runtime/components")['Meta']
    'Style': typeof import("../node_modules/nuxt/dist/head/runtime/components")['Style']
    'Head': typeof import("../node_modules/nuxt/dist/head/runtime/components")['Head']
    'Html': typeof import("../node_modules/nuxt/dist/head/runtime/components")['Html']
    'Body': typeof import("../node_modules/nuxt/dist/head/runtime/components")['Body']
    'NuxtIsland': typeof import("../node_modules/nuxt/dist/app/components/nuxt-island")['default']
    'NuxtRouteAnnouncer': typeof import("../node_modules/nuxt/dist/app/components/server-placeholder")['default']
      'LazyStatCard': LazyComponent<typeof import("../components/StatCard.vue")['default']>
    'LazyThreatCard': LazyComponent<typeof import("../components/ThreatCard.vue")['default']>
    'LazyThreatMap': LazyComponent<typeof import("../components/ThreatMap.vue")['default']>
    'LazyAegisAlert': LazyComponent<typeof import("../components/aegis/Alert.vue")['default']>
    'LazyAegisAntivirus': LazyComponent<typeof import("../components/aegis/Antivirus.vue")['default']>
    'LazyAegisApplicationAsset': LazyComponent<typeof import("../components/aegis/ApplicationAsset.vue")['default']>
    'LazyAegisAsset': LazyComponent<typeof import("../components/aegis/Asset.vue")['default']>
    'LazyAegisAssetMonitor': LazyComponent<typeof import("../components/aegis/AssetMonitor.vue")['default']>
    'LazyAegisAttack': LazyComponent<typeof import("../components/aegis/Attack.vue")['default']>
    'LazyAegisBotnet': LazyComponent<typeof import("../components/aegis/Botnet.vue")['default']>
    'LazyAegisCSRF': LazyComponent<typeof import("../components/aegis/CSRF.vue")['default']>
    'LazyAegisComputeAsset': LazyComponent<typeof import("../components/aegis/ComputeAsset.vue")['default']>
    'LazyAegisCorrectiveControl': LazyComponent<typeof import("../components/aegis/CorrectiveControl.vue")['default']>
    'LazyAegisDDoSAttack': LazyComponent<typeof import("../components/aegis/DDoSAttack.vue")['default']>
    'LazyAegisDMZ': LazyComponent<typeof import("../components/aegis/DMZ.vue")['default']>
    'LazyAegisDataAsset': LazyComponent<typeof import("../components/aegis/DataAsset.vue")['default']>
    'LazyAegisDataExfiltration': LazyComponent<typeof import("../components/aegis/DataExfiltration.vue")['default']>
    'LazyAegisDetectiveControl': LazyComponent<typeof import("../components/aegis/DetectiveControl.vue")['default']>
    'LazyAegisEDR': LazyComponent<typeof import("../components/aegis/EDR.vue")['default']>
    'LazyAegisFirewall': LazyComponent<typeof import("../components/aegis/Firewall.vue")['default']>
    'LazyAegisIDS': LazyComponent<typeof import("../components/aegis/IDS.vue")['default']>
    'LazyAegisIOC': LazyComponent<typeof import("../components/aegis/IOC.vue")['default']>
    'LazyAegisIPS': LazyComponent<typeof import("../components/aegis/IPS.vue")['default']>
    'LazyAegisInternalNetwork': LazyComponent<typeof import("../components/aegis/InternalNetwork.vue")['default']>
    'LazyAegisLateralMovement': LazyComponent<typeof import("../components/aegis/LateralMovement.vue")['default']>
    'LazyAegisLoadBalancer': LazyComponent<typeof import("../components/aegis/LoadBalancer.vue")['default']>
    'LazyAegisMalware': LazyComponent<typeof import("../components/aegis/Malware.vue")['default']>
    'LazyAegisManInTheMiddleAttack': LazyComponent<typeof import("../components/aegis/ManInTheMiddleAttack.vue")['default']>
    'LazyAegisNetworkAsset': LazyComponent<typeof import("../components/aegis/NetworkAsset.vue")['default']>
    'LazyAegisNetworkAttack': LazyComponent<typeof import("../components/aegis/NetworkAttack.vue")['default']>
    'LazyAegisNetworkDevice': LazyComponent<typeof import("../components/aegis/NetworkDevice.vue")['default']>
    'LazyAegisNetworkSegment': LazyComponent<typeof import("../components/aegis/NetworkSegment.vue")['default']>
    'LazyAegisNetworkTopology': LazyComponent<typeof import("../components/aegis/NetworkTopology.vue")['default']>
    'LazyAegisPacketSniffing': LazyComponent<typeof import("../components/aegis/PacketSniffing.vue")['default']>
    'LazyAegisPhishingAttack': LazyComponent<typeof import("../components/aegis/PhishingAttack.vue")['default']>
    'LazyAegisPortScan': LazyComponent<typeof import("../components/aegis/PortScan.vue")['default']>
    'LazyAegisPreventiveControl': LazyComponent<typeof import("../components/aegis/PreventiveControl.vue")['default']>
    'LazyAegisPrivilegeEscalation': LazyComponent<typeof import("../components/aegis/PrivilegeEscalation.vue")['default']>
    'LazyAegisPublicNetwork': LazyComponent<typeof import("../components/aegis/PublicNetwork.vue")['default']>
    'LazyAegisRansomware': LazyComponent<typeof import("../components/aegis/Ransomware.vue")['default']>
    'LazyAegisRootkit': LazyComponent<typeof import("../components/aegis/Rootkit.vue")['default']>
    'LazyAegisRouter': LazyComponent<typeof import("../components/aegis/Router.vue")['default']>
    'LazyAegisSIEM': LazyComponent<typeof import("../components/aegis/SIEM.vue")['default']>
    'LazyAegisSOAR': LazyComponent<typeof import("../components/aegis/SOAR.vue")['default']>
    'LazyAegisSQLInjection': LazyComponent<typeof import("../components/aegis/SQLInjection.vue")['default']>
    'LazyAegisSecurityControl': LazyComponent<typeof import("../components/aegis/SecurityControl.vue")['default']>
    'LazyAegisSecurityEvent': LazyComponent<typeof import("../components/aegis/SecurityEvent.vue")['default']>
    'LazyAegisSecurityIncident': LazyComponent<typeof import("../components/aegis/SecurityIncident.vue")['default']>
    'LazyAegisSocialEngineering': LazyComponent<typeof import("../components/aegis/SocialEngineering.vue")['default']>
    'LazyAegisSpearPhishing': LazyComponent<typeof import("../components/aegis/SpearPhishing.vue")['default']>
    'LazyAegisSpyware': LazyComponent<typeof import("../components/aegis/Spyware.vue")['default']>
    'LazyAegisSwitch': LazyComponent<typeof import("../components/aegis/Switch.vue")['default']>
    'LazyAegisThreat': LazyComponent<typeof import("../components/aegis/Threat.vue")['default']>
    'LazyAegisThreatDashboard': LazyComponent<typeof import("../components/aegis/ThreatDashboard.vue")['default']>
    'LazyAegisThreatIntelligence': LazyComponent<typeof import("../components/aegis/ThreatIntelligence.vue")['default']>
    'LazyAegisTrojan': LazyComponent<typeof import("../components/aegis/Trojan.vue")['default']>
    'LazyAegisUserAsset': LazyComponent<typeof import("../components/aegis/UserAsset.vue")['default']>
    'LazyAegisVPN': LazyComponent<typeof import("../components/aegis/VPN.vue")['default']>
    'LazyAegisVirus': LazyComponent<typeof import("../components/aegis/Virus.vue")['default']>
    'LazyAegisVulnerability': LazyComponent<typeof import("../components/aegis/Vulnerability.vue")['default']>
    'LazyAegisWebAttack': LazyComponent<typeof import("../components/aegis/WebAttack.vue")['default']>
    'LazyAegisWorm': LazyComponent<typeof import("../components/aegis/Worm.vue")['default']>
    'LazyAegisXSS': LazyComponent<typeof import("../components/aegis/XSS.vue")['default']>
    'LazyAegisZeroDayVulnerability': LazyComponent<typeof import("../components/aegis/ZeroDayVulnerability.vue")['default']>
    'LazyNuxtWelcome': LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/welcome.vue")['default']>
    'LazyNuxtLayout': LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/nuxt-layout")['default']>
    'LazyNuxtErrorBoundary': LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/nuxt-error-boundary.vue")['default']>
    'LazyClientOnly': LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/client-only")['default']>
    'LazyDevOnly': LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/dev-only")['default']>
    'LazyServerPlaceholder': LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/server-placeholder")['default']>
    'LazyNuxtLink': LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/nuxt-link")['default']>
    'LazyNuxtLoadingIndicator': LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/nuxt-loading-indicator")['default']>
    'LazyNuxtTime': LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/nuxt-time.vue")['default']>
    'LazyNuxtRouteAnnouncer': LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/nuxt-route-announcer")['default']>
    'LazyNuxtImg': LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/nuxt-stubs")['NuxtImg']>
    'LazyNuxtPicture': LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/nuxt-stubs")['NuxtPicture']>
    'LazyNuxtPage': LazyComponent<typeof import("../node_modules/nuxt/dist/pages/runtime/page-placeholder")['default']>
    'LazyNoScript': LazyComponent<typeof import("../node_modules/nuxt/dist/head/runtime/components")['NoScript']>
    'LazyLink': LazyComponent<typeof import("../node_modules/nuxt/dist/head/runtime/components")['Link']>
    'LazyBase': LazyComponent<typeof import("../node_modules/nuxt/dist/head/runtime/components")['Base']>
    'LazyTitle': LazyComponent<typeof import("../node_modules/nuxt/dist/head/runtime/components")['Title']>
    'LazyMeta': LazyComponent<typeof import("../node_modules/nuxt/dist/head/runtime/components")['Meta']>
    'LazyStyle': LazyComponent<typeof import("../node_modules/nuxt/dist/head/runtime/components")['Style']>
    'LazyHead': LazyComponent<typeof import("../node_modules/nuxt/dist/head/runtime/components")['Head']>
    'LazyHtml': LazyComponent<typeof import("../node_modules/nuxt/dist/head/runtime/components")['Html']>
    'LazyBody': LazyComponent<typeof import("../node_modules/nuxt/dist/head/runtime/components")['Body']>
    'LazyNuxtIsland': LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/nuxt-island")['default']>
    'LazyNuxtRouteAnnouncer': LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/server-placeholder")['default']>
}

declare module 'vue' {
  export interface GlobalComponents extends _GlobalComponents { }
}

export const StatCard: typeof import("../components/StatCard.vue")['default']
export const ThreatCard: typeof import("../components/ThreatCard.vue")['default']
export const ThreatMap: typeof import("../components/ThreatMap.vue")['default']
export const AegisAlert: typeof import("../components/aegis/Alert.vue")['default']
export const AegisAntivirus: typeof import("../components/aegis/Antivirus.vue")['default']
export const AegisApplicationAsset: typeof import("../components/aegis/ApplicationAsset.vue")['default']
export const AegisAsset: typeof import("../components/aegis/Asset.vue")['default']
export const AegisAssetMonitor: typeof import("../components/aegis/AssetMonitor.vue")['default']
export const AegisAttack: typeof import("../components/aegis/Attack.vue")['default']
export const AegisBotnet: typeof import("../components/aegis/Botnet.vue")['default']
export const AegisCSRF: typeof import("../components/aegis/CSRF.vue")['default']
export const AegisComputeAsset: typeof import("../components/aegis/ComputeAsset.vue")['default']
export const AegisCorrectiveControl: typeof import("../components/aegis/CorrectiveControl.vue")['default']
export const AegisDDoSAttack: typeof import("../components/aegis/DDoSAttack.vue")['default']
export const AegisDMZ: typeof import("../components/aegis/DMZ.vue")['default']
export const AegisDataAsset: typeof import("../components/aegis/DataAsset.vue")['default']
export const AegisDataExfiltration: typeof import("../components/aegis/DataExfiltration.vue")['default']
export const AegisDetectiveControl: typeof import("../components/aegis/DetectiveControl.vue")['default']
export const AegisEDR: typeof import("../components/aegis/EDR.vue")['default']
export const AegisFirewall: typeof import("../components/aegis/Firewall.vue")['default']
export const AegisIDS: typeof import("../components/aegis/IDS.vue")['default']
export const AegisIOC: typeof import("../components/aegis/IOC.vue")['default']
export const AegisIPS: typeof import("../components/aegis/IPS.vue")['default']
export const AegisInternalNetwork: typeof import("../components/aegis/InternalNetwork.vue")['default']
export const AegisLateralMovement: typeof import("../components/aegis/LateralMovement.vue")['default']
export const AegisLoadBalancer: typeof import("../components/aegis/LoadBalancer.vue")['default']
export const AegisMalware: typeof import("../components/aegis/Malware.vue")['default']
export const AegisManInTheMiddleAttack: typeof import("../components/aegis/ManInTheMiddleAttack.vue")['default']
export const AegisNetworkAsset: typeof import("../components/aegis/NetworkAsset.vue")['default']
export const AegisNetworkAttack: typeof import("../components/aegis/NetworkAttack.vue")['default']
export const AegisNetworkDevice: typeof import("../components/aegis/NetworkDevice.vue")['default']
export const AegisNetworkSegment: typeof import("../components/aegis/NetworkSegment.vue")['default']
export const AegisNetworkTopology: typeof import("../components/aegis/NetworkTopology.vue")['default']
export const AegisPacketSniffing: typeof import("../components/aegis/PacketSniffing.vue")['default']
export const AegisPhishingAttack: typeof import("../components/aegis/PhishingAttack.vue")['default']
export const AegisPortScan: typeof import("../components/aegis/PortScan.vue")['default']
export const AegisPreventiveControl: typeof import("../components/aegis/PreventiveControl.vue")['default']
export const AegisPrivilegeEscalation: typeof import("../components/aegis/PrivilegeEscalation.vue")['default']
export const AegisPublicNetwork: typeof import("../components/aegis/PublicNetwork.vue")['default']
export const AegisRansomware: typeof import("../components/aegis/Ransomware.vue")['default']
export const AegisRootkit: typeof import("../components/aegis/Rootkit.vue")['default']
export const AegisRouter: typeof import("../components/aegis/Router.vue")['default']
export const AegisSIEM: typeof import("../components/aegis/SIEM.vue")['default']
export const AegisSOAR: typeof import("../components/aegis/SOAR.vue")['default']
export const AegisSQLInjection: typeof import("../components/aegis/SQLInjection.vue")['default']
export const AegisSecurityControl: typeof import("../components/aegis/SecurityControl.vue")['default']
export const AegisSecurityEvent: typeof import("../components/aegis/SecurityEvent.vue")['default']
export const AegisSecurityIncident: typeof import("../components/aegis/SecurityIncident.vue")['default']
export const AegisSocialEngineering: typeof import("../components/aegis/SocialEngineering.vue")['default']
export const AegisSpearPhishing: typeof import("../components/aegis/SpearPhishing.vue")['default']
export const AegisSpyware: typeof import("../components/aegis/Spyware.vue")['default']
export const AegisSwitch: typeof import("../components/aegis/Switch.vue")['default']
export const AegisThreat: typeof import("../components/aegis/Threat.vue")['default']
export const AegisThreatDashboard: typeof import("../components/aegis/ThreatDashboard.vue")['default']
export const AegisThreatIntelligence: typeof import("../components/aegis/ThreatIntelligence.vue")['default']
export const AegisTrojan: typeof import("../components/aegis/Trojan.vue")['default']
export const AegisUserAsset: typeof import("../components/aegis/UserAsset.vue")['default']
export const AegisVPN: typeof import("../components/aegis/VPN.vue")['default']
export const AegisVirus: typeof import("../components/aegis/Virus.vue")['default']
export const AegisVulnerability: typeof import("../components/aegis/Vulnerability.vue")['default']
export const AegisWebAttack: typeof import("../components/aegis/WebAttack.vue")['default']
export const AegisWorm: typeof import("../components/aegis/Worm.vue")['default']
export const AegisXSS: typeof import("../components/aegis/XSS.vue")['default']
export const AegisZeroDayVulnerability: typeof import("../components/aegis/ZeroDayVulnerability.vue")['default']
export const NuxtWelcome: typeof import("../node_modules/nuxt/dist/app/components/welcome.vue")['default']
export const NuxtLayout: typeof import("../node_modules/nuxt/dist/app/components/nuxt-layout")['default']
export const NuxtErrorBoundary: typeof import("../node_modules/nuxt/dist/app/components/nuxt-error-boundary.vue")['default']
export const ClientOnly: typeof import("../node_modules/nuxt/dist/app/components/client-only")['default']
export const DevOnly: typeof import("../node_modules/nuxt/dist/app/components/dev-only")['default']
export const ServerPlaceholder: typeof import("../node_modules/nuxt/dist/app/components/server-placeholder")['default']
export const NuxtLink: typeof import("../node_modules/nuxt/dist/app/components/nuxt-link")['default']
export const NuxtLoadingIndicator: typeof import("../node_modules/nuxt/dist/app/components/nuxt-loading-indicator")['default']
export const NuxtTime: typeof import("../node_modules/nuxt/dist/app/components/nuxt-time.vue")['default']
export const NuxtRouteAnnouncer: typeof import("../node_modules/nuxt/dist/app/components/nuxt-route-announcer")['default']
export const NuxtImg: typeof import("../node_modules/nuxt/dist/app/components/nuxt-stubs")['NuxtImg']
export const NuxtPicture: typeof import("../node_modules/nuxt/dist/app/components/nuxt-stubs")['NuxtPicture']
export const NuxtPage: typeof import("../node_modules/nuxt/dist/pages/runtime/page-placeholder")['default']
export const NoScript: typeof import("../node_modules/nuxt/dist/head/runtime/components")['NoScript']
export const Link: typeof import("../node_modules/nuxt/dist/head/runtime/components")['Link']
export const Base: typeof import("../node_modules/nuxt/dist/head/runtime/components")['Base']
export const Title: typeof import("../node_modules/nuxt/dist/head/runtime/components")['Title']
export const Meta: typeof import("../node_modules/nuxt/dist/head/runtime/components")['Meta']
export const Style: typeof import("../node_modules/nuxt/dist/head/runtime/components")['Style']
export const Head: typeof import("../node_modules/nuxt/dist/head/runtime/components")['Head']
export const Html: typeof import("../node_modules/nuxt/dist/head/runtime/components")['Html']
export const Body: typeof import("../node_modules/nuxt/dist/head/runtime/components")['Body']
export const NuxtIsland: typeof import("../node_modules/nuxt/dist/app/components/nuxt-island")['default']
export const NuxtRouteAnnouncer: typeof import("../node_modules/nuxt/dist/app/components/server-placeholder")['default']
export const LazyStatCard: LazyComponent<typeof import("../components/StatCard.vue")['default']>
export const LazyThreatCard: LazyComponent<typeof import("../components/ThreatCard.vue")['default']>
export const LazyThreatMap: LazyComponent<typeof import("../components/ThreatMap.vue")['default']>
export const LazyAegisAlert: LazyComponent<typeof import("../components/aegis/Alert.vue")['default']>
export const LazyAegisAntivirus: LazyComponent<typeof import("../components/aegis/Antivirus.vue")['default']>
export const LazyAegisApplicationAsset: LazyComponent<typeof import("../components/aegis/ApplicationAsset.vue")['default']>
export const LazyAegisAsset: LazyComponent<typeof import("../components/aegis/Asset.vue")['default']>
export const LazyAegisAssetMonitor: LazyComponent<typeof import("../components/aegis/AssetMonitor.vue")['default']>
export const LazyAegisAttack: LazyComponent<typeof import("../components/aegis/Attack.vue")['default']>
export const LazyAegisBotnet: LazyComponent<typeof import("../components/aegis/Botnet.vue")['default']>
export const LazyAegisCSRF: LazyComponent<typeof import("../components/aegis/CSRF.vue")['default']>
export const LazyAegisComputeAsset: LazyComponent<typeof import("../components/aegis/ComputeAsset.vue")['default']>
export const LazyAegisCorrectiveControl: LazyComponent<typeof import("../components/aegis/CorrectiveControl.vue")['default']>
export const LazyAegisDDoSAttack: LazyComponent<typeof import("../components/aegis/DDoSAttack.vue")['default']>
export const LazyAegisDMZ: LazyComponent<typeof import("../components/aegis/DMZ.vue")['default']>
export const LazyAegisDataAsset: LazyComponent<typeof import("../components/aegis/DataAsset.vue")['default']>
export const LazyAegisDataExfiltration: LazyComponent<typeof import("../components/aegis/DataExfiltration.vue")['default']>
export const LazyAegisDetectiveControl: LazyComponent<typeof import("../components/aegis/DetectiveControl.vue")['default']>
export const LazyAegisEDR: LazyComponent<typeof import("../components/aegis/EDR.vue")['default']>
export const LazyAegisFirewall: LazyComponent<typeof import("../components/aegis/Firewall.vue")['default']>
export const LazyAegisIDS: LazyComponent<typeof import("../components/aegis/IDS.vue")['default']>
export const LazyAegisIOC: LazyComponent<typeof import("../components/aegis/IOC.vue")['default']>
export const LazyAegisIPS: LazyComponent<typeof import("../components/aegis/IPS.vue")['default']>
export const LazyAegisInternalNetwork: LazyComponent<typeof import("../components/aegis/InternalNetwork.vue")['default']>
export const LazyAegisLateralMovement: LazyComponent<typeof import("../components/aegis/LateralMovement.vue")['default']>
export const LazyAegisLoadBalancer: LazyComponent<typeof import("../components/aegis/LoadBalancer.vue")['default']>
export const LazyAegisMalware: LazyComponent<typeof import("../components/aegis/Malware.vue")['default']>
export const LazyAegisManInTheMiddleAttack: LazyComponent<typeof import("../components/aegis/ManInTheMiddleAttack.vue")['default']>
export const LazyAegisNetworkAsset: LazyComponent<typeof import("../components/aegis/NetworkAsset.vue")['default']>
export const LazyAegisNetworkAttack: LazyComponent<typeof import("../components/aegis/NetworkAttack.vue")['default']>
export const LazyAegisNetworkDevice: LazyComponent<typeof import("../components/aegis/NetworkDevice.vue")['default']>
export const LazyAegisNetworkSegment: LazyComponent<typeof import("../components/aegis/NetworkSegment.vue")['default']>
export const LazyAegisNetworkTopology: LazyComponent<typeof import("../components/aegis/NetworkTopology.vue")['default']>
export const LazyAegisPacketSniffing: LazyComponent<typeof import("../components/aegis/PacketSniffing.vue")['default']>
export const LazyAegisPhishingAttack: LazyComponent<typeof import("../components/aegis/PhishingAttack.vue")['default']>
export const LazyAegisPortScan: LazyComponent<typeof import("../components/aegis/PortScan.vue")['default']>
export const LazyAegisPreventiveControl: LazyComponent<typeof import("../components/aegis/PreventiveControl.vue")['default']>
export const LazyAegisPrivilegeEscalation: LazyComponent<typeof import("../components/aegis/PrivilegeEscalation.vue")['default']>
export const LazyAegisPublicNetwork: LazyComponent<typeof import("../components/aegis/PublicNetwork.vue")['default']>
export const LazyAegisRansomware: LazyComponent<typeof import("../components/aegis/Ransomware.vue")['default']>
export const LazyAegisRootkit: LazyComponent<typeof import("../components/aegis/Rootkit.vue")['default']>
export const LazyAegisRouter: LazyComponent<typeof import("../components/aegis/Router.vue")['default']>
export const LazyAegisSIEM: LazyComponent<typeof import("../components/aegis/SIEM.vue")['default']>
export const LazyAegisSOAR: LazyComponent<typeof import("../components/aegis/SOAR.vue")['default']>
export const LazyAegisSQLInjection: LazyComponent<typeof import("../components/aegis/SQLInjection.vue")['default']>
export const LazyAegisSecurityControl: LazyComponent<typeof import("../components/aegis/SecurityControl.vue")['default']>
export const LazyAegisSecurityEvent: LazyComponent<typeof import("../components/aegis/SecurityEvent.vue")['default']>
export const LazyAegisSecurityIncident: LazyComponent<typeof import("../components/aegis/SecurityIncident.vue")['default']>
export const LazyAegisSocialEngineering: LazyComponent<typeof import("../components/aegis/SocialEngineering.vue")['default']>
export const LazyAegisSpearPhishing: LazyComponent<typeof import("../components/aegis/SpearPhishing.vue")['default']>
export const LazyAegisSpyware: LazyComponent<typeof import("../components/aegis/Spyware.vue")['default']>
export const LazyAegisSwitch: LazyComponent<typeof import("../components/aegis/Switch.vue")['default']>
export const LazyAegisThreat: LazyComponent<typeof import("../components/aegis/Threat.vue")['default']>
export const LazyAegisThreatDashboard: LazyComponent<typeof import("../components/aegis/ThreatDashboard.vue")['default']>
export const LazyAegisThreatIntelligence: LazyComponent<typeof import("../components/aegis/ThreatIntelligence.vue")['default']>
export const LazyAegisTrojan: LazyComponent<typeof import("../components/aegis/Trojan.vue")['default']>
export const LazyAegisUserAsset: LazyComponent<typeof import("../components/aegis/UserAsset.vue")['default']>
export const LazyAegisVPN: LazyComponent<typeof import("../components/aegis/VPN.vue")['default']>
export const LazyAegisVirus: LazyComponent<typeof import("../components/aegis/Virus.vue")['default']>
export const LazyAegisVulnerability: LazyComponent<typeof import("../components/aegis/Vulnerability.vue")['default']>
export const LazyAegisWebAttack: LazyComponent<typeof import("../components/aegis/WebAttack.vue")['default']>
export const LazyAegisWorm: LazyComponent<typeof import("../components/aegis/Worm.vue")['default']>
export const LazyAegisXSS: LazyComponent<typeof import("../components/aegis/XSS.vue")['default']>
export const LazyAegisZeroDayVulnerability: LazyComponent<typeof import("../components/aegis/ZeroDayVulnerability.vue")['default']>
export const LazyNuxtWelcome: LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/welcome.vue")['default']>
export const LazyNuxtLayout: LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/nuxt-layout")['default']>
export const LazyNuxtErrorBoundary: LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/nuxt-error-boundary.vue")['default']>
export const LazyClientOnly: LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/client-only")['default']>
export const LazyDevOnly: LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/dev-only")['default']>
export const LazyServerPlaceholder: LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/server-placeholder")['default']>
export const LazyNuxtLink: LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/nuxt-link")['default']>
export const LazyNuxtLoadingIndicator: LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/nuxt-loading-indicator")['default']>
export const LazyNuxtTime: LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/nuxt-time.vue")['default']>
export const LazyNuxtRouteAnnouncer: LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/nuxt-route-announcer")['default']>
export const LazyNuxtImg: LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/nuxt-stubs")['NuxtImg']>
export const LazyNuxtPicture: LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/nuxt-stubs")['NuxtPicture']>
export const LazyNuxtPage: LazyComponent<typeof import("../node_modules/nuxt/dist/pages/runtime/page-placeholder")['default']>
export const LazyNoScript: LazyComponent<typeof import("../node_modules/nuxt/dist/head/runtime/components")['NoScript']>
export const LazyLink: LazyComponent<typeof import("../node_modules/nuxt/dist/head/runtime/components")['Link']>
export const LazyBase: LazyComponent<typeof import("../node_modules/nuxt/dist/head/runtime/components")['Base']>
export const LazyTitle: LazyComponent<typeof import("../node_modules/nuxt/dist/head/runtime/components")['Title']>
export const LazyMeta: LazyComponent<typeof import("../node_modules/nuxt/dist/head/runtime/components")['Meta']>
export const LazyStyle: LazyComponent<typeof import("../node_modules/nuxt/dist/head/runtime/components")['Style']>
export const LazyHead: LazyComponent<typeof import("../node_modules/nuxt/dist/head/runtime/components")['Head']>
export const LazyHtml: LazyComponent<typeof import("../node_modules/nuxt/dist/head/runtime/components")['Html']>
export const LazyBody: LazyComponent<typeof import("../node_modules/nuxt/dist/head/runtime/components")['Body']>
export const LazyNuxtIsland: LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/nuxt-island")['default']>
export const LazyNuxtRouteAnnouncer: LazyComponent<typeof import("../node_modules/nuxt/dist/app/components/server-placeholder")['default']>

export const componentNames: string[]
