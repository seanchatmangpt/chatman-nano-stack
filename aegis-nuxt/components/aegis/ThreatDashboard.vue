<template>
  <div class="threat-dashboard">
    <h2 class="text-2xl font-bold mb-4">🛡️ Threat Detection Dashboard</h2>

    <!-- Real-time threat map -->
    <div class="threat-map mb-6">
      <ClientOnly>
        <ThreatMap
          :threats="activethreats"
          :update-interval="100"
          @threat-selected="onThreatSelected"
        />
      </ClientOnly>
    </div>

    <!-- Threat statistics -->
    <div class="grid grid-cols-4 gap-4 mb-6">
      <StatCard label="Total Threats" :value="threatStats.total" color="blue" />
      <StatCard label="Critical" :value="threatStats.critical" color="red" />
      <StatCard label="High" :value="threatStats.high" color="yellow" />
      <StatCard
        label="Neutralized"
        :value="threatStats.neutralized"
        color="green"
      />
    </div>

    <!-- Active threats list -->
    <div class="active-threats">
      <h3 class="text-xl font-semibold mb-3">Active Threats</h3>
      <div class="space-y-2">
        <ThreatCard
          v-for="threat in attacks"
          :key="threat.id"
          :threat="threat"
          @neutralize="neutralizeThreat"
        />
        <ThreatCard
          v-for="threat in ddoSattacks"
          :key="threat.id"
          :threat="threat"
          @neutralize="neutralizeThreat"
        />
        <ThreatCard
          v-for="threat in manInTheMiddleAttacks"
          :key="threat.id"
          :threat="threat"
          @neutralize="neutralizeThreat"
        />
        <ThreatCard
          v-for="threat in networkAttacks"
          :key="threat.id"
          :threat="threat"
          @neutralize="neutralizeThreat"
        />
        <ThreatCard
          v-for="threat in phishingAttacks"
          :key="threat.id"
          :threat="threat"
          @neutralize="neutralizeThreat"
        />
        <ThreatCard
          v-for="threat in threats"
          :key="threat.id"
          :threat="threat"
          @neutralize="neutralizeThreat"
        />
        <ThreatCard
          v-for="threat in threatIntelligences"
          :key="threat.id"
          :threat="threat"
          @neutralize="neutralizeThreat"
        />
        <ThreatCard
          v-for="threat in webAttacks"
          :key="threat.id"
          :threat="threat"
          @neutralize="neutralizeThreat"
        />
      </div>
    </div>
  </div>
</template>

<script setup lang="js">
// Generated from TTL ontology on 2025-07-24T20:14:09.966068
import { useAegisFabric } from "~/composables/useAegisFabric";
import type { Threat, ThreatStats } from "~/types/aegis";

const {
  activethreats,
  threatStats,
  neutralizeThreat,
  attacks,
  ddoSattacks,
  manInTheMiddleAttacks,
  networkAttacks,
  phishingAttacks,
  threats,
  threatIntelligences,
  webAttacks,
} = useAegisFabric();

const onThreatSelected = (threat: Threat) => {
  console.log("Threat selected:", threat);
  // Handle threat selection
};
</script>

<style scoped>
.threat-dashboard {
  @apply p-6 bg-gray-900 text-white rounded-lg;
}

.threat-map {
  @apply h-96 bg-gray-800 rounded-lg overflow-hidden;
}
</style>
