<template>
  <div class="threat-map cyber-card">
    <h3 class="text-lg font-semibold mb-4 text-cyber-blue">🌐 Threat Map</h3>

    <!-- Map Container -->
    <div
      class="relative h-80 bg-cyber-dark border border-cyber-blue/20 rounded-lg overflow-hidden"
    >
      <!-- Grid Background -->
      <div class="absolute inset-0 opacity-10">
        <svg width="100%" height="100%">
          <defs>
            <pattern
              id="grid"
              width="40"
              height="40"
              patternUnits="userSpaceOnUse"
            >
              <path
                d="M 40 0 L 0 0 0 40"
                fill="none"
                stroke="#00d4ff"
                stroke-width="1"
              />
            </pattern>
          </defs>
          <rect width="100%" height="100%" fill="url(#grid)" />
        </svg>
      </div>

      <!-- Threat Nodes -->
      <div
        v-for="threat in threats"
        :key="threat.id"
        class="absolute w-3 h-3 rounded-full cursor-pointer transition-all duration-300"
        :class="getThreatClass(threat)"
        :style="getThreatPosition(threat)"
        @click="$emit('threat-selected', threat)"
        :title="`${threat.label} - ${threat.severity}`"
      >
        <div
          class="w-full h-full rounded-full animate-ping"
          :class="getThreatPingClass(threat)"
        ></div>
      </div>

      <!-- Legend -->
      <div
        class="absolute bottom-4 left-4 bg-cyber-gray/90 backdrop-blur-sm rounded-lg p-3 border border-cyber-blue/20"
      >
        <div class="text-xs space-y-1">
          <div class="flex items-center space-x-2">
            <div class="w-2 h-2 rounded-full bg-cyber-red"></div>
            <span>Critical</span>
          </div>
          <div class="flex items-center space-x-2">
            <div class="w-2 h-2 rounded-full bg-orange-600"></div>
            <span>High</span>
          </div>
          <div class="flex items-center space-x-2">
            <div class="w-2 h-2 rounded-full bg-cyber-yellow"></div>
            <span>Medium</span>
          </div>
          <div class="flex items-center space-x-2">
            <div class="w-2 h-2 rounded-full bg-cyber-green"></div>
            <span>Low</span>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup lang="js">
import type { Threat } from "~/types/aegis";

interface Props {
  threats: (Threat | any)[];
  updateInterval?: number;
}

interface Emits {
  (e: "threat-selected", threat: Threat): void;
}

const props = withDefaults(defineProps<Props>(), {
  updateInterval: 100,
});

const emit = defineEmits<Emits>();

const getThreatClass = (threat: Threat) => {
  switch (threat.severity) {
    case "critical":
      return "bg-cyber-red border-cyber-red";
    case "high":
      return "bg-orange-600 border-orange-600";
    case "medium":
      return "bg-cyber-yellow border-cyber-yellow";
    case "low":
      return "bg-cyber-green border-cyber-green";
    default:
      return "bg-gray-500 border-gray-500";
  }
};

const getThreatPingClass = (threat: Threat) => {
  switch (threat.severity) {
    case "critical":
      return "bg-cyber-red";
    case "high":
      return "bg-orange-600";
    case "medium":
      return "bg-cyber-yellow";
    case "low":
      return "bg-cyber-green";
    default:
      return "bg-gray-500";
  }
};

const getThreatPosition = (threat: Threat) => {
  // Generate pseudo-random but consistent positions based on threat ID
  const hash = threat.id.split("").reduce((a, b) => {
    a = (a << 5) - a + b.charCodeAt(0);
    return a & a;
  }, 0);

  const x = (Math.abs(hash) % 80) + 10; // 10-90%
  const y = (Math.abs(hash * 2) % 70) + 15; // 15-85%

  return {
    left: `${x}%`,
    top: `${y}%`,
  };
};
</script>
