<template>
  <div class="threat-card cyber-card" :class="getThreatCardClass()">
    <div class="flex items-center justify-between">
      <div class="flex-1">
        <div class="flex items-center space-x-3">
          <div
            class="w-3 h-3 rounded-full"
            :class="getThreatIndicatorClass()"
          ></div>
          <div>
            <h4 class="font-semibold text-white">{{ threat.label }}</h4>
            <p class="text-sm text-gray-400">
              {{ threat.threatID || threat.id }}
            </p>
          </div>
        </div>

        <div class="mt-2 flex items-center space-x-4 text-xs">
          <span class="px-2 py-1 rounded" :class="getSeverityClass()">
            {{ threat.severity }}
          </span>
          <span class="text-gray-400">
            {{ formatDate(threat.firstSeen || new Date()) }}
          </span>
          <span v-if="threat.targetType" class="text-cyber-blue">
            {{ threat.targetType }}
          </span>
        </div>
      </div>

      <div class="flex items-center space-x-2">
        <button
          v-if="threat.status === 'active'"
          @click="$emit('neutralize', threat.id)"
          class="px-3 py-1 text-xs bg-cyber-green text-black rounded hover:bg-cyber-green/80 transition-colors"
        >
          Neutralize
        </button>
        <span
          v-else
          class="px-3 py-1 text-xs rounded"
          :class="getStatusClass()"
        >
          {{ threat.status }}
        </span>
      </div>
    </div>
  </div>
</template>

<script setup lang="js">
import type { Threat } from "~/types/aegis";

interface Props {
  threat: Threat | any;
}

interface Emits {
  (e: "neutralize", threatId: string): void;
}

const props = defineProps<Props>();
const emit = defineEmits<Emits>();

const getThreatCardClass = () => {
  if (props.threat.status === "neutralized") {
    return "opacity-60";
  }
  return "";
};

const getThreatIndicatorClass = () => {
  switch (props.threat.severity) {
    case "critical":
      return "bg-cyber-red animate-pulse";
    case "high":
      return "bg-orange-600 animate-pulse";
    case "medium":
      return "bg-cyber-yellow";
    case "low":
      return "bg-cyber-green";
    default:
      return "bg-gray-500";
  }
};

const getSeverityClass = () => {
  switch (props.threat.severity) {
    case "critical":
      return "threat-critical";
    case "high":
      return "threat-high";
    case "medium":
      return "threat-medium";
    case "low":
      return "threat-low";
    default:
      return "bg-gray-600 text-white";
  }
};

const getStatusClass = () => {
  switch (props.threat.status) {
    case "neutralized":
      return "bg-cyber-green text-black";
    case "investigating":
      return "bg-cyber-yellow text-black";
    default:
      return "bg-gray-600 text-white";
  }
};

const formatDate = (date: Date) => {
  return new Intl.DateTimeFormat("en-US", {
    month: "short",
    day: "numeric",
    hour: "2-digit",
    minute: "2-digit",
  }).format(date);
};
</script>
