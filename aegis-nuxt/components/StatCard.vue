<template>
  <div class="stat-card cyber-card">
    <div class="flex items-center justify-between">
      <div>
        <p class="text-sm text-gray-400">{{ label }}</p>
        <p class="text-2xl font-bold" :class="getValueColor()">{{ value }}</p>
      </div>
      <div class="flex items-center space-x-2">
        <div v-if="trend" class="text-sm" :class="getTrendColor()">
          {{ trend > 0 ? "+" : "" }}{{ trend }}%
        </div>
        <div
          class="w-8 h-8 rounded-full flex items-center justify-center"
          :class="getIconColor()"
        >
          <span class="text-lg">{{ getIcon() }}</span>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
interface Props {
  label: string;
  value: number;
  trend?: number;
  color?: "blue" | "green" | "red" | "yellow" | "purple";
}

const props = withDefaults(defineProps<Props>(), {
  color: "blue",
});

const getValueColor = () => {
  switch (props.color) {
    case "green":
      return "text-cyber-green";
    case "red":
      return "text-cyber-red";
    case "yellow":
      return "text-cyber-yellow";
    case "purple":
      return "text-cyber-purple";
    default:
      return "text-cyber-blue";
  }
};

const getTrendColor = () => {
  if (!props.trend) return "text-gray-400";
  return props.trend > 0 ? "text-cyber-red" : "text-cyber-green";
};

const getIconColor = () => {
  switch (props.color) {
    case "green":
      return "bg-cyber-green/20 text-cyber-green";
    case "red":
      return "bg-cyber-red/20 text-cyber-red";
    case "yellow":
      return "bg-cyber-yellow/20 text-cyber-yellow";
    case "purple":
      return "bg-cyber-purple/20 text-cyber-purple";
    default:
      return "bg-cyber-blue/20 text-cyber-blue";
  }
};

const getIcon = () => {
  const icons = {
    total: "📊",
    critical: "🚨",
    high: "⚠️",
    medium: "⚡",
    low: "ℹ️",
    neutralized: "✅",
  };

  const key = props.label.toLowerCase().replace(/\s+/g, "");
  return icons[key as keyof typeof icons] || "📈";
};
</script>
