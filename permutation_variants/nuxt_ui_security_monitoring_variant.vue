<!--
BitActor Nuxt UI Security Monitoring Variant
Real-time cybersecurity threat detection and monitoring for BitActor pipeline
Advanced threat intelligence with IoC tracking and incident response
No TypeScript - Pure JavaScript with security analytics
-->

<template>
  <div class="security-monitoring">
    <header class="security-header">
      <h1 class="header-title">
        <span class="title-icon">🛡️</span>
        Security Monitoring Center
      </h1>
      <div class="threat-level-indicator" :class="currentThreatLevel">
        <div class="threat-pulse"></div>
        <span class="threat-text">{{ currentThreatLevel.toUpperCase() }}</span>
      </div>
    </header>

    <!-- Security Alerts Grid -->
    <section class="security-alerts">
      <div class="alerts-grid">
        <div v-for="alert in securityAlerts" :key="alert.id" 
             class="alert-card" :class="alert.severity">
          <div class="alert-header">
            <span class="alert-icon">{{ alert.icon }}</span>
            <span class="alert-title">{{ alert.title }}</span>
            <span class="alert-time">{{ formatTime(alert.timestamp) }}</span>
          </div>
          <div class="alert-details">{{ alert.description }}</div>
          <div class="alert-actions">
            <button @click="investigateAlert(alert)" class="investigate-btn">🔍 Investigate</button>
            <button @click="mitigateAlert(alert)" class="mitigate-btn">🛡️ Mitigate</button>
          </div>
        </div>
      </div>
    </section>

    <!-- Threat Intelligence Dashboard -->
    <section class="threat-intelligence">
      <h2 class="section-title">Threat Intelligence</h2>
      <div class="intelligence-grid">
        <div class="ioc-panel">
          <h3>IoC Detection</h3>
          <div v-for="ioc in activeIOCs" :key="ioc.id" class="ioc-item" :class="ioc.severity">
            <span class="ioc-type">{{ ioc.type }}</span>
            <span class="ioc-value">{{ ioc.value }}</span>
            <span class="ioc-count">{{ ioc.detections }}</span>
          </div>
        </div>
        
        <div class="attack-vectors">
          <h3>Attack Vectors</h3>
          <canvas ref="attackVectorChart" class="vector-chart"></canvas>
        </div>
        
        <div class="pipeline-security">
          <h3>Pipeline Security Status</h3>
          <div v-for="stage in pipelineStages" :key="stage.name" 
               class="stage-security" :class="stage.securityStatus">
            <span class="stage-name">{{ stage.name }}</span>
            <span class="security-score">{{ stage.securityScore }}/100</span>
          </div>
        </div>
      </div>
    </section>
  </div>
</template>

<script>
export default {
  name: 'NuxtUISecurityMonitoringVariant',
  
  data() {
    return {
      currentThreatLevel: 'moderate',
      
      securityAlerts: [
        {
          id: 'alert_001',
          severity: 'critical',
          icon: '🚨',
          title: 'Suspicious BitActor Pattern Detected',
          description: 'Anomalous execution pattern in BitActor stage suggesting potential code injection',
          timestamp: Date.now() - 30000
        },
        {
          id: 'alert_002',
          severity: 'warning',
          icon: '⚠️',
          title: 'Elevated Error Rate in Ash Framework',
          description: 'Unusual increase in validation errors could indicate data tampering',
          timestamp: Date.now() - 120000
        }
      ],
      
      activeIOCs: [
        { id: 'ioc_001', type: 'IP', value: '192.168.1.100', severity: 'high', detections: 15 },
        { id: 'ioc_002', type: 'Hash', value: 'a1b2c3d4...', severity: 'medium', detections: 3 },
        { id: 'ioc_003', type: 'Domain', value: 'malicious.com', severity: 'critical', detections: 8 }
      ],
      
      pipelineStages: [
        { name: 'Typer', securityStatus: 'secure', securityScore: 95 },
        { name: 'Turtle', securityStatus: 'secure', securityScore: 92 },
        { name: 'TTL2DSPy', securityStatus: 'warning', securityScore: 78 },
        { name: 'BitActor', securityStatus: 'critical', securityScore: 45 },
        { name: 'Erlang', securityStatus: 'secure', securityScore: 88 },
        { name: 'Ash', securityStatus: 'warning', securityScore: 72 },
        { name: 'Reactor', securityStatus: 'secure', securityScore: 91 },
        { name: 'K8s', securityStatus: 'secure', securityScore: 96 }
      ]
    }
  },
  
  methods: {
    formatTime(timestamp) {
      return new Date(timestamp).toLocaleTimeString()
    },
    
    investigateAlert(alert) {
      console.log(`Investigating alert: ${alert.title}`)
    },
    
    mitigateAlert(alert) {
      console.log(`Mitigating alert: ${alert.title}`)
    }
  }
}
</script>

<style scoped>
.security-monitoring {
  min-height: 100vh;
  background: linear-gradient(135deg, #1a1a2e 0%, #16213e 50%, #0f3460 100%);
  color: #ffffff;
  font-family: 'Inter', sans-serif;
  padding: 20px;
}

.security-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  background: rgba(255, 255, 255, 0.1);
  backdrop-filter: blur(10px);
  border-radius: 16px;
  padding: 25px;
  margin-bottom: 30px;
}

.header-title {
  font-size: 2.2rem;
  font-weight: 700;
  margin: 0;
  display: flex;
  align-items: center;
  gap: 15px;
}

.threat-level-indicator {
  padding: 12px 24px;
  border-radius: 25px;
  display: flex;
  align-items: center;
  gap: 10px;
  font-weight: 600;
}

.threat-level-indicator.low {
  background: #5cb85c;
}

.threat-level-indicator.moderate {
  background: #f0ad4e;
}

.threat-level-indicator.high {
  background: #d9534f;
}

.threat-pulse {
  width: 12px;
  height: 12px;
  border-radius: 50%;
  background: rgba(255, 255, 255, 0.8);
  animation: pulse 2s infinite;
}

.security-alerts {
  margin-bottom: 30px;
}

.alerts-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(400px, 1fr));
  gap: 20px;
}

.alert-card {
  background: rgba(255, 255, 255, 0.1);
  border-radius: 12px;
  padding: 20px;
  border: 1px solid;
}

.alert-card.critical {
  border-color: #d9534f;
  box-shadow: 0 0 20px rgba(217, 83, 79, 0.3);
}

.alert-card.warning {
  border-color: #f0ad4e;
  box-shadow: 0 0 20px rgba(240, 173, 78, 0.3);
}

.alert-header {
  display: flex;
  align-items: center;
  gap: 10px;
  margin-bottom: 10px;
}

.alert-icon {
  font-size: 1.5rem;
}

.alert-title {
  font-weight: 600;
  flex: 1;
}

.alert-time {
  font-size: 0.8rem;
  opacity: 0.7;
}

.alert-details {
  margin-bottom: 15px;
  opacity: 0.9;
}

.alert-actions {
  display: flex;
  gap: 10px;
}

.investigate-btn, .mitigate-btn {
  padding: 8px 16px;
  border: none;
  border-radius: 6px;
  cursor: pointer;
  font-size: 0.9rem;
  transition: all 0.3s ease;
}

.investigate-btn {
  background: #5bc0de;
  color: white;
}

.mitigate-btn {
  background: #d9534f;
  color: white;
}

.threat-intelligence {
  background: rgba(255, 255, 255, 0.05);
  border-radius: 16px;
  padding: 30px;
}

.section-title {
  font-size: 1.8rem;
  font-weight: 600;
  margin-bottom: 25px;
}

.intelligence-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
  gap: 25px;
}

.ioc-panel, .attack-vectors, .pipeline-security {
  background: rgba(255, 255, 255, 0.1);
  border-radius: 12px;
  padding: 20px;
}

.ioc-panel h3, .attack-vectors h3, .pipeline-security h3 {
  margin-bottom: 15px;
  color: #4a90e2;
}

.ioc-item {
  display: flex;
  justify-content: space-between;
  padding: 8px;
  margin-bottom: 8px;
  border-radius: 6px;
  background: rgba(255, 255, 255, 0.05);
  border-left: 4px solid;
}

.ioc-item.critical {
  border-left-color: #d9534f;
}

.ioc-item.high {
  border-left-color: #f0ad4e;
}

.ioc-item.medium {
  border-left-color: #5bc0de;
}

.ioc-type {
  font-weight: 600;
  font-size: 0.8rem;
}

.ioc-value {
  font-family: monospace;
  font-size: 0.8rem;
}

.ioc-count {
  font-weight: 600;
  color: #d9534f;
}

.vector-chart {
  width: 100%;
  height: 200px;
  background: rgba(0, 0, 0, 0.2);
  border-radius: 8px;
}

.stage-security {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 10px;
  margin-bottom: 8px;
  border-radius: 6px;
  background: rgba(255, 255, 255, 0.05);
  border-left: 4px solid;
}

.stage-security.secure {
  border-left-color: #5cb85c;
}

.stage-security.warning {
  border-left-color: #f0ad4e;
}

.stage-security.critical {
  border-left-color: #d9534f;
}

.stage-name {
  font-weight: 500;
}

.security-score {
  font-weight: 600;
  color: #4a90e2;
}

@keyframes pulse {
  0%, 100% { opacity: 0.8; }
  50% { opacity: 1; }
}
</style>