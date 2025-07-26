<!--
BitActor Nuxt UI Configuration Interface Variant
Advanced configuration management for BitActor pipeline components
Real-time configuration validation with TTL optimization
No TypeScript - Pure JavaScript with reactive configuration
-->

<template>
  <div class="bitactor-config">
    <header class="config-header">
      <h1 class="header-title">
        <span class="title-icon">⚙️</span>
        BitActor Configuration
      </h1>
      <div class="header-actions">
        <button @click="validateConfig" class="action-btn">✓ Validate</button>
        <button @click="resetConfig" class="action-btn warning">🔄 Reset</button>
        <button @click="saveConfig" class="action-btn primary">💾 Save</button>
        <button @click="deployConfig" class="action-btn success">🚀 Deploy</button>
      </div>
    </header>

    <div class="config-layout">
      <!-- Configuration Sections -->
      <main class="config-main">
        <!-- Global Settings -->
        <section class="config-section">
          <h2 class="section-title">Global Pipeline Settings</h2>
          <div class="config-grid">
            <div class="config-group">
              <label>Global TTL Budget (ms):</label>
              <input v-model.number="config.global.ttlBudget" type="number" 
                     min="1" max="1000" class="config-input">
              <span class="config-help">Total time budget for entire pipeline</span>
            </div>
            
            <div class="config-group">
              <label>Execution Mode:</label>
              <select v-model="config.global.executionMode" class="config-select">
                <option value="sequential">Sequential</option>
                <option value="parallel">Parallel</option>
                <option value="adaptive">Adaptive</option>
              </select>
              <span class="config-help">How stages are executed</span>
            </div>
            
            <div class="config-group">
              <label>Error Handling:</label>
              <select v-model="config.global.errorHandling" class="config-select">
                <option value="strict">Strict (Fail Fast)</option>
                <option value="graceful">Graceful Degradation</option>
                <option value="retry">Retry with Backoff</option>
              </select>
              <span class="config-help">Error handling strategy</span>
            </div>

            <div class="config-group">
              <label>Monitoring Level:</label>
              <select v-model="config.global.monitoring" class="config-select">
                <option value="basic">Basic</option>
                <option value="detailed">Detailed</option>
                <option value="debug">Debug</option>
              </select>
              <span class="config-help">Level of monitoring and logging</span>
            </div>
          </div>
        </section>

        <!-- Stage-Specific Configuration -->
        <section class="config-section">
          <h2 class="section-title">Stage Configuration</h2>
          <div class="stages-config">
            <div v-for="stage in config.stages" :key="stage.name" 
                 class="stage-config-card">
              <div class="stage-header">
                <span class="stage-icon">{{ stage.icon }}</span>
                <h3 class="stage-name">{{ stage.name }}</h3>
                <span class="stage-status" :class="stage.configStatus">
                  {{ stage.configStatus }}
                </span>
              </div>

              <div class="stage-config-content">
                <div class="config-row">
                  <div class="config-group">
                    <label>TTL Budget (ms):</label>
                    <input v-model.number="stage.ttlBudget" type="number" 
                           min="0.1" max="100" step="0.1" class="config-input">
                  </div>
                  
                  <div class="config-group">
                    <label>Priority:</label>
                    <select v-model="stage.priority" class="config-select">
                      <option value="low">Low</option>
                      <option value="medium">Medium</option>
                      <option value="high">High</option>
                      <option value="critical">Critical</option>
                    </select>
                  </div>
                </div>

                <div class="config-row">
                  <div class="config-group">
                    <label>Memory Limit (MB):</label>
                    <input v-model.number="stage.memoryLimit" type="number" 
                           min="64" max="4096" class="config-input">
                  </div>
                  
                  <div class="config-group">
                    <label>CPU Limit (%):</label>
                    <input v-model.number="stage.cpuLimit" type="number" 
                           min="10" max="100" class="config-input">
                  </div>
                </div>

                <div class="config-row">
                  <div class="config-group full-width">
                    <label>Configuration Parameters:</label>
                    <textarea v-model="stage.parameters" class="config-textarea" 
                             placeholder="JSON configuration parameters"></textarea>
                  </div>
                </div>

                <div class="config-toggles">
                  <label class="toggle-label">
                    <input v-model="stage.enabled" type="checkbox" class="toggle-input">
                    <span class="toggle-text">Enabled</span>
                  </label>
                  
                  <label class="toggle-label">
                    <input v-model="stage.debugging" type="checkbox" class="toggle-input">
                    <span class="toggle-text">Debug Mode</span>
                  </label>
                  
                  <label class="toggle-label">
                    <input v-model="stage.parallel" type="checkbox" class="toggle-input">
                    <span class="toggle-text">Parallel Execution</span>
                  </label>
                </div>
              </div>
            </div>
          </div>
        </section>

        <!-- Advanced Configuration -->
        <section class="config-section">
          <h2 class="section-title">Advanced Settings</h2>
          <div class="advanced-config">
            <div class="config-subsection">
              <h4>Swarm Configuration</h4>
              <div class="config-grid">
                <div class="config-group">
                  <label>Swarm Topology:</label>
                  <select v-model="config.swarm.topology" class="config-select">
                    <option value="hierarchical">Hierarchical</option>
                    <option value="mesh">Mesh</option>
                    <option value="ring">Ring</option>
                    <option value="star">Star</option>
                  </select>
                </div>
                
                <div class="config-group">
                  <label>Max Agents:</label>
                  <input v-model.number="config.swarm.maxAgents" type="number" 
                         min="1" max="100" class="config-input">
                </div>
                
                <div class="config-group">
                  <label>Coordination Timeout (ms):</label>
                  <input v-model.number="config.swarm.coordinationTimeout" type="number" 
                         min="100" max="10000" class="config-input">
                </div>
              </div>
            </div>

            <div class="config-subsection">
              <h4>Security Configuration</h4>
              <div class="config-grid">
                <div class="config-group">
                  <label>Authentication:</label>
                  <select v-model="config.security.authentication" class="config-select">
                    <option value="none">None</option>
                    <option value="basic">Basic Auth</option>
                    <option value="oauth">OAuth 2.0</option>
                    <option value="jwt">JWT Tokens</option>
                  </select>
                </div>
                
                <div class="config-group">
                  <label>Encryption:</label>
                  <select v-model="config.security.encryption" class="config-select">
                    <option value="none">None</option>
                    <option value="tls">TLS</option>
                    <option value="e2e">End-to-End</option>
                  </select>
                </div>
                
                <div class="config-group">
                  <label>Audit Level:</label>
                  <select v-model="config.security.auditLevel" class="config-select">
                    <option value="minimal">Minimal</option>
                    <option value="standard">Standard</option>
                    <option value="comprehensive">Comprehensive</option>
                  </select>
                </div>
              </div>
            </div>

            <div class="config-subsection">
              <h4>Performance Tuning</h4>
              <div class="config-grid">
                <div class="config-group">
                  <label>Cache Size (MB):</label>
                  <input v-model.number="config.performance.cacheSize" type="number" 
                         min="64" max="2048" class="config-input">
                </div>
                
                <div class="config-group">
                  <label>Connection Pool Size:</label>
                  <input v-model.number="config.performance.connectionPool" type="number" 
                         min="5" max="100" class="config-input">
                </div>
                
                <div class="config-group">
                  <label>Batch Size:</label>
                  <input v-model.number="config.performance.batchSize" type="number" 
                         min="1" max="1000" class="config-input">
                </div>
              </div>
            </div>
          </div>
        </section>
      </main>

      <!-- Configuration Sidebar -->
      <aside class="config-sidebar">
        <!-- Configuration Status -->
        <div class="sidebar-section">
          <h3>Configuration Status</h3>
          <div class="status-indicators">
            <div class="status-item" :class="overallConfigStatus">
              <span class="status-icon">{{ getStatusIcon(overallConfigStatus) }}</span>
              <span class="status-text">{{ overallConfigStatus }}</span>
            </div>
          </div>
          
          <div class="config-summary">
            <div class="summary-item">
              <label>Total TTL Budget:</label>
              <span>{{ totalTTLBudget }}ms</span>
            </div>
            <div class="summary-item">
              <label>Active Stages:</label>
              <span>{{ activeStagesCount }}/{{ config.stages.length }}</span>
            </div>
            <div class="summary-item">
              <label>Memory Usage:</label>
              <span>{{ totalMemoryUsage }}MB</span>
            </div>
          </div>
        </div>

        <!-- Validation Results -->
        <div class="sidebar-section">
          <h3>Validation Results</h3>
          <div class="validation-list">
            <div v-for="result in validationResults" :key="result.id"
                 class="validation-item" :class="result.severity">
              <span class="validation-icon">{{ result.icon }}</span>
              <div class="validation-content">
                <div class="validation-message">{{ result.message }}</div>
                <div v-if="result.suggestion" class="validation-suggestion">
                  💡 {{ result.suggestion }}
                </div>
              </div>
            </div>
          </div>
        </div>

        <!-- Configuration Templates -->
        <div class="sidebar-section">
          <h3>Configuration Templates</h3>
          <div class="template-list">
            <button v-for="template in configTemplates" :key="template.name"
                    @click="loadTemplate(template)" class="template-btn">
              <span class="template-icon">{{ template.icon }}</span>
              <div class="template-info">
                <div class="template-name">{{ template.name }}</div>
                <div class="template-description">{{ template.description }}</div>
              </div>
            </button>
          </div>
        </div>

        <!-- Export/Import -->
        <div class="sidebar-section">
          <h3>Configuration Management</h3>
          <div class="management-actions">
            <button @click="exportConfig" class="management-btn">
              📤 Export Config
            </button>
            <button @click="importConfig" class="management-btn">
              📥 Import Config
            </button>
            <input ref="fileInput" type="file" accept=".json" 
                   @change="handleFileImport" style="display: none;">
          </div>
        </div>
      </aside>
    </div>
  </div>
</template>

<script>
export default {
  name: 'NuxtUIBitActorConfigVariant',
  
  data() {
    return {
      config: {
        global: {
          ttlBudget: 8,
          executionMode: 'sequential',
          errorHandling: 'graceful',
          monitoring: 'detailed'
        },
        
        stages: [
          {
            name: 'Typer',
            icon: '📝',
            enabled: true,
            ttlBudget: 1.0,
            priority: 'medium',
            memoryLimit: 128,
            cpuLimit: 50,
            debugging: false,
            parallel: false,
            parameters: '{"precision": "high", "validation": true}',
            configStatus: 'valid'
          },
          {
            name: 'Turtle',
            icon: '🐢',
            enabled: true,
            ttlBudget: 1.0,
            priority: 'medium',
            memoryLimit: 256,
            cpuLimit: 60,
            debugging: false,
            parallel: false,
            parameters: '{"format": "turtle", "compression": false}',
            configStatus: 'valid'
          },
          {
            name: 'TTL2DSPy',
            icon: '🔍',
            enabled: true,
            ttlBudget: 1.0,
            priority: 'high',
            memoryLimit: 512,
            cpuLimit: 70,
            debugging: true,
            parallel: false,
            parameters: '{"analysis_depth": "deep", "caching": true}',
            configStatus: 'warning'
          },
          {
            name: 'BitActor',
            icon: '⚡',
            enabled: true,
            ttlBudget: 1.5,
            priority: 'critical',
            memoryLimit: 1024,
            cpuLimit: 80,
            debugging: false,
            parallel: true,
            parameters: '{"actors": 4, "message_queue_size": 1000}',
            configStatus: 'valid'
          },
          {
            name: 'Erlang',
            icon: '🔧',
            enabled: true,
            ttlBudget: 1.0,
            priority: 'high',
            memoryLimit: 512,
            cpuLimit: 65,
            debugging: false,
            parallel: false,
            parameters: '{"processes": "auto", "distribution": true}',
            configStatus: 'valid'
          },
          {
            name: 'Ash',
            icon: '🌊',
            enabled: true,
            ttlBudget: 1.2,
            priority: 'high',
            memoryLimit: 768,
            cpuLimit: 70,
            debugging: false,
            parallel: true,
            parameters: '{"resources": "auto", "validation": "strict"}',
            configStatus: 'valid'
          },
          {
            name: 'Reactor',
            icon: '🔄',
            enabled: true,
            ttlBudget: 0.8,
            priority: 'medium',
            memoryLimit: 256,
            cpuLimit: 40,
            debugging: false,
            parallel: false,
            parameters: '{"workflow": "default", "retry": 3}',
            configStatus: 'valid'
          },
          {
            name: 'K8s',
            icon: '☸️',
            enabled: true,
            ttlBudget: 0.5,
            priority: 'low',
            memoryLimit: 128,
            cpuLimit: 30,
            debugging: false,
            parallel: false,
            parameters: '{"replicas": 3, "strategy": "rolling"}',
            configStatus: 'valid'
          }
        ],
        
        swarm: {
          topology: 'hierarchical',
          maxAgents: 12,
          coordinationTimeout: 5000
        },
        
        security: {
          authentication: 'jwt',
          encryption: 'tls',
          auditLevel: 'standard'
        },
        
        performance: {
          cacheSize: 512,
          connectionPool: 20,
          batchSize: 100
        }
      },
      
      validationResults: [
        {
          id: 'val_001',
          severity: 'success',
          icon: '✓',
          message: 'All configurations are valid'
        },
        {
          id: 'val_002',
          severity: 'warning',
          icon: '⚠️',
          message: 'TTL2DSPy debug mode may impact performance',
          suggestion: 'Disable debugging for production deployment'
        }
      ],
      
      configTemplates: [
        {
          name: 'Development',
          icon: '🛠️',
          description: 'Optimized for development with debug features',
          config: 'dev_template'
        },
        {
          name: 'Production',
          icon: '🚀',
          description: 'High-performance production configuration',
          config: 'prod_template'
        },
        {
          name: 'Testing',
          icon: '🧪',
          description: 'Configuration for testing and validation',
          config: 'test_template'
        }
      ]
    }
  },
  
  computed: {
    totalTTLBudget() {
      return this.config.stages
        .filter(stage => stage.enabled)
        .reduce((sum, stage) => sum + stage.ttlBudget, 0)
        .toFixed(1)
    },
    
    activeStagesCount() {
      return this.config.stages.filter(stage => stage.enabled).length
    },
    
    totalMemoryUsage() {
      return this.config.stages
        .filter(stage => stage.enabled)
        .reduce((sum, stage) => sum + stage.memoryLimit, 0)
    },
    
    overallConfigStatus() {
      const hasErrors = this.validationResults.some(r => r.severity === 'error')
      const hasWarnings = this.validationResults.some(r => r.severity === 'warning')
      
      if (hasErrors) return 'error'
      if (hasWarnings) return 'warning'
      return 'valid'
    }
  },
  
  methods: {
    getStatusIcon(status) {
      const icons = {
        valid: '✓',
        warning: '⚠️',
        error: '❌'
      }
      return icons[status] || '❓'
    },
    
    validateConfig() {
      this.validationResults = []
      
      // Validate global TTL budget
      if (this.totalTTLBudget > this.config.global.ttlBudget) {
        this.validationResults.push({
          id: 'val_ttl_exceed',
          severity: 'error',
          icon: '❌',
          message: 'Stage TTL budgets exceed global budget',
          suggestion: 'Reduce individual stage budgets or increase global budget'
        })
      }
      
      // Validate memory limits
      if (this.totalMemoryUsage > 4096) {
        this.validationResults.push({
          id: 'val_memory',
          severity: 'warning',
          icon: '⚠️',
          message: 'High memory usage detected',
          suggestion: 'Consider reducing memory limits for non-critical stages'
        })
      }
      
      // Validate stage configurations
      this.config.stages.forEach(stage => {
        if (stage.enabled && stage.ttlBudget <= 0) {
          this.validationResults.push({
            id: `val_${stage.name}_ttl`,
            severity: 'error',
            icon: '❌',
            message: `${stage.name}: TTL budget must be greater than 0`
          })
        }
      })
      
      if (this.validationResults.length === 0) {
        this.validationResults.push({
          id: 'val_success',
          severity: 'success',
          icon: '✓',
          message: 'All configurations are valid'
        })
      }
    },
    
    resetConfig() {
      // Reset to default configuration
      console.log('Resetting configuration to defaults')
    },
    
    saveConfig() {
      console.log('Saving configuration:', this.config)
    },
    
    deployConfig() {
      console.log('Deploying configuration to BitActor pipeline')
    },
    
    loadTemplate(template) {
      console.log(`Loading template: ${template.name}`)
    },
    
    exportConfig() {
      const configData = JSON.stringify(this.config, null, 2)
      const blob = new Blob([configData], { type: 'application/json' })
      const url = URL.createObjectURL(blob)
      const a = document.createElement('a')
      a.href = url
      a.download = `bitactor-config-${Date.now()}.json`
      a.click()
      URL.revokeObjectURL(url)
    },
    
    importConfig() {
      this.$refs.fileInput.click()
    },
    
    handleFileImport(event) {
      const file = event.target.files[0]
      if (file) {
        const reader = new FileReader()
        reader.onload = (e) => {
          try {
            const importedConfig = JSON.parse(e.target.result)
            this.config = importedConfig
            this.validateConfig()
          } catch (error) {
            console.error('Failed to import configuration:', error)
          }
        }
        reader.readAsText(file)
      }
    }
  }
}
</script>

<style scoped>
.bitactor-config {
  min-height: 100vh;
  background: linear-gradient(135deg, #1a1a2e 0%, #16213e 50%, #0f3460 100%);
  color: #ffffff;
  font-family: 'Inter', sans-serif;
  padding: 20px;
}

.config-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  background: rgba(255, 255, 255, 0.1);
  backdrop-filter: blur(10px);
  border-radius: 16px;
  padding: 25px;
  margin-bottom: 20px;
}

.header-title {
  font-size: 2.2rem;
  font-weight: 700;
  margin: 0;
  display: flex;
  align-items: center;
  gap: 15px;
}

.header-actions {
  display: flex;
  gap: 15px;
}

.action-btn {
  padding: 10px 20px;
  border: none;
  border-radius: 8px;
  background: rgba(255, 255, 255, 0.1);
  color: white;
  cursor: pointer;
  transition: all 0.3s ease;
}

.action-btn.primary {
  background: #4a90e2;
}

.action-btn.success {
  background: #5cb85c;
}

.action-btn.warning {
  background: #f0ad4e;
}

.config-layout {
  display: grid;
  grid-template-columns: 1fr 350px;
  gap: 20px;
}

.config-main {
  display: flex;
  flex-direction: column;
  gap: 25px;
}

.config-section {
  background: rgba(255, 255, 255, 0.05);
  border-radius: 16px;
  padding: 25px;
}

.section-title {
  font-size: 1.6rem;
  font-weight: 600;
  margin-bottom: 20px;
  color: #4a90e2;
}

.config-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
  gap: 20px;
}

.config-group {
  display: flex;
  flex-direction: column;
  gap: 6px;
}

.config-group.full-width {
  grid-column: 1 / -1;
}

.config-group label {
  font-weight: 500;
  opacity: 0.9;
  font-size: 0.9rem;
}

.config-input, .config-select {
  padding: 10px 12px;
  border: 1px solid rgba(255, 255, 255, 0.2);
  border-radius: 6px;
  background: rgba(255, 255, 255, 0.1);
  color: white;
  font-size: 0.9rem;
}

.config-textarea {
  padding: 10px 12px;
  border: 1px solid rgba(255, 255, 255, 0.2);
  border-radius: 6px;
  background: rgba(255, 255, 255, 0.1);
  color: white;
  font-size: 0.9rem;
  resize: vertical;
  min-height: 80px;
  font-family: monospace;
}

.config-help {
  font-size: 0.8rem;
  opacity: 0.6;
  font-style: italic;
}

.stages-config {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(450px, 1fr));
  gap: 20px;
}

.stage-config-card {
  background: rgba(255, 255, 255, 0.1);
  border-radius: 12px;
  padding: 20px;
  border: 1px solid rgba(255, 255, 255, 0.2);
}

.stage-header {
  display: flex;
  align-items: center;
  gap: 12px;
  margin-bottom: 15px;
}

.stage-icon {
  font-size: 1.5rem;
}

.stage-name {
  font-size: 1.2rem;
  font-weight: 600;
  margin: 0;
  flex: 1;
}

.stage-status {
  padding: 4px 12px;
  border-radius: 20px;
  font-size: 0.8rem;
  font-weight: 600;
  text-transform: uppercase;
}

.stage-status.valid {
  background: #5cb85c;
  color: white;
}

.stage-status.warning {
  background: #f0ad4e;
  color: white;
}

.stage-status.error {
  background: #d9534f;
  color: white;
}

.stage-config-content {
  display: flex;
  flex-direction: column;
  gap: 15px;
}

.config-row {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 15px;
}

.config-toggles {
  display: flex;
  gap: 20px;
  flex-wrap: wrap;
}

.toggle-label {
  display: flex;
  align-items: center;
  gap: 8px;
  cursor: pointer;
}

.toggle-input {
  width: auto;
}

.toggle-text {
  font-size: 0.9rem;
}

.advanced-config {
  display: flex;
  flex-direction: column;
  gap: 25px;
}

.config-subsection {
  padding: 20px;
  background: rgba(255, 255, 255, 0.05);
  border-radius: 8px;
}

.config-subsection h4 {
  margin-top: 0;
  margin-bottom: 15px;
  color: #5bc0de;
  font-size: 1.1rem;
}

.config-sidebar {
  display: flex;
  flex-direction: column;
  gap: 20px;
}

.sidebar-section {
  background: rgba(255, 255, 255, 0.1);
  border-radius: 12px;
  padding: 20px;
}

.sidebar-section h3 {
  margin-top: 0;
  margin-bottom: 15px;
  color: #4a90e2;
  font-size: 1.1rem;
}

.status-indicators {
  margin-bottom: 15px;
}

.status-item {
  display: flex;
  align-items: center;
  gap: 10px;
  padding: 10px;
  border-radius: 6px;
}

.status-item.valid {
  background: rgba(92, 184, 92, 0.2);
}

.status-item.warning {
  background: rgba(240, 173, 78, 0.2);
}

.status-item.error {
  background: rgba(217, 83, 79, 0.2);
}

.status-icon {
  font-size: 1.2rem;
}

.status-text {
  font-weight: 600;
  text-transform: capitalize;
}

.config-summary {
  display: flex;
  flex-direction: column;
  gap: 8px;
}

.summary-item {
  display: flex;
  justify-content: space-between;
  font-size: 0.9rem;
}

.summary-item label {
  opacity: 0.8;
}

.summary-item span {
  font-weight: 600;
}

.validation-list {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.validation-item {
  display: flex;
  gap: 10px;
  padding: 10px;
  border-radius: 6px;
  border-left: 4px solid;
}

.validation-item.success {
  background: rgba(92, 184, 92, 0.1);
  border-left-color: #5cb85c;
}

.validation-item.warning {
  background: rgba(240, 173, 78, 0.1);
  border-left-color: #f0ad4e;
}

.validation-item.error {
  background: rgba(217, 83, 79, 0.1);
  border-left-color: #d9534f;
}

.validation-icon {
  font-size: 1rem;
  margin-top: 2px;
}

.validation-content {
  flex: 1;
}

.validation-message {
  font-size: 0.9rem;
  font-weight: 500;
}

.validation-suggestion {
  font-size: 0.8rem;
  opacity: 0.8;
  margin-top: 4px;
}

.template-list {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.template-btn {
  display: flex;
  align-items: center;
  gap: 12px;
  padding: 12px;
  background: rgba(255, 255, 255, 0.05);
  border: 1px solid rgba(255, 255, 255, 0.1);
  border-radius: 8px;
  color: white;
  cursor: pointer;
  transition: all 0.3s ease;
  text-align: left;
}

.template-btn:hover {
  background: rgba(255, 255, 255, 0.1);
  transform: translateY(-2px);
}

.template-icon {
  font-size: 1.5rem;
}

.template-info {
  flex: 1;
}

.template-name {
  font-weight: 600;
  margin-bottom: 2px;
}

.template-description {
  font-size: 0.8rem;
  opacity: 0.7;
}

.management-actions {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.management-btn {
  padding: 10px 15px;
  background: rgba(255, 255, 255, 0.1);
  border: 1px solid rgba(255, 255, 255, 0.2);
  border-radius: 6px;
  color: white;
  cursor: pointer;
  transition: all 0.3s ease;
  font-size: 0.9rem;
}

.management-btn:hover {
  background: rgba(255, 255, 255, 0.2);
}

/* Responsive Design */
@media (max-width: 1200px) {
  .config-layout {
    grid-template-columns: 1fr;
  }
  
  .stages-config {
    grid-template-columns: 1fr;
  }
  
  .config-grid {
    grid-template-columns: 1fr;
  }
  
  .config-row {
    grid-template-columns: 1fr;
  }
}
</style>