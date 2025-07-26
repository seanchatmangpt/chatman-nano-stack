<template>
  <div class="ultrathink-pipeline-connector">
    <div class="connector-header">
      <h2>🌊 Ultrathink Swarm 80/20 Pipeline Connector</h2>
      <p>Connect Nuxt UI to typer → turtle → ttl2dspy → BitActor → Erlang → Ash → Reactor → k8s</p>
    </div>

    <!-- Pipeline Connection Status -->
    <div class="connection-status">
      <div 
        v-for="stage in pipelineStages" 
        :key="stage.id"
        class="stage-status"
        :class="{ 
          connected: stage.connected, 
          active: stage.active,
          critical: stage.critical 
        }"
      >
        <div class="stage-icon">{{ stage.icon }}</div>
        <div class="stage-info">
          <div class="stage-name">{{ stage.name }}</div>
          <div class="stage-status-text">{{ stage.status }}</div>
        </div>
        <div class="stage-metrics">
          <span class="metric">{{ stage.latency }}ms</span>
          <span class="metric">{{ stage.throughput }}/s</span>
        </div>
      </div>
    </div>

    <!-- 80/20 Optimization Matrix -->
    <div class="optimization-matrix">
      <h3>🎯 80/20 Optimization Matrix</h3>
      
      <div class="matrix-controls">
        <div class="control-group">
          <label>Input Domain</label>
          <select v-model="selectedDomain" @change="updateOptimizationMatrix">
            <option value="cybersecurity">Cybersecurity</option>
            <option value="finance">Finance</option>
            <option value="healthcare">Healthcare</option>
            <option value="automotive">Automotive</option>
            <option value="iot">Industrial IoT</option>
          </select>
        </div>
        
        <div class="control-group">
          <label>Complexity Level</label>
          <input 
            type="range" 
            v-model.number="complexityLevel" 
            min="0" 
            max="10" 
            @input="updateOptimizationMatrix"
          >
          <span class="value">{{ complexityLevel }}/10</span>
        </div>
        
        <div class="control-group">
          <label>Optimization Strategy</label>
          <select v-model="optimizationStrategy" @change="updateOptimizationMatrix">
            <option value="skip_non_critical">Skip Non-Critical (80/20)</option>
            <option value="parallel_execution">Parallel Execution</option>
            <option value="adaptive_routing">Adaptive Routing</option>
            <option value="cached_bypass">Cached Bypass</option>
            <option value="minimal_path">Minimal Path</option>
          </select>
        </div>
      </div>

      <!-- Live Pipeline Flow -->
      <div class="pipeline-flow">
        <div class="flow-header">
          <h4>📊 Live Pipeline Flow</h4>
          <button @click="executeOptimizedPipeline" class="execute-btn" :disabled="executing">
            {{ executing ? 'Executing...' : '🚀 Execute Optimized Pipeline' }}
          </button>
        </div>
        
        <div class="flow-visualization">
          <div 
            v-for="(stage, index) in optimizedFlow" 
            :key="stage.id"
            class="flow-stage"
            :class="{ 
              active: stage.executing,
              completed: stage.completed,
              skipped: stage.skipped,
              optimized: stage.optimized
            }"
          >
            <div class="stage-number">{{ index + 1 }}</div>
            <div class="stage-content">
              <div class="stage-title">{{ stage.name }}</div>
              <div class="stage-description">{{ stage.description }}</div>
              <div class="stage-progress" v-if="stage.executing">
                <div class="progress-bar" :style="{ width: stage.progress + '%' }"></div>
              </div>
            </div>
            <div class="stage-metrics-mini">
              <span v-if="!stage.skipped" class="duration">{{ stage.estimatedDuration }}ms</span>
              <span v-if="stage.optimized" class="optimization">⚡{{ stage.optimizationPercent }}%</span>
            </div>
          </div>
        </div>
      </div>
    </div>

    <!-- Permutation Combinations Generator -->
    <div class="permutation-generator">
      <h3>🔄 Smart Permutation Combinations</h3>
      
      <div class="generator-controls">
        <div class="input-section">
          <label>Ontology Input (TTL)</label>
          <textarea 
            v-model="ontologyInput" 
            placeholder="Enter TTL ontology or select from presets..."
            class="ontology-textarea"
          ></textarea>
          
          <div class="preset-buttons">
            <button 
              v-for="preset in ontologyPresets" 
              :key="preset.id"
              @click="loadPreset(preset)"
              class="preset-btn"
            >
              {{ preset.icon }} {{ preset.name }}
            </button>
          </div>
        </div>
        
        <div class="generation-options">
          <div class="option-group">
            <label>Generation Mode</label>
            <select v-model="generationMode">
              <option value="full_pipeline">Full Pipeline</option>
              <option value="critical_path">Critical Path Only</option>
              <option value="domain_optimized">Domain Optimized</option>
              <option value="performance_first">Performance First</option>
            </select>
          </div>
          
          <div class="option-group">
            <label>Target Output</label>
            <div class="checkbox-group">
              <label class="checkbox-label">
                <input type="checkbox" v-model="outputTargets.ash" />
                Ash Resources
              </label>
              <label class="checkbox-label">
                <input type="checkbox" v-model="outputTargets.reactor" />
                Reactor Workflows
              </label>
              <label class="checkbox-label">
                <input type="checkbox" v-model="outputTargets.k8s" />
                K8s Manifests
              </label>
              <label class="checkbox-label">
                <input type="checkbox" v-model="outputTargets.bitactor" />
                BitActor Code
              </label>
            </div>
          </div>
        </div>
      </div>
      
      <div class="permutation-results" v-if="permutationResults.length > 0">
        <h4>🎯 Generated Permutations (80/20 Optimized)</h4>
        <div class="results-grid">
          <div 
            v-for="result in permutationResults" 
            :key="result.id"
            class="result-card"
            :class="{ optimal: result.isOptimal }"
            @click="executePermutation(result)"
          >
            <div class="result-header">
              <span class="result-name">{{ result.name }}</span>
              <span class="efficiency-score">{{ result.efficiency }}%</span>
            </div>
            
            <div class="result-path">
              <div 
                v-for="step in result.path" 
                :key="step.name"
                class="path-step"
                :class="{ skipped: step.skipped }"
              >
                {{ step.shortName }}
              </div>
            </div>
            
            <div class="result-metrics">
              <span class="metric">{{ result.estimatedTime }}ms</span>
              <span class="metric">{{ result.stageCount }}/8 stages</span>
              <span class="metric" v-if="result.optimizationSaving > 0">
                -{{ result.optimizationSaving }}% time
              </span>
            </div>
          </div>
        </div>
      </div>
    </div>

    <!-- Real-time Swarm Intelligence -->
    <div class="swarm-intelligence">
      <h3>🧠 Real-time Swarm Intelligence</h3>
      
      <div class="intelligence-panels">
        <div class="intelligence-panel">
          <h4>📈 Performance Analytics</h4>
          <div class="analytics-content">
            <div class="metric-display">
              <span class="metric-label">Avg Pipeline Time</span>
              <span class="metric-value">{{ swarmMetrics.avgPipelineTime }}ms</span>
            </div>
            <div class="metric-display">
              <span class="metric-label">Success Rate</span>
              <span class="metric-value">{{ swarmMetrics.successRate }}%</span>
            </div>
            <div class="metric-display">
              <span class="metric-label">Optimizations Applied</span>
              <span class="metric-value">{{ swarmMetrics.optimizationsApplied }}</span>
            </div>
          </div>
        </div>
        
        <div class="intelligence-panel">
          <h4>🎯 Swarm Recommendations</h4>
          <div class="recommendations-content">
            <div 
              v-for="recommendation in swarmRecommendations" 
              :key="recommendation.id"
              class="recommendation-item"
              :class="recommendation.priority"
            >
              <div class="rec-icon">{{ recommendation.icon }}</div>
              <div class="rec-content">
                <div class="rec-title">{{ recommendation.title }}</div>
                <div class="rec-description">{{ recommendation.description }}</div>
              </div>
              <button 
                @click="applyRecommendation(recommendation)"
                class="apply-rec-btn"
              >
                Apply
              </button>
            </div>
          </div>
        </div>
        
        <div class="intelligence-panel">
          <h4>🔄 Adaptive Learning</h4>
          <div class="learning-content">
            <div class="learning-stat">
              <span class="stat-label">Patterns Learned</span>
              <span class="stat-value">{{ adaptiveLearning.patternsLearned }}</span>
            </div>
            <div class="learning-stat">
              <span class="stat-label">Accuracy Improvement</span>
              <span class="stat-value">+{{ adaptiveLearning.accuracyImprovement }}%</span>
            </div>
            <div class="recent-insights">
              <h5>Recent Insights:</h5>
              <ul>
                <li v-for="insight in adaptiveLearning.recentInsights" :key="insight.id">
                  {{ insight.text }}
                </li>
              </ul>
            </div>
          </div>
        </div>
      </div>
    </div>

    <!-- Live Execution Monitor -->
    <div class="execution-monitor" v-if="currentExecution">
      <h3>⚡ Live Execution Monitor</h3>
      
      <div class="execution-overview">
        <div class="overview-header">
          <span class="execution-name">{{ currentExecution.name }}</span>
          <span class="execution-status" :class="currentExecution.status">
            {{ currentExecution.status }}
          </span>
        </div>
        
        <div class="execution-timeline">
          <div 
            v-for="step in currentExecution.steps" 
            :key="step.id"
            class="timeline-step"
            :class="{ 
              active: step.status === 'running',
              completed: step.status === 'completed',
              failed: step.status === 'failed'
            }"
          >
            <div class="step-dot"></div>
            <div class="step-info">
              <div class="step-name">{{ step.name }}</div>
              <div class="step-time">{{ step.duration || step.estimatedDuration }}ms</div>
            </div>
          </div>
        </div>
        
        <div class="execution-outputs" v-if="currentExecution.outputs.length > 0">
          <h4>📋 Generated Outputs</h4>
          <div class="outputs-list">
            <div 
              v-for="output in currentExecution.outputs" 
              :key="output.type"
              class="output-item"
            >
              <span class="output-type">{{ output.type }}</span>
              <span class="output-size">{{ formatBytes(output.size) }}</span>
              <button @click="downloadOutput(output)" class="download-btn">
                📥 Download
              </button>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
export default {
  name: 'UltrathinkPipelineConnector',
  
  data() {
    return {
      // Pipeline stages with real-time status
      pipelineStages: [
        {
          id: 'typer',
          name: 'Typer',
          icon: '⌨️',
          status: 'Ready',
          connected: true,
          active: false,
          critical: true,
          latency: 15,
          throughput: 850
        },
        {
          id: 'turtle',
          name: 'Turtle',
          icon: '🐢',
          status: 'Ready',
          connected: true,
          active: false,
          critical: true,
          latency: 8,
          throughput: 1200
        },
        {
          id: 'ttl2dspy',
          name: 'TTL2DSPy',
          icon: '🔄',
          status: 'Ready',
          connected: true,
          active: false,
          critical: false,
          latency: 45,
          throughput: 300
        },
        {
          id: 'bitactor',
          name: 'BitActor',
          icon: '⚡',
          status: 'Ready',
          connected: true,
          active: false,
          critical: true,
          latency: 2,
          throughput: 5000
        },
        {
          id: 'erlang',
          name: 'Erlang/OTP',
          icon: '🚀',
          status: 'Ready',
          connected: true,
          active: false,
          critical: false,
          latency: 12,
          throughput: 2000
        },
        {
          id: 'ash',
          name: 'Ash',
          icon: '🔥',
          status: 'Ready',
          connected: true,
          active: false,
          critical: true,
          latency: 25,
          throughput: 800
        },
        {
          id: 'reactor',
          name: 'Reactor',
          icon: '⚛️',
          status: 'Ready',
          connected: true,
          active: false,
          critical: true,
          latency: 18,
          throughput: 1500
        },
        {
          id: 'k8s',
          name: 'Kubernetes',
          icon: '☸️',
          status: 'Ready',
          connected: true,
          active: false,
          critical: true,
          latency: 35,
          throughput: 400
        }
      ],
      
      // 80/20 optimization configuration
      selectedDomain: 'cybersecurity',
      complexityLevel: 7,
      optimizationStrategy: 'skip_non_critical',
      executing: false,
      
      // Optimized flow for current configuration
      optimizedFlow: [],
      
      // Permutation generation
      ontologyInput: '',
      generationMode: 'critical_path',
      outputTargets: {
        ash: true,
        reactor: true,
        k8s: true,
        bitactor: false
      },
      
      // Ontology presets
      ontologyPresets: [
        {
          id: 'cybersecurity',
          name: 'Cybersecurity',
          icon: '🛡️',
          ontology: `@prefix cyber: <http://cybersecurity.org/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

cyber:ThreatActor a owl:Class .
cyber:Vulnerability a owl:Class .
cyber:Attack a owl:Class .
cyber:Asset a owl:Class .
cyber:SecurityControl a owl:Class .

cyber:exploits a owl:ObjectProperty ;
  owl:domain cyber:ThreatActor ;
  owl:range cyber:Vulnerability .

cyber:targets a owl:ObjectProperty ;
  owl:domain cyber:Attack ;
  owl:range cyber:Asset .

cyber:protects a owl:ObjectProperty ;
  owl:domain cyber:SecurityControl ;
  owl:range cyber:Asset .`
        },
        {
          id: 'finance',
          name: 'Finance',
          icon: '💰',
          ontology: `@prefix finance: <http://finance.org/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

finance:Account a owl:Class .
finance:Transaction a owl:Class .
finance:Portfolio a owl:Class .
finance:Asset a owl:Class .
finance:Risk a owl:Class .

finance:amount a owl:DatatypeProperty ;
  owl:domain finance:Transaction ;
  owl:range xsd:decimal .

finance:contains a owl:ObjectProperty ;
  owl:domain finance:Portfolio ;
  owl:range finance:Asset .`
        },
        {
          id: 'healthcare',
          name: 'Healthcare',
          icon: '🏥',
          ontology: `@prefix health: <http://healthcare.org/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

health:Patient a owl:Class .
health:Condition a owl:Class .
health:Treatment a owl:Class .
health:Provider a owl:Class .
health:Medication a owl:Class .

health:diagnosedWith a owl:ObjectProperty ;
  owl:domain health:Patient ;
  owl:range health:Condition .

health:prescribes a owl:ObjectProperty ;
  owl:domain health:Provider ;
  owl:range health:Medication .`
        }
      ],
      
      // Permutation results
      permutationResults: [],
      
      // Swarm intelligence metrics
      swarmMetrics: {
        avgPipelineTime: 850,
        successRate: 96.5,
        optimizationsApplied: 245
      },
      
      // Swarm recommendations
      swarmRecommendations: [
        {
          id: 'skip_ttl2dspy',
          title: 'Skip TTL2DSPy for Simple Cases',
          description: 'For complexity < 5, bypass TTL2DSPy to save 45ms average',
          icon: '⚡',
          priority: 'high'
        },
        {
          id: 'parallel_ash_reactor',
          title: 'Parallelize Ash + Reactor',
          description: 'Run Ash and Reactor generation in parallel for 30% speedup',
          icon: '🔄',
          priority: 'medium'
        }
      ],
      
      // Adaptive learning
      adaptiveLearning: {
        patternsLearned: 156,
        accuracyImprovement: 18.3,
        recentInsights: [
          { id: 1, text: 'Cybersecurity ontologies benefit from BitActor optimization' },
          { id: 2, text: 'Healthcare domains can skip Erlang stage 78% of the time' },
          { id: 3, text: 'Finance applications show 2x speedup with cached bypass' }
        ]
      },
      
      // Current execution state
      currentExecution: null,
      
      // WebSocket integration for real-time notifications
      ws: null,
      wsConnected: false,
      
      // Real-time pipeline synchronization
      externalPipelineState: {
        isExecuting: false,
        activeStages: [],
        executionId: null
      },
      
      // Notification integration
      notificationSystem: null,
      realTimeEvents: [],
      
      // Integration status
      integrationStatus: {
        notificationSystem: false,
        permutationEngine: false,
        swarmIntelligence: false,
        telemetryDashboard: false
      }
    }
  },
  
  mounted() {
    this.updateOptimizationMatrix()
    this.loadPreset(this.ontologyPresets[0])
    this.generatePermutations()
    this.startMetricsUpdater()
    this.initializeNotificationIntegration()
    this.connectToRealtimePipeline()
  },
  
  beforeDestroy() {
    if (this.metricsInterval) {
      clearInterval(this.metricsInterval)
    }
    this.disconnectFromRealtimePipeline()
  },
  
  methods: {
    updateOptimizationMatrix() {
      // Update optimized flow based on current settings
      const allStages = [...this.pipelineStages]
      
      if (this.optimizationStrategy === 'skip_non_critical') {
        // 80/20 approach - skip non-critical stages
        this.optimizedFlow = allStages.filter(stage => 
          stage.critical || this.complexityLevel > 7
        ).map(stage => ({
          ...stage,
          executing: false,
          completed: false,
          skipped: false,
          optimized: !stage.critical,
          progress: 0,
          estimatedDuration: stage.latency * (stage.critical ? 1 : 0.7),
          optimizationPercent: stage.critical ? 0 : 30,
          description: this.getStageDescription(stage.id)
        }))
      } else if (this.optimizationStrategy === 'parallel_execution') {
        // Parallel execution strategy
        this.optimizedFlow = allStages.map(stage => ({
          ...stage,
          executing: false,
          completed: false,
          skipped: false,
          optimized: ['ash', 'reactor', 'ttl2dspy'].includes(stage.id),
          progress: 0,
          estimatedDuration: stage.latency * 0.6,
          optimizationPercent: ['ash', 'reactor', 'ttl2dspy'].includes(stage.id) ? 40 : 0,
          description: this.getStageDescription(stage.id)
        }))
      } else if (this.optimizationStrategy === 'minimal_path') {
        // Minimal path - only essential stages
        const essentialStages = ['typer', 'turtle', 'ash', 'k8s']
        this.optimizedFlow = allStages.filter(stage => 
          essentialStages.includes(stage.id)
        ).map(stage => ({
          ...stage,
          executing: false,
          completed: false,
          skipped: false,
          optimized: true,
          progress: 0,
          estimatedDuration: stage.latency * 0.8,
          optimizationPercent: 20,
          description: this.getStageDescription(stage.id)
        }))
      } else {
        // Default flow
        this.optimizedFlow = allStages.map(stage => ({
          ...stage,
          executing: false,
          completed: false,
          skipped: false,
          optimized: false,
          progress: 0,
          estimatedDuration: stage.latency,
          optimizationPercent: 0,
          description: this.getStageDescription(stage.id)
        }))
      }
    },
    
    getStageDescription(stageId) {
      const descriptions = {
        typer: 'Convert input to structured format',
        turtle: 'Parse TTL syntax and validate',
        ttl2dspy: 'Generate DSPy agents from ontology',
        bitactor: 'Compile to high-performance BitActor',
        erlang: 'Create Erlang/OTP supervision trees',
        ash: 'Generate Ash framework resources',
        reactor: 'Create Reactor workflow definitions',
        k8s: 'Generate Kubernetes deployment manifests'
      }
      return descriptions[stageId] || 'Processing stage'
    },
    
    async executeOptimizedPipeline() {
      if (this.executing) return
      
      this.executing = true
      
      // Create execution context
      this.currentExecution = {
        id: `exec_${Date.now()}`,
        name: `${this.selectedDomain} - ${this.optimizationStrategy}`,
        status: 'running',
        startTime: Date.now(),
        steps: this.optimizedFlow.map(stage => ({
          id: stage.id,
          name: stage.name,
          status: 'pending',
          estimatedDuration: stage.estimatedDuration
        })),
        outputs: []
      }
      
      try {
        // Execute each stage in the optimized flow
        for (let i = 0; i < this.optimizedFlow.length; i++) {
          const stage = this.optimizedFlow[i]
          
          // Mark stage as executing
          stage.executing = true
          this.currentExecution.steps[i].status = 'running'
          
          // ⚡ INTEGRATION: Emit stage started event
          this.handlePipelineStageStarted({
            id: stage.id,
            name: stage.name,
            estimatedDuration: stage.estimatedDuration,
            optimized: stage.optimized,
            parallel: stage.parallel || false
          })
          
          try {
            // Simulate stage execution with progress
            await this.executeStageWithProgress(stage, i)
            
            // Mark stage as completed
            stage.executing = false
            stage.completed = true
            this.currentExecution.steps[i].status = 'completed'
            this.currentExecution.steps[i].duration = stage.estimatedDuration
            
            // ⚡ INTEGRATION: Emit stage completed event
            this.handlePipelineStageCompleted({
              id: stage.id,
              name: stage.name,
              duration: stage.estimatedDuration,
              estimatedDuration: stage.estimatedDuration,
              efficiency: stage.optimized ? 85 + Math.random() * 10 : 75 + Math.random() * 15,
              optimized: stage.optimized,
              outputs: this.generateStageOutputs(stage.id)
            })
            
          } catch (stageError) {
            // Mark stage as failed
            stage.executing = false
            stage.completed = false
            this.currentExecution.steps[i].status = 'failed'
            
            // ⚡ INTEGRATION: Emit stage error event
            this.handlePipelineStageError({
              id: stage.id,
              name: stage.name,
              error: stageError.message || 'Stage execution failed'
            })
            
            throw stageError
          }
        }
        
        // Generate outputs based on target selection
        this.generateExecutionOutputs()
        
        this.currentExecution.status = 'completed'
        this.currentExecution.endTime = Date.now()
        
        // Update swarm metrics
        this.updateSwarmMetrics()
        
        // ⚡ INTEGRATION: Emit optimization applied event
        this.handleOptimizationApplied({
          type: this.optimizationStrategy,
          improvement: this.calculateOptimizationSaving(this.optimizationStrategy),
          timeSaved: this.calculateTimeSaved(),
          stagesOptimized: this.optimizedFlow.filter(s => s.optimized).length
        })
        
        this.$emit('pipeline-executed', {
          execution: this.currentExecution,
          optimization: this.optimizationStrategy,
          timeSaved: this.calculateTimeSaved()
        })
        
        // ⚡ INTEGRATION: Emit permutation executed event
        this.$emit('permutation-executed', {
          id: this.currentExecution.id,
          name: this.currentExecution.name,
          strategy: this.optimizationStrategy,
          efficiency: this.calculatePermutationEfficiency(this.optimizationStrategy),
          timing: {
            totalDuration: this.currentExecution.endTime - this.currentExecution.startTime,
            optimizationSaving: this.calculateTimeSaved()
          }
        })
        
      } catch (error) {
        this.currentExecution.status = 'failed'
        console.error('Pipeline execution failed:', error)
      } finally {
        this.executing = false
      }
    },
    
    async executeStageWithProgress(stage, index) {
      const duration = stage.estimatedDuration
      const steps = 20
      const stepDuration = duration / steps
      
      for (let i = 0; i <= steps; i++) {
        stage.progress = (i / steps) * 100
        await this.delay(stepDuration)
      }
    },
    
    generateExecutionOutputs() {
      if (this.outputTargets.ash) {
        this.currentExecution.outputs.push({
          type: 'Ash Resources',
          format: 'elixir',
          size: Math.floor(Math.random() * 50000) + 10000,
          content: this.generateAshOutput()
        })
      }
      
      if (this.outputTargets.reactor) {
        this.currentExecution.outputs.push({
          type: 'Reactor Workflows',
          format: 'elixir',
          size: Math.floor(Math.random() * 30000) + 8000,
          content: this.generateReactorOutput()
        })
      }
      
      if (this.outputTargets.k8s) {
        this.currentExecution.outputs.push({
          type: 'K8s Manifests',
          format: 'yaml',
          size: Math.floor(Math.random() * 15000) + 5000,
          content: this.generateK8sOutput()
        })
      }
      
      if (this.outputTargets.bitactor) {
        this.currentExecution.outputs.push({
          type: 'BitActor Code',
          format: 'c',
          size: Math.floor(Math.random() * 40000) + 15000,
          content: this.generateBitActorOutput()
        })
      }
    },
    
    loadPreset(preset) {
      this.ontologyInput = preset.ontology
      this.selectedDomain = preset.id
      this.updateOptimizationMatrix()
      this.generatePermutations()
    },
    
    generatePermutations() {
      // Generate smart permutations based on 80/20 analysis
      const strategies = [
        'skip_non_critical',
        'parallel_execution', 
        'adaptive_routing',
        'cached_bypass',
        'minimal_path'
      ]
      
      this.permutationResults = strategies.map((strategy, index) => {
        const efficiency = this.calculatePermutationEfficiency(strategy)
        const path = this.generatePermutationPath(strategy)
        
        return {
          id: `perm_${index}`,
          name: this.getStrategyName(strategy),
          strategy,
          efficiency,
          path,
          estimatedTime: path.reduce((sum, step) => sum + (step.skipped ? 0 : step.duration), 0),
          stageCount: path.filter(step => !step.skipped).length,
          optimizationSaving: this.calculateOptimizationSaving(strategy),
          isOptimal: false
        }
      })
      
      // Mark most efficient as optimal
      const optimal = this.permutationResults.reduce((best, current) => 
        current.efficiency > best.efficiency ? current : best
      )
      optimal.isOptimal = true
    },
    
    calculatePermutationEfficiency(strategy) {
      const baseEfficiency = {
        skip_non_critical: 85,
        parallel_execution: 78,
        adaptive_routing: 82,
        cached_bypass: 90,
        minimal_path: 95
      }
      
      // Adjust based on domain and complexity
      let efficiency = baseEfficiency[strategy] || 70
      
      if (this.selectedDomain === 'healthcare' && strategy === 'minimal_path') {
        efficiency += 8 // Healthcare benefits from minimal path
      }
      
      if (this.complexityLevel < 5 && strategy === 'skip_non_critical') {
        efficiency += 10 // Simple cases benefit from skipping
      }
      
      return Math.min(efficiency, 100)
    },
    
    generatePermutationPath(strategy) {
      const allStages = this.pipelineStages.map(stage => ({
        name: stage.name,
        shortName: stage.icon,
        duration: stage.latency,
        skipped: false
      }))
      
      if (strategy === 'skip_non_critical') {
        return allStages.map(stage => ({
          ...stage,
          skipped: !this.pipelineStages.find(s => s.name === stage.name)?.critical && this.complexityLevel < 7
        }))
      } else if (strategy === 'minimal_path') {
        const essentials = ['Typer', 'Turtle', 'Ash', 'Kubernetes']
        return allStages.map(stage => ({
          ...stage,
          skipped: !essentials.includes(stage.name)
        }))
      }
      
      return allStages
    },
    
    getStrategyName(strategy) {
      const names = {
        skip_non_critical: '80/20 Critical Path',
        parallel_execution: 'Parallel Execution',
        adaptive_routing: 'Adaptive Routing',
        cached_bypass: 'Cached Bypass',
        minimal_path: 'Minimal Essential'
      }
      return names[strategy] || strategy
    },
    
    calculateOptimizationSaving(strategy) {
      const savings = {
        skip_non_critical: 35,
        parallel_execution: 40,
        adaptive_routing: 25,
        cached_bypass: 60,
        minimal_path: 45
      }
      return savings[strategy] || 0
    },
    
    executePermutation(permutation) {
      this.optimizationStrategy = permutation.strategy
      this.updateOptimizationMatrix()
      this.executeOptimizedPipeline()
    },
    
    applyRecommendation(recommendation) {
      if (recommendation.id === 'skip_ttl2dspy') {
        this.optimizationStrategy = 'skip_non_critical'
        this.complexityLevel = 4
      } else if (recommendation.id === 'parallel_ash_reactor') {
        this.optimizationStrategy = 'parallel_execution'
      }
      
      this.updateOptimizationMatrix()
      
      // Remove applied recommendation
      this.swarmRecommendations = this.swarmRecommendations.filter(r => r.id !== recommendation.id)
    },
    
    startMetricsUpdater() {
      this.metricsInterval = setInterval(() => {
        // Simulate live metrics updates
        this.swarmMetrics.avgPipelineTime += Math.floor(Math.random() * 20) - 10
        this.swarmMetrics.successRate += (Math.random() - 0.5) * 0.5
        
        // Update stage latencies
        this.pipelineStages.forEach(stage => {
          stage.latency += Math.floor(Math.random() * 6) - 3
          stage.latency = Math.max(1, stage.latency)
          stage.throughput += Math.floor(Math.random() * 100) - 50
          stage.throughput = Math.max(100, stage.throughput)
        })
      }, 2000)
    },
    
    updateSwarmMetrics() {
      this.swarmMetrics.optimizationsApplied++
      
      const executionTime = this.currentExecution.endTime - this.currentExecution.startTime
      this.swarmMetrics.avgPipelineTime = (this.swarmMetrics.avgPipelineTime + executionTime) / 2
    },
    
    calculateTimeSaved() {
      const baselineTime = this.pipelineStages.reduce((sum, stage) => sum + stage.latency, 0)
      const optimizedTime = this.optimizedFlow.reduce((sum, stage) => sum + stage.estimatedDuration, 0)
      return baselineTime - optimizedTime
    },
    
    generateAshOutput() {
      return `defmodule MyApp.${this.selectedDomain.charAt(0).toUpperCase() + this.selectedDomain.slice(1)} do
  use Ash.Resource,
    domain: MyApp.Domain

  attributes do
    uuid_primary_key :id
    attribute :name, :string, allow_nil?: false
    timestamps()
  end

  actions do
    defaults [:read, :create, :update, :destroy]
  end
end`
    },
    
    generateReactorOutput() {
      return `defmodule MyApp.${this.selectedDomain.charAt(0).toUpperCase() + this.selectedDomain.slice(1)}Reactor do
  use Reactor

  input :ontology
  
  step :parse_ontology do
    run MyApp.OntologyParser
    argument :ontology, input(:ontology)
  end
  
  step :generate_resources do
    run MyApp.ResourceGenerator
    argument :parsed_data, result(:parse_ontology)
  end
  
  return :generate_resources
end`
    },
    
    generateK8sOutput() {
      return `apiVersion: apps/v1
kind: Deployment
metadata:
  name: ${this.selectedDomain}-app
spec:
  replicas: 3
  selector:
    matchLabels:
      app: ${this.selectedDomain}
  template:
    metadata:
      labels:
        app: ${this.selectedDomain}
    spec:
      containers:
      - name: app
        image: myapp/${this.selectedDomain}:latest
        ports:
        - containerPort: 4000`
    },
    
    generateBitActorOutput() {
      return `#include "bitactor.h"

typedef struct {
    uint64_t id;
    char name[256];
    uint32_t status;
} ${this.selectedDomain}_entity_t;

static inline void process_${this.selectedDomain}_entity(${this.selectedDomain}_entity_t* entity) {
    // Ultra-fast processing logic
    entity->status = PROCESSED;
}`
    },
    
    downloadOutput(output) {
      const blob = new Blob([output.content], { type: 'text/plain' })
      const url = URL.createObjectURL(blob)
      const a = document.createElement('a')
      a.href = url
      a.download = `${output.type.toLowerCase().replace(/\s+/g, '_')}.${output.format}`
      document.body.appendChild(a)
      a.click()
      document.body.removeChild(a)
      URL.revokeObjectURL(url)
    },
    
    formatBytes(bytes) {
      if (bytes === 0) return '0 Bytes'
      const k = 1024
      const sizes = ['Bytes', 'KB', 'MB', 'GB']
      const i = Math.floor(Math.log(bytes) / Math.log(k))
      return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + sizes[i]
    },

    generateStageOutputs(stageId) {
      // Generate stage-specific output metadata for notifications
      const outputs = []
      
      if (stageId === 'ash') {
        outputs.push({ type: 'ash_resource', count: Math.floor(Math.random() * 5) + 1 })
      } else if (stageId === 'reactor') {
        outputs.push({ type: 'reactor_workflow', count: Math.floor(Math.random() * 3) + 1 })
      } else if (stageId === 'k8s') {
        outputs.push({ type: 'k8s_manifest', count: Math.floor(Math.random() * 4) + 1 })
      } else if (stageId === 'bitactor') {
        outputs.push({ type: 'bitactor_code', count: 1 })
      } else {
        outputs.push({ type: 'processed_data', count: 1 })
      }
      
      return outputs
    },
    
    delay(ms) {
      return new Promise(resolve => setTimeout(resolve, ms))
    },

    // ⚡ INTEGRATION: Notification Integration Methods
    initializeNotificationIntegration() {
      // Connect to parent component's notification system
      this.integrationStatus.notificationSystem = true
      
      // Set up event listeners for real-time pipeline events
      this.$on('pipeline-stage-started', this.handlePipelineStageStarted)
      this.$on('pipeline-stage-completed', this.handlePipelineStageCompleted)
      this.$on('pipeline-stage-failed', this.handlePipelineStageError)
      this.$on('pipeline-optimization-applied', this.handleOptimizationApplied)
      
      console.log('🔄 Notification integration initialized')
    },

    connectToRealtimePipeline() {
      try {
        // Connect to Phoenix WebSocket for real-time pipeline updates
        this.ws = new WebSocket('ws://localhost:4000/socket/websocket')
        
        this.ws.onopen = () => {
          this.wsConnected = true
          this.integrationStatus.telemetryDashboard = true
          
          // Join pipeline monitoring channel
          this.ws.send(JSON.stringify({
            topic: 'pipeline:monitor',
            event: 'phx_join',
            payload: { connector_id: this.$options.name },
            ref: Date.now()
          }))
          
          this.emitNotificationEvent({
            type: 'pipeline_events',
            level: 'info',
            message: 'Real-time pipeline connection established',
            source: 'connector',
            channel: 'pipeline_events',
            metadata: {
              connector: 'UltrathinkPipelineConnector',
              timestamp: Date.now()
            }
          })
          
          console.log('🌊 Real-time pipeline connected')
        }

        this.ws.onmessage = (event) => {
          const message = JSON.parse(event.data)
          this.handleRealtimePipelineMessage(message)
        }

        this.ws.onerror = (error) => {
          console.error('🚨 WebSocket connection error:', error)
          this.emitNotificationEvent({
            type: 'error_alerts',
            level: 'error',
            message: 'WebSocket connection failed',
            source: 'connector',
            channel: 'error_alerts',
            metadata: { error: error.message }
          })
        }

        this.ws.onclose = () => {
          this.wsConnected = false
          this.integrationStatus.telemetryDashboard = false
          
          this.emitNotificationEvent({
            type: 'pipeline_events',
            level: 'warning',
            message: 'Real-time pipeline connection lost',
            source: 'connector',
            channel: 'pipeline_events'
          })
        }
      } catch (error) {
        console.error('Failed to connect to real-time pipeline:', error)
      }
    },

    disconnectFromRealtimePipeline() {
      if (this.ws) {
        this.ws.close()
        this.ws = null
        this.wsConnected = false
        
        Object.keys(this.integrationStatus).forEach(key => {
          this.integrationStatus[key] = false
        })
        
        console.log('🔌 Real-time pipeline disconnected')
      }
    },

    handleRealtimePipelineMessage(message) {
      this.realTimeEvents.unshift({
        id: Date.now(),
        timestamp: new Date(),
        event: message.event,
        payload: message.payload
      })
      
      // Keep only last 100 events
      if (this.realTimeEvents.length > 100) {
        this.realTimeEvents = this.realTimeEvents.slice(0, 100)
      }

      // Forward pipeline state updates
      if (message.event === 'pipeline_state_update') {
        this.externalPipelineState = {
          ...this.externalPipelineState,
          ...message.payload
        }
        
        // Sync with local pipeline stages
        this.syncExternalPipelineState(message.payload)
      }

      // Forward notifications to notification system
      if (message.event === 'notification_broadcast') {
        this.emitNotificationEvent(message.payload)
      }
    },

    syncExternalPipelineState(state) {
      if (state.activeStages) {
        this.pipelineStages.forEach(stage => {
          stage.active = state.activeStages.includes(stage.id)
          if (stage.active) {
            stage.status = 'Executing'
          } else if (state.completedStages && state.completedStages.includes(stage.id)) {
            stage.status = 'Completed'
          } else {
            stage.status = 'Ready'
          }
        })
      }
    },

    // Pipeline stage event handlers
    handlePipelineStageStarted(stageData) {
      this.emitNotificationEvent({
        type: 'pipeline_events',
        level: 'info',
        message: `Pipeline stage started: ${stageData.name}`,
        source: stageData.id,
        channel: 'pipeline_events',
        metadata: {
          stage: stageData.id,
          estimatedDuration: stageData.estimatedDuration,
          optimization: stageData.optimized
        }
      })

      // Send Ash-specific notifications
      if (stageData.id === 'ash') {
        this.emitNotificationEvent({
          type: 'ash_resources',
          level: 'info',
          message: 'Ash resource generation initiated',
          source: 'ash',
          channel: 'ash_resources',
          metadata: {
            domain: this.selectedDomain,
            complexity: this.complexityLevel
          }
        })
      }

      // Send Reactor-specific notifications
      if (stageData.id === 'reactor') {
        this.emitNotificationEvent({
          type: 'reactor_workflows',
          level: 'info',
          message: 'Reactor workflow generation started',
          source: 'reactor',
          channel: 'reactor_workflows',
          metadata: {
            optimization: this.optimizationStrategy,
            parallel: stageData.parallel
          }
        })
      }
    },

    handlePipelineStageCompleted(stageData) {
      this.emitNotificationEvent({
        type: 'pipeline_events',
        level: 'success',
        message: `Pipeline stage completed: ${stageData.name} (${stageData.duration}ms)`,
        source: stageData.id,
        channel: 'pipeline_events',
        metadata: {
          stage: stageData.id,
          actualDuration: stageData.duration,
          estimatedDuration: stageData.estimatedDuration,
          efficiency: stageData.efficiency
        }
      })

      // Performance metrics for optimization tracking
      this.emitNotificationEvent({
        type: 'performance_metrics',
        level: 'info',
        message: `Stage ${stageData.name} performance: ${stageData.efficiency}% efficiency`,
        source: stageData.id,
        channel: 'performance_metrics',
        metadata: {
          stage: stageData.id,
          duration: stageData.duration,
          efficiency: stageData.efficiency,
          optimizationApplied: stageData.optimized
        }
      })

      // Ash resource completion notification
      if (stageData.id === 'ash') {
        this.emitNotificationEvent({
          type: 'ash_resources',
          level: 'success',
          message: 'Ash resources generated successfully',
          source: 'ash',
          channel: 'ash_resources',
          metadata: {
            resourceCount: stageData.outputs?.length || 0,
            domain: this.selectedDomain
          }
        })
      }

      // Reactor workflow completion notification
      if (stageData.id === 'reactor') {
        this.emitNotificationEvent({
          type: 'reactor_workflows',
          level: 'success',
          message: 'Reactor workflows generated successfully',
          source: 'reactor',
          channel: 'reactor_workflows',
          metadata: {
            workflowCount: stageData.outputs?.length || 0,
            strategy: this.optimizationStrategy
          }
        })
      }
    },

    handlePipelineStageError(stageData) {
      this.emitNotificationEvent({
        type: 'error_alerts',
        level: 'error',
        message: `Pipeline stage failed: ${stageData.name} - ${stageData.error}`,
        source: stageData.id,
        channel: 'error_alerts',
        metadata: {
          stage: stageData.id,
          error: stageData.error,
          timestamp: Date.now()
        }
      })
    },

    handleOptimizationApplied(optimizationData) {
      this.emitNotificationEvent({
        type: 'swarm_intelligence',
        level: 'info',
        message: `80/20 optimization applied: ${optimizationData.type} - ${optimizationData.improvement}% improvement`,
        source: 'swarm_optimizer',
        channel: 'swarm_intelligence',
        metadata: {
          optimizationType: optimizationData.type,
          improvement: optimizationData.improvement,
          strategy: this.optimizationStrategy
        }
      })
    },

    // Enhanced notification emission with pipeline integration
    emitNotificationEvent(notificationData) {
      // Emit to parent swarm-pipeline component
      this.$emit('pipeline-connected', {
        stages: this.optimizedFlow,
        optimizations: [notificationData],
        selectedPermutation: {
          id: this.optimizationStrategy,
          name: this.getStrategyName(this.optimizationStrategy)
        },
        config: {
          domain: this.selectedDomain,
          complexity: this.complexityLevel,
          strategy: this.optimizationStrategy
        }
      })

      // Send via WebSocket if connected
      if (this.wsConnected && this.ws) {
        this.ws.send(JSON.stringify({
          topic: 'notifications:broadcast',
          event: 'notification_event',
          payload: notificationData,
          ref: Date.now()
        }))
      }

      console.log('📡 Notification emitted:', notificationData)
    }
  }
}
</script>

<style scoped>
.ultrathink-pipeline-connector {
  padding: 2rem;
  background: #0a0a0a;
  color: #e0e0e0;
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
}

.connector-header {
  text-align: center;
  margin-bottom: 3rem;
}

.connector-header h2 {
  font-size: 2.5rem;
  margin-bottom: 0.5rem;
  background: linear-gradient(45deg, #00ff88, #0088ff);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
}

.connector-header p {
  color: #888;
  font-size: 1.1rem;
}

/* Pipeline Connection Status */
.connection-status {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 1rem;
  margin-bottom: 3rem;
}

.stage-status {
  background: #1a1a1a;
  border: 2px solid #333;
  border-radius: 12px;
  padding: 1rem;
  display: flex;
  align-items: center;
  gap: 1rem;
  transition: all 0.3s ease;
}

.stage-status.connected {
  border-color: #00ff88;
}

.stage-status.active {
  background: #2a4a2a;
  border-color: #00ff88;
  box-shadow: 0 0 20px rgba(0, 255, 136, 0.3);
}

.stage-status.critical {
  border-left: 4px solid #ff8800;
}

.stage-icon {
  font-size: 1.5rem;
}

.stage-info {
  flex: 1;
}

.stage-name {
  font-weight: 600;
  margin-bottom: 0.25rem;
}

.stage-status-text {
  font-size: 0.8rem;
  color: #888;
}

.stage-metrics {
  display: flex;
  flex-direction: column;
  align-items: flex-end;
  gap: 0.25rem;
}

.stage-metrics .metric {
  font-size: 0.8rem;
  color: #00ff88;
}

/* Optimization Matrix */
.optimization-matrix {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 2rem;
  margin-bottom: 3rem;
}

.optimization-matrix h3 {
  margin-bottom: 1.5rem;
  color: #0088ff;
}

.matrix-controls {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 1.5rem;
  margin-bottom: 2rem;
}

.control-group {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

.control-group label {
  font-weight: 600;
  color: #888;
}

.control-group select,
.control-group input[type="range"] {
  padding: 0.5rem;
  background: #2a2a2a;
  border: 1px solid #444;
  border-radius: 6px;
  color: #e0e0e0;
}

.control-group .value {
  color: #00ff88;
  font-weight: 600;
}

/* Pipeline Flow */
.pipeline-flow {
  background: #0a0a0a;
  border-radius: 12px;
  padding: 1.5rem;
}

.flow-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1.5rem;
}

.execute-btn {
  background: #00ff88;
  color: #000;
  border: none;
  padding: 0.75rem 1.5rem;
  border-radius: 8px;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
}

.execute-btn:hover:not(:disabled) {
  background: #00cc70;
  transform: translateY(-2px);
}

.execute-btn:disabled {
  opacity: 0.5;
  cursor: not-allowed;
}

.flow-visualization {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
  gap: 1rem;
}

.flow-stage {
  background: #1a1a1a;
  border: 2px solid #333;
  border-radius: 8px;
  padding: 1rem;
  display: flex;
  align-items: center;
  gap: 1rem;
  transition: all 0.3s ease;
}

.flow-stage.active {
  border-color: #0088ff;
  background: #1a2a4a;
}

.flow-stage.completed {
  border-color: #00ff88;
  background: #1a2a1a;
}

.flow-stage.skipped {
  opacity: 0.3;
  border-style: dashed;
}

.flow-stage.optimized::after {
  content: '⚡';
  position: absolute;
  right: 0.5rem;
  top: 0.5rem;
}

.stage-number {
  width: 24px;
  height: 24px;
  background: #333;
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 0.8rem;
  font-weight: 600;
}

.stage-content {
  flex: 1;
}

.stage-title {
  font-weight: 600;
  margin-bottom: 0.25rem;
}

.stage-description {
  font-size: 0.8rem;
  color: #888;
}

.stage-progress {
  margin-top: 0.5rem;
  background: #2a2a2a;
  border-radius: 4px;
  height: 4px;
  overflow: hidden;
}

.progress-bar {
  height: 100%;
  background: #00ff88;
  transition: width 0.3s ease;
}

.stage-metrics-mini {
  display: flex;
  flex-direction: column;
  align-items: flex-end;
  gap: 0.25rem;
}

.stage-metrics-mini .duration {
  font-size: 0.8rem;
  color: #888;
}

.stage-metrics-mini .optimization {
  font-size: 0.7rem;
  color: #00ff88;
  font-weight: 600;
}

/* Permutation Generator */
.permutation-generator {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 2rem;
  margin-bottom: 3rem;
}

.permutation-generator h3 {
  margin-bottom: 1.5rem;
  color: #ff00ff;
}

.generator-controls {
  display: grid;
  grid-template-columns: 2fr 1fr;
  gap: 2rem;
  margin-bottom: 2rem;
}

.input-section label {
  display: block;
  margin-bottom: 0.5rem;
  font-weight: 600;
  color: #888;
}

.ontology-textarea {
  width: 100%;
  height: 200px;
  background: #0a0a0a;
  border: 1px solid #444;
  border-radius: 8px;
  padding: 1rem;
  color: #e0e0e0;
  font-family: 'Monaco', 'Menlo', 'Ubuntu Mono', monospace;
  font-size: 0.9rem;
  resize: vertical;
}

.preset-buttons {
  display: flex;
  gap: 0.5rem;
  margin-top: 1rem;
}

.preset-btn {
  background: #2a2a2a;
  border: 1px solid #444;
  border-radius: 6px;
  padding: 0.5rem 1rem;
  color: #e0e0e0;
  cursor: pointer;
  transition: all 0.3s ease;
}

.preset-btn:hover {
  background: #3a3a3a;
  border-color: #0088ff;
}

.generation-options {
  display: flex;
  flex-direction: column;
  gap: 1.5rem;
}

.option-group label {
  display: block;
  margin-bottom: 0.5rem;
  font-weight: 600;
  color: #888;
}

.option-group select {
  width: 100%;
  padding: 0.5rem;
  background: #2a2a2a;
  border: 1px solid #444;
  border-radius: 6px;
  color: #e0e0e0;
}

.checkbox-group {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

.checkbox-label {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  cursor: pointer;
}

.checkbox-label input[type="checkbox"] {
  margin: 0;
}

/* Permutation Results */
.permutation-results h4 {
  margin-bottom: 1rem;
  color: #00ff88;
}

.results-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
  gap: 1rem;
}

.result-card {
  background: #0a0a0a;
  border: 2px solid #333;
  border-radius: 12px;
  padding: 1.5rem;
  cursor: pointer;
  transition: all 0.3s ease;
}

.result-card:hover {
  border-color: #0088ff;
  transform: translateY(-2px);
}

.result-card.optimal {
  border-color: #00ff88;
  background: #1a2a1a;
}

.result-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1rem;
}

.result-name {
  font-weight: 600;
}

.efficiency-score {
  background: #00ff88;
  color: #000;
  padding: 0.25rem 0.75rem;
  border-radius: 12px;
  font-weight: 700;
  font-size: 0.9rem;
}

.result-path {
  display: flex;
  gap: 0.25rem;
  margin-bottom: 1rem;
}

.path-step {
  width: 24px;
  height: 24px;
  background: #00ff88;
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 0.8rem;
}

.path-step.skipped {
  background: #333;
  opacity: 0.3;
}

.result-metrics {
  display: flex;
  justify-content: space-between;
  font-size: 0.8rem;
  color: #888;
}

/* Swarm Intelligence */
.swarm-intelligence {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 2rem;
  margin-bottom: 3rem;
}

.swarm-intelligence h3 {
  margin-bottom: 1.5rem;
  color: #ff8800;
}

.intelligence-panels {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
  gap: 1.5rem;
}

.intelligence-panel {
  background: #0a0a0a;
  border-radius: 12px;
  padding: 1.5rem;
}

.intelligence-panel h4 {
  margin-bottom: 1rem;
  color: #0088ff;
}

/* Performance Analytics */
.analytics-content {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.metric-display {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.metric-label {
  color: #888;
}

.metric-value {
  color: #00ff88;
  font-weight: 600;
}

/* Recommendations */
.recommendations-content {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.recommendation-item {
  display: flex;
  align-items: center;
  gap: 1rem;
  padding: 1rem;
  background: #1a1a1a;
  border-radius: 8px;
  border-left: 3px solid #0088ff;
}

.recommendation-item.high {
  border-left-color: #ff4444;
}

.recommendation-item.medium {
  border-left-color: #ff8800;
}

.rec-icon {
  font-size: 1.2rem;
}

.rec-content {
  flex: 1;
}

.rec-title {
  font-weight: 600;
  margin-bottom: 0.25rem;
}

.rec-description {
  font-size: 0.8rem;
  color: #888;
}

.apply-rec-btn {
  background: #0088ff;
  color: #fff;
  border: none;
  padding: 0.5rem 1rem;
  border-radius: 6px;
  cursor: pointer;
  transition: all 0.3s ease;
}

.apply-rec-btn:hover {
  background: #0066cc;
}

/* Adaptive Learning */
.learning-content {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.learning-stat {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.stat-label {
  color: #888;
}

.stat-value {
  color: #00ff88;
  font-weight: 600;
}

.recent-insights h5 {
  color: #0088ff;
  margin-bottom: 0.5rem;
}

.recent-insights ul {
  list-style: none;
  padding: 0;
}

.recent-insights li {
  font-size: 0.8rem;
  color: #888;
  margin-bottom: 0.25rem;
  padding-left: 1rem;
  position: relative;
}

.recent-insights li::before {
  content: '•';
  color: #00ff88;
  position: absolute;
  left: 0;
}

/* Execution Monitor */
.execution-monitor {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 2rem;
}

.execution-monitor h3 {
  margin-bottom: 1.5rem;
  color: #ff00ff;
}

.execution-overview {
  display: flex;
  flex-direction: column;
  gap: 2rem;
}

.overview-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.execution-name {
  font-size: 1.2rem;
  font-weight: 600;
}

.execution-status {
  padding: 0.5rem 1rem;
  border-radius: 6px;
  font-weight: 600;
  text-transform: uppercase;
  font-size: 0.8rem;
}

.execution-status.running {
  background: #ff8800;
  color: #000;
}

.execution-status.completed {
  background: #00ff88;
  color: #000;
}

.execution-status.failed {
  background: #ff4444;
  color: #fff;
}

/* Execution Timeline */
.execution-timeline {
  display: flex;
  gap: 1rem;
  overflow-x: auto;
  padding: 1rem 0;
}

.timeline-step {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 0.5rem;
  min-width: 100px;
}

.step-dot {
  width: 12px;
  height: 12px;
  border-radius: 50%;
  background: #333;
  border: 2px solid #333;
}

.timeline-step.active .step-dot {
  background: #0088ff;
  border-color: #0088ff;
  box-shadow: 0 0 10px rgba(0, 136, 255, 0.5);
}

.timeline-step.completed .step-dot {
  background: #00ff88;
  border-color: #00ff88;
}

.timeline-step.failed .step-dot {
  background: #ff4444;
  border-color: #ff4444;
}

.step-info {
  text-align: center;
}

.step-name {
  font-size: 0.8rem;
  font-weight: 600;
}

.step-time {
  font-size: 0.7rem;
  color: #888;
}

/* Execution Outputs */
.execution-outputs h4 {
  margin-bottom: 1rem;
  color: #00ff88;
}

.outputs-list {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

.output-item {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 1rem;
  background: #0a0a0a;
  border-radius: 8px;
  border: 1px solid #333;
}

.output-type {
  font-weight: 600;
}

.output-size {
  color: #888;
  font-size: 0.8rem;
}

.download-btn {
  background: #0088ff;
  color: #fff;
  border: none;
  padding: 0.5rem 1rem;
  border-radius: 6px;
  cursor: pointer;
  transition: all 0.3s ease;
}

.download-btn:hover {
  background: #0066cc;
}
</style>