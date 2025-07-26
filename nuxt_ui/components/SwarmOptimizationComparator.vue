<template>
  <div class="swarm-optimization-comparator">
    <h2>🔬 Swarm Optimization Comparator</h2>
    
    <!-- Strategy Selection Panel -->
    <div class="strategy-selector">
      <h3>Select Strategies to Compare</h3>
      <div class="strategy-grid">
        <div
          v-for="strategy in availableStrategies"
          :key="strategy.id"
          class="strategy-card"
          :class="{ 
            selected: selectedStrategies.includes(strategy.id),
            disabled: selectedStrategies.length >= 4 && !selectedStrategies.includes(strategy.id)
          }"
          @click="toggleStrategy(strategy.id)"
        >
          <div class="strategy-icon">{{ strategy.icon }}</div>
          <div class="strategy-name">{{ strategy.name }}</div>
          <div class="strategy-efficiency">{{ (strategy.efficiency * 100).toFixed(0) }}% efficient</div>
        </div>
      </div>
      
      <div class="comparison-controls">
        <button @click="runComparison" class="run-comparison-btn" :disabled="selectedStrategies.length < 2">
          🚀 Run Comparison
        </button>
        <button @click="clearComparison" class="clear-btn">
          🧹 Clear All
        </button>
        <button @click="exportResults" class="export-btn">
          📊 Export Results
        </button>
      </div>
    </div>
    
    <!-- Comparison Results Grid -->
    <div class="comparison-grid" v-if="comparisonResults.length > 0">
      <div
        v-for="result in comparisonResults"
        :key="result.strategyId"
        class="comparison-result"
        :class="{ optimal: result.isOptimal }"
      >
        <div class="result-header">
          <h4>{{ getStrategyName(result.strategyId) }}</h4>
          <div class="optimal-badge" v-if="result.isOptimal">🏆 OPTIMAL</div>
        </div>
        
        <!-- Performance Metrics -->
        <div class="metrics-section">
          <h5>⚡ Performance Metrics</h5>
          <div class="metric-item">
            <span class="metric-label">Execution Time</span>
            <span class="metric-value">{{ result.executionTime }}ms</span>
            <div class="metric-bar">
              <div 
                class="metric-fill" 
                :style="{ width: getMetricPercentage(result.executionTime, 'time') + '%' }"
              ></div>
            </div>
          </div>
          
          <div class="metric-item">
            <span class="metric-label">Stages Used</span>
            <span class="metric-value">{{ result.stagesUsed }}/8</span>
            <div class="metric-bar">
              <div 
                class="metric-fill stages" 
                :style="{ width: (result.stagesUsed / 8 * 100) + '%' }"
              ></div>
            </div>
          </div>
          
          <div class="metric-item">
            <span class="metric-label">Resource Usage</span>
            <span class="metric-value">{{ result.resourceUsage }}%</span>
            <div class="metric-bar">
              <div 
                class="metric-fill resources" 
                :style="{ width: result.resourceUsage + '%' }"
              ></div>
            </div>
          </div>
          
          <div class="metric-item">
            <span class="metric-label">80/20 Score</span>
            <span class="metric-value">{{ result.eightyTwentyScore }}%</span>
            <div class="metric-bar">
              <div 
                class="metric-fill score" 
                :style="{ width: result.eightyTwentyScore + '%' }"
              ></div>
            </div>
          </div>
        </div>
        
        <!-- Execution Path -->
        <div class="path-section">
          <h5>🛤️ Execution Path</h5>
          <div class="execution-path">
            <span
              v-for="(stage, index) in result.executionPath"
              :key="stage"
              class="path-stage"
              :class="{ critical: criticalStages.includes(stage) }"
            >
              {{ getStageIcon(stage) }} {{ stage }}
              <span v-if="index < result.executionPath.length - 1" class="path-arrow">→</span>
            </span>
          </div>
        </div>
        
        <!-- Optimizations Applied -->
        <div class="optimizations-section">
          <h5>🔧 Optimizations Applied</h5>
          <div class="optimization-list">
            <div
              v-for="optimization in result.optimizations"
              :key="optimization.type"
              class="optimization-item"
              :class="optimization.type"
            >
              <span class="optimization-icon">{{ getOptimizationIcon(optimization.type) }}</span>
              <span class="optimization-text">{{ optimization.description }}</span>
              <span class="optimization-impact">{{ optimization.impact }}%</span>
            </div>
          </div>
        </div>
        
        <!-- Swarm Intelligence Insights -->
        <div class="insights-section">
          <h5>🧠 Swarm Insights</h5>
          <div class="insight-list">
            <div
              v-for="insight in result.swarmInsights"
              :key="insight.id"
              class="insight-item"
              :class="insight.type"
            >
              {{ insight.text }}
            </div>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Comparison Chart -->
    <div class="comparison-chart" v-if="comparisonResults.length > 0">
      <h3>📈 Performance Comparison Chart</h3>
      <canvas ref="comparisonCanvas" width="800" height="400"></canvas>
    </div>
    
    <!-- Side-by-Side Analysis -->
    <div class="side-by-side-analysis" v-if="comparisonResults.length >= 2">
      <h3>🔍 Side-by-Side Analysis</h3>
      <div class="analysis-table">
        <table>
          <thead>
            <tr>
              <th>Metric</th>
              <th v-for="result in comparisonResults" :key="result.strategyId">
                {{ getStrategyName(result.strategyId) }}
              </th>
            </tr>
          </thead>
          <tbody>
            <tr>
              <td><strong>Execution Time</strong></td>
              <td v-for="result in comparisonResults" :key="result.strategyId">
                {{ result.executionTime }}ms
              </td>
            </tr>
            <tr>
              <td><strong>Stages Used</strong></td>
              <td v-for="result in comparisonResults" :key="result.strategyId">
                {{ result.stagesUsed }}/8
              </td>
            </tr>
            <tr>
              <td><strong>Resource Efficiency</strong></td>
              <td v-for="result in comparisonResults" :key="result.strategyId">
                {{ (100 - result.resourceUsage) }}%
              </td>
            </tr>
            <tr>
              <td><strong>80/20 Optimization</strong></td>
              <td v-for="result in comparisonResults" :key="result.strategyId">
                {{ result.eightyTwentyScore }}%
              </td>
            </tr>
            <tr>
              <td><strong>Swarm Intelligence</strong></td>
              <td v-for="result in comparisonResults" :key="result.strategyId">
                {{ result.swarmInsights.length }} insights
              </td>
            </tr>
          </tbody>
        </table>
      </div>
    </div>
    
    <!-- Recommendations Panel -->
    <div class="recommendations-panel" v-if="recommendations.length > 0">
      <h3>💡 Swarm Recommendations</h3>
      <div class="recommendation-list">
        <div
          v-for="recommendation in recommendations"
          :key="recommendation.id"
          class="recommendation-item"
          :class="recommendation.priority"
        >
          <div class="recommendation-icon">{{ getRecommendationIcon(recommendation.type) }}</div>
          <div class="recommendation-content">
            <div class="recommendation-title">{{ recommendation.title }}</div>
            <div class="recommendation-description">{{ recommendation.description }}</div>
          </div>
          <button 
            @click="applyRecommendation(recommendation)"
            class="apply-recommendation-btn"
          >
            Apply
          </button>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
export default {
  name: 'SwarmOptimizationComparator',
  
  data() {
    return {
      availableStrategies: [
        { id: 'linear', name: 'Linear Sequential', icon: '📝', efficiency: 0.6 },
        { id: 'skip_optimization', name: 'Skip Optimization (80/20)', icon: '⚡', efficiency: 0.9 },
        { id: 'parallel_merge', name: 'Parallel Merge', icon: '🔀', efficiency: 0.8 },
        { id: 'emergence_guided', name: 'Emergence Guided', icon: '🧠', efficiency: 0.85 },
        { id: 'domain_specific', name: 'Domain Specific', icon: '🎯', efficiency: 0.7 },
        { id: 'complexity_branch', name: 'Complexity Branch', icon: '🌳', efficiency: 0.75 },
        { id: 'adaptive_branch', name: 'Adaptive Branch', icon: '🔄', efficiency: 0.78 },
        { id: 'iterative_loop', name: 'Iterative Loop', icon: '🔁', efficiency: 0.72 }
      ],
      
      selectedStrategies: [],
      comparisonResults: [],
      recommendations: [],
      
      criticalStages: ['typer', 'turtle', 'ash', 'k8s'],
      
      // WebSocket for real-time data
      socket: null,
      connected: false
    }
  },
  
  mounted() {
    this.setupWebSocket()
  },
  
  beforeDestroy() {
    if (this.socket) {
      this.socket.disconnect()
    }
  },
  
  methods: {
    setupWebSocket() {
      // Simulate WebSocket connection for now
      this.connected = true
      console.log('WebSocket connected for optimization comparator')
    },
    
    toggleStrategy(strategyId) {
      if (this.selectedStrategies.includes(strategyId)) {
        this.selectedStrategies = this.selectedStrategies.filter(id => id !== strategyId)
      } else if (this.selectedStrategies.length < 4) {
        this.selectedStrategies.push(strategyId)
      }
    },
    
    async runComparison() {
      if (this.selectedStrategies.length < 2) return
      
      // Simulate running comparison for each selected strategy
      const results = []
      
      for (const strategyId of this.selectedStrategies) {
        const result = await this.simulateStrategyExecution(strategyId)
        results.push(result)
      }
      
      // Find optimal strategy
      const optimalResult = results.reduce((best, current) => 
        current.overallScore > best.overallScore ? current : best
      )
      optimalResult.isOptimal = true
      
      this.comparisonResults = results
      this.generateRecommendations()
      this.drawComparisonChart()
    },
    
    async simulateStrategyExecution(strategyId) {
      // Simulate strategy execution with realistic metrics
      const strategy = this.availableStrategies.find(s => s.id === strategyId)
      
      const baseMetrics = this.getBaseMetrics(strategyId)
      const optimizations = this.generateOptimizations(strategyId)
      const swarmInsights = this.generateSwarmInsights(strategyId)
      
      // Apply optimizations to base metrics
      let executionTime = baseMetrics.executionTime
      let resourceUsage = baseMetrics.resourceUsage
      let eightyTwentyScore = baseMetrics.eightyTwentyScore
      
      optimizations.forEach(opt => {
        executionTime *= (1 - opt.impact / 100)
        resourceUsage *= (1 - opt.impact / 200) // Less impact on resources
        eightyTwentyScore += opt.impact / 4 // Boost 80/20 score
      })
      
      const overallScore = this.calculateOverallScore({
        executionTime,
        stagesUsed: baseMetrics.stagesUsed,
        resourceUsage,
        eightyTwentyScore
      })
      
      return {
        strategyId,
        executionTime: Math.round(executionTime),
        stagesUsed: baseMetrics.stagesUsed,
        resourceUsage: Math.round(resourceUsage),
        eightyTwentyScore: Math.round(Math.min(100, eightyTwentyScore)),
        executionPath: baseMetrics.executionPath,
        optimizations,
        swarmInsights,
        overallScore,
        isOptimal: false
      }
    },
    
    getBaseMetrics(strategyId) {
      const metrics = {
        linear: {
          executionTime: 500,
          stagesUsed: 8,
          resourceUsage: 80,
          eightyTwentyScore: 60,
          executionPath: ['typer', 'turtle', 'ttl2dspy', 'bitactor', 'erlang', 'ash', 'reactor', 'k8s']
        },
        skip_optimization: {
          executionTime: 200,
          stagesUsed: 4,
          resourceUsage: 40,
          eightyTwentyScore: 90,
          executionPath: ['typer', 'turtle', 'ash', 'k8s']
        },
        parallel_merge: {
          executionTime: 300,
          stagesUsed: 6,
          resourceUsage: 90,
          eightyTwentyScore: 75,
          executionPath: ['typer', 'turtle', 'ttl2dspy', 'ash', 'reactor', 'k8s']
        },
        emergence_guided: {
          executionTime: 250,
          stagesUsed: 5,
          resourceUsage: 60,
          eightyTwentyScore: 85,
          executionPath: ['typer', 'turtle', 'ttl2dspy', 'ash', 'k8s']
        },
        domain_specific: {
          executionTime: 350,
          stagesUsed: 6,
          resourceUsage: 70,
          eightyTwentyScore: 70,
          executionPath: ['typer', 'turtle', 'ttl2dspy', 'ash', 'reactor', 'k8s']
        },
        complexity_branch: {
          executionTime: 400,
          stagesUsed: 7,
          resourceUsage: 75,
          eightyTwentyScore: 65,
          executionPath: ['typer', 'turtle', 'ttl2dspy', 'bitactor', 'ash', 'reactor', 'k8s']
        },
        adaptive_branch: {
          executionTime: 280,
          stagesUsed: 5,
          resourceUsage: 65,
          eightyTwentyScore: 78,
          executionPath: ['typer', 'turtle', 'ash', 'reactor', 'k8s']
        },
        iterative_loop: {
          executionTime: 380,
          stagesUsed: 6,
          resourceUsage: 72,
          eightyTwentyScore: 68,
          executionPath: ['typer', 'turtle', 'ttl2dspy', 'ash', 'reactor', 'k8s']
        }
      }
      
      return metrics[strategyId] || metrics.linear
    },
    
    generateOptimizations(strategyId) {
      const optimizations = []
      
      // Common optimizations
      if (['skip_optimization', 'emergence_guided'].includes(strategyId)) {
        optimizations.push({
          type: 'stage_reduction',
          description: 'Eliminated non-critical stages',
          impact: 25
        })
      }
      
      if (['parallel_merge', 'adaptive_branch'].includes(strategyId)) {
        optimizations.push({
          type: 'parallelization',
          description: 'Parallel execution of independent stages',
          impact: 20
        })
      }
      
      if (strategyId === 'skip_optimization') {
        optimizations.push({
          type: 'pareto_optimization',
          description: '80/20 principle applied',
          impact: 30
        })
      }
      
      if (['emergence_guided', 'adaptive_branch'].includes(strategyId)) {
        optimizations.push({
          type: 'ai_optimization',
          description: 'AI-driven path selection',
          impact: 15
        })
      }
      
      // Add swarm-specific optimizations
      optimizations.push({
        type: 'swarm_coordination',
        description: 'Swarm intelligence coordination',
        impact: Math.floor(Math.random() * 10) + 5
      })
      
      return optimizations
    },
    
    generateSwarmInsights(strategyId) {
      const insights = []
      
      const strategy = this.availableStrategies.find(s => s.id === strategyId)
      
      insights.push({
        id: 'efficiency',
        type: 'performance',
        text: `Strategy efficiency: ${(strategy.efficiency * 100).toFixed(0)}%`
      })
      
      if (strategyId === 'skip_optimization') {
        insights.push({
          id: 'pareto',
          type: 'optimization',
          text: 'Optimal 80/20 distribution achieved'
        })
      }
      
      if (['parallel_merge', 'adaptive_branch'].includes(strategyId)) {
        insights.push({
          id: 'parallelism',
          type: 'performance',
          text: 'High parallelization potential detected'
        })
      }
      
      if (['emergence_guided', 'adaptive_branch'].includes(strategyId)) {
        insights.push({
          id: 'adaptation',
          type: 'intelligence',
          text: 'Dynamic adaptation capabilities active'
        })
      }
      
      // Add swarm-specific insights
      insights.push({
        id: 'swarm_coordination',
        type: 'coordination',
        text: 'Swarm coordination factor: 0.85+'
      })
      
      return insights
    },
    
    calculateOverallScore(metrics) {
      // Weighted scoring: efficiency (40%), speed (30%), resource usage (20%), 80/20 score (10%)
      const speedScore = Math.max(0, 100 - (metrics.executionTime / 5))
      const resourceScore = 100 - metrics.resourceUsage
      const stageScore = 100 - ((metrics.stagesUsed - 4) / 4 * 100) // Favor fewer stages
      
      return (
        speedScore * 0.3 +
        resourceScore * 0.2 +
        stageScore * 0.4 +
        metrics.eightyTwentyScore * 0.1
      )
    },
    
    clearComparison() {
      this.selectedStrategies = []
      this.comparisonResults = []
      this.recommendations = []
    },
    
    exportResults() {
      const data = {
        timestamp: new Date().toISOString(),
        comparison: this.comparisonResults,
        recommendations: this.recommendations
      }
      
      const blob = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' })
      const url = URL.createObjectURL(blob)
      const a = document.createElement('a')
      a.href = url
      a.download = `swarm-optimization-comparison-${Date.now()}.json`
      a.click()
      URL.revokeObjectURL(url)
    },
    
    generateRecommendations() {
      this.recommendations = []
      
      if (this.comparisonResults.length === 0) return
      
      const optimalResult = this.comparisonResults.find(r => r.isOptimal)
      const worstResult = this.comparisonResults.reduce((worst, current) => 
        current.overallScore < worst.overallScore ? current : worst
      )
      
      // Recommend best strategy
      this.recommendations.push({
        id: 'optimal-strategy',
        type: 'performance',
        priority: 'high',
        title: 'Optimal Strategy Identified',
        description: `${this.getStrategyName(optimalResult.strategyId)} shows best overall performance with ${optimalResult.overallScore.toFixed(0)}% efficiency.`,
        action: () => this.applyStrategy(optimalResult.strategyId)
      })
      
      // Check for 80/20 opportunities
      const lowEightyTwenty = this.comparisonResults.filter(r => r.eightyTwentyScore < 70)
      if (lowEightyTwenty.length > 0) {
        this.recommendations.push({
          id: '80-20-improvement',
          type: 'optimization',
          priority: 'medium',
          title: '80/20 Optimization Opportunity',
          description: 'Some strategies could benefit from better 80/20 optimization. Consider Skip Optimization strategy.',
          action: () => this.suggestSkipOptimization()
        })
      }
      
      // Resource efficiency recommendation
      const highResourceUsage = this.comparisonResults.filter(r => r.resourceUsage > 80)
      if (highResourceUsage.length > 0) {
        this.recommendations.push({
          id: 'resource-efficiency',
          type: 'resource',
          priority: 'medium',
          title: 'Resource Usage Optimization',
          description: 'High resource usage detected. Consider strategies with better resource efficiency.',
          action: () => this.optimizeResourceUsage()
        })
      }
    },
    
    applyRecommendation(recommendation) {
      if (recommendation.action) {
        recommendation.action()
      }
      
      // Remove applied recommendation
      this.recommendations = this.recommendations.filter(r => r.id !== recommendation.id)
    },
    
    drawComparisonChart() {
      this.$nextTick(() => {
        const canvas = this.$refs.comparisonCanvas
        if (!canvas) return
        
        const ctx = canvas.getContext('2d')
        const width = canvas.width
        const height = canvas.height
        
        // Clear canvas
        ctx.fillStyle = '#1a1a1a'
        ctx.fillRect(0, 0, width, height)
        
        // Chart settings
        const margin = 60
        const chartWidth = width - 2 * margin
        const chartHeight = height - 2 * margin
        
        // Draw axes
        ctx.strokeStyle = '#666'
        ctx.lineWidth = 2
        ctx.beginPath()
        ctx.moveTo(margin, height - margin)
        ctx.lineTo(width - margin, height - margin) // X-axis
        ctx.moveTo(margin, height - margin)
        ctx.lineTo(margin, margin) // Y-axis
        ctx.stroke()
        
        // Draw bars
        const barWidth = chartWidth / this.comparisonResults.length
        const colors = ['#00ff88', '#0088ff', '#ff8800', '#ff00ff']
        
        this.comparisonResults.forEach((result, index) => {
          const x = margin + (index * barWidth) + (barWidth * 0.1)
          const barHeight = (result.overallScore / 100) * chartHeight
          const y = height - margin - barHeight
          
          // Draw bar
          ctx.fillStyle = colors[index % colors.length]
          ctx.fillRect(x, y, barWidth * 0.8, barHeight)
          
          // Draw strategy name
          ctx.fillStyle = '#e0e0e0'
          ctx.font = '12px Arial'
          ctx.textAlign = 'center'
          ctx.fillText(
            this.getStrategyName(result.strategyId),
            x + (barWidth * 0.4),
            height - margin + 20
          )
          
          // Draw score
          ctx.fillStyle = '#fff'
          ctx.font = 'bold 14px Arial'
          ctx.fillText(
            result.overallScore.toFixed(0) + '%',
            x + (barWidth * 0.4),
            y - 10
          )
        })
        
        // Chart title
        ctx.fillStyle = '#e0e0e0'
        ctx.font = 'bold 16px Arial'
        ctx.textAlign = 'center'
        ctx.fillText('Overall Performance Score', width / 2, 30)
      })
    },
    
    getStrategyName(strategyId) {
      const strategy = this.availableStrategies.find(s => s.id === strategyId)
      return strategy ? strategy.name : strategyId
    },
    
    getStageIcon(stageName) {
      const icons = {
        typer: '🎯',
        turtle: '🐢',
        ttl2dspy: '🧠',
        bitactor: '⚛️',
        erlang: '🔧',
        ash: '🔥',
        reactor: '⚡',
        k8s: '☸️'
      }
      return icons[stageName] || '📦'
    },
    
    getOptimizationIcon(type) {
      const icons = {
        stage_reduction: '✂️',
        parallelization: '🔀',
        pareto_optimization: '⚡',
        ai_optimization: '🧠',
        swarm_coordination: '🐝'
      }
      return icons[type] || '🔧'
    },
    
    getRecommendationIcon(type) {
      const icons = {
        performance: '🚀',
        optimization: '⚡',
        resource: '💾',
        intelligence: '🧠'
      }
      return icons[type] || '💡'
    },
    
    getMetricPercentage(value, type) {
      const maxValues = {
        time: 500,
        stages: 8,
        resources: 100,
        score: 100
      }
      
      return (value / maxValues[type]) * 100
    },
    
    applyStrategy(strategyId) {
      console.log(`Applying strategy: ${strategyId}`)
      // Integration with main pipeline system
    },
    
    suggestSkipOptimization() {
      if (!this.selectedStrategies.includes('skip_optimization')) {
        this.selectedStrategies.push('skip_optimization')
        this.runComparison()
      }
    },
    
    optimizeResourceUsage() {
      console.log('Optimizing resource usage across strategies')
      // Implement resource optimization logic
    }
  }
}
</script>

<style scoped>
.swarm-optimization-comparator {
  padding: 2rem;
  background: #0a0a0a;
  color: #e0e0e0;
}

.swarm-optimization-comparator h2 {
  margin-bottom: 2rem;
  font-size: 2rem;
}

.strategy-selector {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
  margin-bottom: 2rem;
}

.strategy-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 1rem;
  margin: 1rem 0;
}

.strategy-card {
  background: #2a2a2a;
  border-radius: 12px;
  padding: 1rem;
  text-align: center;
  cursor: pointer;
  transition: all 0.3s ease;
  border: 2px solid transparent;
}

.strategy-card:hover {
  transform: translateY(-2px);
  filter: brightness(1.1);
}

.strategy-card.selected {
  border-color: #00ff88;
  background: #1a2a1a;
}

.strategy-card.disabled {
  opacity: 0.5;
  cursor: not-allowed;
}

.strategy-icon {
  font-size: 2rem;
  margin-bottom: 0.5rem;
}

.strategy-name {
  font-weight: 600;
  margin-bottom: 0.25rem;
}

.strategy-efficiency {
  font-size: 0.8rem;
  color: #888;
}

.comparison-controls {
  display: flex;
  gap: 1rem;
  margin-top: 1.5rem;
}

.run-comparison-btn, .clear-btn, .export-btn {
  padding: 0.75rem 1.5rem;
  border-radius: 8px;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
  border: none;
}

.run-comparison-btn {
  background: #00ff88;
  color: #000;
}

.run-comparison-btn:disabled {
  background: #444;
  color: #888;
  cursor: not-allowed;
}

.clear-btn {
  background: #ff4444;
  color: #fff;
}

.export-btn {
  background: #0088ff;
  color: #fff;
}

.comparison-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(400px, 1fr));
  gap: 2rem;
  margin-bottom: 2rem;
}

.comparison-result {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
  border: 2px solid #333;
}

.comparison-result.optimal {
  border-color: #ffd700;
  background: #2a2a1a;
}

.result-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1.5rem;
}

.result-header h4 {
  margin: 0;
  font-size: 1.2rem;
}

.optimal-badge {
  background: #ffd700;
  color: #000;
  padding: 0.25rem 0.5rem;
  border-radius: 4px;
  font-size: 0.8rem;
  font-weight: 700;
}

.metrics-section, .path-section, .optimizations-section, .insights-section {
  margin-bottom: 1.5rem;
}

.metrics-section h5, .path-section h5, .optimizations-section h5, .insights-section h5 {
  margin-bottom: 1rem;
  color: #888;
}

.metric-item {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 0.5rem;
}

.metric-label {
  flex: 1;
}

.metric-value {
  font-weight: 600;
  color: #00ff88;
  margin-right: 1rem;
}

.metric-bar {
  flex: 1;
  height: 8px;
  background: #333;
  border-radius: 4px;
  overflow: hidden;
}

.metric-fill {
  height: 100%;
  background: #00ff88;
  transition: all 0.3s ease;
}

.metric-fill.stages {
  background: #0088ff;
}

.metric-fill.resources {
  background: #ff8800;
}

.metric-fill.score {
  background: #ff00ff;
}

.execution-path {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  flex-wrap: wrap;
}

.path-stage {
  background: #2a2a2a;
  padding: 0.25rem 0.5rem;
  border-radius: 4px;
  font-size: 0.8rem;
}

.path-stage.critical {
  background: #2a1a1a;
  border: 1px solid #ff8800;
}

.path-arrow {
  color: #666;
}

.optimization-list, .insight-list {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

.optimization-item, .insight-item {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.5rem;
  background: #2a2a2a;
  border-radius: 6px;
  font-size: 0.9rem;
}

.optimization-impact {
  background: #00ff88;
  color: #000;
  padding: 0.125rem 0.25rem;
  border-radius: 3px;
  font-size: 0.7rem;
  font-weight: 600;
}

.insight-item.performance {
  border-left: 3px solid #00ff88;
}

.insight-item.optimization {
  border-left: 3px solid #ff8800;
}

.insight-item.intelligence {
  border-left: 3px solid #ff00ff;
}

.insight-item.coordination {
  border-left: 3px solid #0088ff;
}

.comparison-chart {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
  margin-bottom: 2rem;
  text-align: center;
}

.side-by-side-analysis {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
  margin-bottom: 2rem;
}

.analysis-table {
  overflow-x: auto;
}

.analysis-table table {
  width: 100%;
  border-collapse: collapse;
}

.analysis-table th,
.analysis-table td {
  padding: 0.75rem;
  text-align: left;
  border-bottom: 1px solid #333;
}

.analysis-table th {
  background: #2a2a2a;
  font-weight: 600;
}

.recommendations-panel {
  background: #1a1a1a;
  border-radius: 16px;
  padding: 1.5rem;
}

.recommendation-list {
  display: flex;
  flex-direction: column;
  gap: 1rem;
  margin-top: 1rem;
}

.recommendation-item {
  display: flex;
  align-items: center;
  gap: 1rem;
  padding: 1rem;
  background: #2a2a2a;
  border-radius: 8px;
  border-left: 4px solid #666;
}

.recommendation-item.high {
  border-left-color: #ff4444;
}

.recommendation-item.medium {
  border-left-color: #ff8800;
}

.recommendation-item.low {
  border-left-color: #00ff88;
}

.recommendation-icon {
  font-size: 1.5rem;
}

.recommendation-content {
  flex: 1;
}

.recommendation-title {
  font-weight: 600;
  margin-bottom: 0.25rem;
}

.recommendation-description {
  font-size: 0.9rem;
  color: #aaa;
}

.apply-recommendation-btn {
  padding: 0.5rem 1rem;
  background: #0088ff;
  color: #fff;
  border: none;
  border-radius: 6px;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
}

.apply-recommendation-btn:hover {
  background: #0066cc;
}
</style>