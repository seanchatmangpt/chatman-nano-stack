<template>
  <div class="permutation-matrix">
    <h2>🔄 Pipeline Permutation Matrix</h2>
    
    <!-- Interactive Controls -->
    <div class="matrix-controls">
      <div class="control-group">
        <label>Complexity Level</label>
        <input 
          type="range" 
          v-model="complexity" 
          min="0" 
          max="1" 
          step="0.1"
          @input="updatePermutations"
        >
        <span class="value">{{ complexity }}</span>
      </div>
      
      <div class="control-group">
        <label>Domain</label>
        <select v-model="selectedDomain" @change="updatePermutations">
          <option value="generic">Generic</option>
          <option value="cybersecurity">Cybersecurity</option>
          <option value="finance">Finance</option>
          <option value="healthcare">Healthcare</option>
        </select>
      </div>
      
      <div class="control-group">
        <label>Priority</label>
        <select v-model="priority" @change="updatePermutations">
          <option value="low">Low</option>
          <option value="medium">Medium</option>
          <option value="high">High</option>
          <option value="critical">Critical</option>
        </select>
      </div>
    </div>
    
    <!-- Permutation Grid -->
    <div class="permutation-grid">
      <div 
        v-for="permutation in permutations" 
        :key="permutation.id"
        class="permutation-card"
        :class="{ optimal: permutation.isOptimal }"
        @click="selectPermutation(permutation)"
      >
        <div class="card-header">
          <span class="strategy-name">{{ permutation.strategy }}</span>
          <span class="efficiency-badge">{{ (permutation.efficiency * 100).toFixed(0) }}%</span>
        </div>
        
        <div class="path-preview">
          <div 
            v-for="(stage, index) in permutation.path" 
            :key="index"
            class="stage-dot"
            :class="{ active: stage.active, skipped: stage.skipped }"
            :title="stage.name"
          >
            {{ getStageInitial(stage.name) }}
          </div>
        </div>
        
        <div class="metrics">
          <div class="metric-item">
            <span class="label">Stages</span>
            <span class="value">{{ permutation.activeStages }}/8</span>
          </div>
          <div class="metric-item">
            <span class="label">Duration</span>
            <span class="value">{{ permutation.totalDuration }}ms</span>
          </div>
          <div class="metric-item">
            <span class="label">80/20</span>
            <span class="value">{{ permutation.optimization80_20 ? '✓' : '✗' }}</span>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Swarm Recommendation -->
    <div class="swarm-recommendation" v-if="swarmRecommendation">
      <h3>🧠 Swarm Intelligence Recommendation</h3>
      <div class="recommendation-content">
        <p>Based on current parameters:</p>
        <ul>
          <li>Complexity: {{ complexity }}</li>
          <li>Domain: {{ selectedDomain }}</li>
          <li>Priority: {{ priority }}</li>
        </ul>
        <p class="recommendation">
          <strong>Recommended Strategy:</strong> {{ swarmRecommendation.strategy }}
        </p>
        <p class="reasoning">{{ swarmRecommendation.reasoning }}</p>
        <button @click="applyRecommendation" class="apply-btn">
          Apply Recommendation
        </button>
      </div>
    </div>
  </div>
</template>

<script>
export default {
  name: 'PermutationMatrix',
  
  data() {
    return {
      complexity: 0.5,
      selectedDomain: 'generic',
      priority: 'medium',
      
      stages: ['typer', 'turtle', 'ttl2dspy', 'bitactor', 'erlang', 'ash', 'reactor', 'k8s'],
      
      permutations: [],
      swarmRecommendation: null,
      selectedPermutation: null
    }
  },
  
  mounted() {
    this.generatePermutations()
    this.updateSwarmRecommendation()
  },
  
  methods: {
    generatePermutations() {
      // Generate different permutation strategies
      const strategies = [
        this.createLinearPermutation(),
        this.createSkipOptimizationPermutation(),
        this.createParallelMergePermutation(),
        this.createAdaptiveBranchPermutation(),
        this.createIterativeLoopPermutation(),
        this.createComplexityRoutePermutation(),
        this.createDomainSpecificPermutation(),
        this.createEmergenceGuidedPermutation(),
        this.createHybridPermutation(),
        this.createMinimalPermutation()
      ]
      
      this.permutations = strategies.map((strategy, index) => {
        const activeStages = strategy.path.filter(s => !s.skipped).length
        const totalDuration = strategy.path.reduce((sum, s) => sum + (s.skipped ? 0 : s.duration), 0)
        
        return {
          id: `perm-${index}`,
          ...strategy,
          activeStages,
          totalDuration,
          efficiency: this.calculateEfficiency(activeStages, totalDuration),
          isOptimal: false
        }
      })
      
      // Mark optimal permutation
      const optimal = this.permutations.reduce((best, current) => 
        current.efficiency > best.efficiency ? current : best
      )
      optimal.isOptimal = true
    },
    
    createLinearPermutation() {
      return {
        strategy: 'Linear Sequential',
        optimization80_20: false,
        path: this.stages.map(stage => ({
          name: stage,
          active: true,
          skipped: false,
          duration: this.getBaseDuration(stage)
        }))
      }
    },
    
    createSkipOptimizationPermutation() {
      const criticalStages = ['typer', 'turtle', 'ash', 'k8s']
      return {
        strategy: 'Skip Optimization (80/20)',
        optimization80_20: true,
        path: this.stages.map(stage => ({
          name: stage,
          active: criticalStages.includes(stage),
          skipped: !criticalStages.includes(stage),
          duration: criticalStages.includes(stage) ? this.getBaseDuration(stage) * 0.8 : 0
        }))
      }
    },
    
    createParallelMergePermutation() {
      return {
        strategy: 'Parallel Merge',
        optimization80_20: false,
        path: this.stages.map(stage => ({
          name: stage,
          active: true,
          skipped: false,
          parallel: ['ash', 'ttl2dspy', 'bitactor', 'erlang'].includes(stage),
          duration: this.getBaseDuration(stage) * 0.7
        }))
      }
    },
    
    createAdaptiveBranchPermutation() {
      const complexity = this.complexity
      let selectedStages = []
      
      if (complexity > 0.8) {
        selectedStages = this.stages
      } else if (complexity > 0.5) {
        selectedStages = ['typer', 'turtle', 'ttl2dspy', 'ash', 'reactor', 'k8s']
      } else {
        selectedStages = ['typer', 'turtle', 'ash', 'k8s']
      }
      
      return {
        strategy: 'Adaptive Branch',
        optimization80_20: complexity < 0.5,
        path: this.stages.map(stage => ({
          name: stage,
          active: selectedStages.includes(stage),
          skipped: !selectedStages.includes(stage),
          duration: selectedStages.includes(stage) ? this.getBaseDuration(stage) : 0
        }))
      }
    },
    
    createIterativeLoopPermutation() {
      return {
        strategy: 'Iterative Loop',
        optimization80_20: false,
        path: this.stages.map(stage => ({
          name: stage,
          active: true,
          skipped: false,
          iterative: true,
          duration: this.getBaseDuration(stage) * 1.2
        }))
      }
    },
    
    createComplexityRoutePermutation() {
      const route = this.complexity > 0.7 ? 
        ['typer', 'turtle', 'ttl2dspy', 'bitactor', 'erlang', 'ash', 'reactor', 'k8s'] :
        ['typer', 'turtle', 'ash', 'reactor', 'k8s']
        
      return {
        strategy: 'Complexity Route',
        optimization80_20: this.complexity < 0.5,
        path: this.stages.map(stage => ({
          name: stage,
          active: route.includes(stage),
          skipped: !route.includes(stage),
          duration: route.includes(stage) ? this.getBaseDuration(stage) : 0
        }))
      }
    },
    
    createDomainSpecificPermutation() {
      let route = []
      
      switch(this.selectedDomain) {
        case 'cybersecurity':
          route = ['typer', 'turtle', 'ttl2dspy', 'bitactor', 'ash', 'reactor', 'erlang', 'k8s']
          break
        case 'finance':
          route = ['typer', 'turtle', 'ash', 'reactor', 'erlang', 'k8s']
          break
        case 'healthcare':
          route = ['typer', 'turtle', 'ash', 'k8s']
          break
        default:
          route = this.stages
      }
      
      return {
        strategy: `Domain: ${this.selectedDomain}`,
        optimization80_20: this.selectedDomain === 'healthcare',
        path: this.stages.map(stage => ({
          name: stage,
          active: route.includes(stage),
          skipped: !route.includes(stage),
          duration: route.includes(stage) ? this.getBaseDuration(stage) : 0
        }))
      }
    },
    
    createEmergenceGuidedPermutation() {
      // Swarm intelligence decides optimal path
      const emergentPath = this.calculateEmergentPath()
      
      return {
        strategy: 'Emergence Guided',
        optimization80_20: true,
        path: this.stages.map(stage => ({
          name: stage,
          active: emergentPath.includes(stage),
          skipped: !emergentPath.includes(stage),
          emergent: true,
          duration: emergentPath.includes(stage) ? this.getBaseDuration(stage) * 0.9 : 0
        }))
      }
    },
    
    createHybridPermutation() {
      // Combines multiple strategies
      const critical = ['typer', 'turtle', 'ash', 'k8s']
      const parallel = ['ttl2dspy', 'bitactor']
      
      return {
        strategy: 'Hybrid Optimization',
        optimization80_20: true,
        path: this.stages.map(stage => ({
          name: stage,
          active: critical.includes(stage) || (this.complexity > 0.6 && parallel.includes(stage)),
          skipped: !(critical.includes(stage) || (this.complexity > 0.6 && parallel.includes(stage))),
          parallel: parallel.includes(stage),
          duration: this.getBaseDuration(stage) * (critical.includes(stage) ? 0.8 : 0.6)
        }))
      }
    },
    
    createMinimalPermutation() {
      return {
        strategy: 'Minimal Path',
        optimization80_20: true,
        path: this.stages.map(stage => ({
          name: stage,
          active: ['typer', 'turtle', 'k8s'].includes(stage),
          skipped: !['typer', 'turtle', 'k8s'].includes(stage),
          duration: ['typer', 'turtle', 'k8s'].includes(stage) ? this.getBaseDuration(stage) * 0.7 : 0
        }))
      }
    },
    
    calculateEmergentPath() {
      // Swarm intelligence algorithm
      const baseStages = ['typer', 'turtle', 'k8s']
      const additionalStages = []
      
      if (this.complexity > 0.7) {
        additionalStages.push('ttl2dspy', 'bitactor')
      }
      
      if (this.selectedDomain === 'cybersecurity') {
        additionalStages.push('ash', 'reactor')
      } else if (this.complexity > 0.5) {
        additionalStages.push('ash')
      }
      
      if (this.priority === 'critical' || this.priority === 'high') {
        additionalStages.push('erlang')
      }
      
      return [...new Set([...baseStages, ...additionalStages])]
    },
    
    getBaseDuration(stage) {
      const durations = {
        typer: 50,
        turtle: 30,
        ttl2dspy: 100,
        bitactor: 200,
        erlang: 100,
        ash: 150,
        reactor: 100,
        k8s: 70
      }
      return durations[stage] || 50
    },
    
    calculateEfficiency(activeStages, totalDuration) {
      // Efficiency based on stage reduction and speed
      const stageReduction = 1 - (activeStages / 8)
      const speedFactor = 1000 / (totalDuration + 100)
      return (stageReduction * 0.6 + speedFactor * 0.4)
    },
    
    getStageInitial(stage) {
      const initials = {
        typer: 'T',
        turtle: 'Tu',
        ttl2dspy: 'D',
        bitactor: 'B',
        erlang: 'E',
        ash: 'A',
        reactor: 'R',
        k8s: 'K'
      }
      return initials[stage] || stage[0].toUpperCase()
    },
    
    updatePermutations() {
      this.generatePermutations()
      this.updateSwarmRecommendation()
    },
    
    updateSwarmRecommendation() {
      // Swarm intelligence recommendation logic
      let recommendedStrategy = 'Linear Sequential'
      let reasoning = 'Default sequential processing'
      
      if (this.complexity < 0.3 && this.priority !== 'critical') {
        recommendedStrategy = 'Skip Optimization (80/20)'
        reasoning = 'Low complexity allows for significant optimization with 80/20 principle'
      } else if (this.complexity > 0.7 && this.selectedDomain === 'cybersecurity') {
        recommendedStrategy = 'Domain: cybersecurity'
        reasoning = 'High complexity cybersecurity requires comprehensive analysis'
      } else if (this.priority === 'critical') {
        recommendedStrategy = 'Parallel Merge'
        reasoning = 'Critical priority benefits from parallel processing'
      } else if (this.complexity > 0.5 && this.complexity < 0.8) {
        recommendedStrategy = 'Emergence Guided'
        reasoning = 'Medium complexity is ideal for swarm intelligence optimization'
      }
      
      this.swarmRecommendation = {
        strategy: recommendedStrategy,
        reasoning
      }
    },
    
    selectPermutation(permutation) {
      this.selectedPermutation = permutation
      this.$emit('permutation-selected', permutation)
    },
    
    applyRecommendation() {
      const recommended = this.permutations.find(p => 
        p.strategy === this.swarmRecommendation.strategy
      )
      if (recommended) {
        this.selectPermutation(recommended)
      }
    }
  }
}
</script>

<style scoped>
.permutation-matrix {
  padding: 2rem;
  background: #0a0a0a;
  color: #e0e0e0;
}

.permutation-matrix h2 {
  margin-bottom: 2rem;
  font-size: 2rem;
}

.matrix-controls {
  display: flex;
  gap: 2rem;
  margin-bottom: 2rem;
  padding: 1.5rem;
  background: #1a1a1a;
  border-radius: 12px;
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

.control-group input[type="range"] {
  width: 150px;
}

.control-group select {
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

.permutation-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
  gap: 1.5rem;
  margin-bottom: 2rem;
}

.permutation-card {
  background: #1a1a1a;
  border: 2px solid #333;
  border-radius: 12px;
  padding: 1.5rem;
  cursor: pointer;
  transition: all 0.3s ease;
}

.permutation-card:hover {
  border-color: #0088ff;
  transform: translateY(-2px);
}

.permutation-card.optimal {
  border-color: #00ff88;
  background: #1a2a1a;
}

.card-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1rem;
}

.strategy-name {
  font-weight: 600;
}

.efficiency-badge {
  background: #00ff88;
  color: #000;
  padding: 0.25rem 0.75rem;
  border-radius: 20px;
  font-weight: 700;
  font-size: 0.9rem;
}

.path-preview {
  display: flex;
  gap: 0.25rem;
  margin-bottom: 1rem;
  padding: 1rem;
  background: #0a0a0a;
  border-radius: 8px;
}

.stage-dot {
  width: 28px;
  height: 28px;
  border-radius: 50%;
  background: #2a2a2a;
  border: 2px solid #444;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 0.75rem;
  font-weight: 600;
}

.stage-dot.active {
  background: #00ff88;
  color: #000;
  border-color: #00ff88;
}

.stage-dot.skipped {
  opacity: 0.3;
  text-decoration: line-through;
}

.metrics {
  display: flex;
  gap: 1rem;
}

.metric-item {
  display: flex;
  flex-direction: column;
  align-items: center;
}

.metric-item .label {
  font-size: 0.8rem;
  color: #888;
}

.metric-item .value {
  font-weight: 600;
  color: #e0e0e0;
}

.swarm-recommendation {
  background: #1a1a1a;
  border: 2px solid #0088ff;
  border-radius: 16px;
  padding: 2rem;
}

.swarm-recommendation h3 {
  margin-bottom: 1rem;
  color: #0088ff;
}

.recommendation-content ul {
  margin: 1rem 0;
  padding-left: 2rem;
}

.recommendation {
  margin: 1rem 0;
  font-size: 1.2rem;
  color: #00ff88;
}

.reasoning {
  color: #888;
  font-style: italic;
  margin-bottom: 1rem;
}

.apply-btn {
  background: #0088ff;
  color: #fff;
  border: none;
  padding: 0.75rem 2rem;
  border-radius: 8px;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
}

.apply-btn:hover {
  background: #0066cc;
  transform: translateY(-2px);
}
</style>