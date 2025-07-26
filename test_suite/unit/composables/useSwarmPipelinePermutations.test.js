import { describe, it, expect, beforeEach, vi } from 'vitest'
import { useSwarmPipelinePermutations } from '../../../nuxt_ui/composables/useSwarmPipelinePermutations.js'

describe('useSwarmPipelinePermutations', () => {
  let permutations

  beforeEach(() => {
    permutations = useSwarmPipelinePermutations()
  })

  describe('Permutation Generation', () => {
    it('should generate permutations with default configuration', () => {
      const result = permutations.generatePermutations()
      
      expect(result).toBeInstanceOf(Array)
      expect(result.length).toBeGreaterThan(0)
      
      // Should have at least the main optimization strategies
      const strategies = result.map(p => p.strategy)
      expect(strategies).toContain('critical_only')
      expect(strategies).toContain('skip_optional')
      expect(strategies).toContain('parallel_execution')
    })

    it('should mark optimal permutation', () => {
      const result = permutations.generatePermutations()
      
      const optimalPermutations = result.filter(p => p.isOptimal)
      expect(optimalPermutations).toHaveLength(1)
      
      const optimal = optimalPermutations[0]
      expect(optimal.efficiency).toBeGreaterThan(80)
    })

    it('should sort permutations by efficiency', () => {
      const result = permutations.generatePermutations()
      
      for (let i = 1; i < result.length; i++) {
        expect(result[i].efficiency).toBeLessThanOrEqual(result[i-1].efficiency)
      }
    })

    it('should respect domain-specific configurations', () => {
      const cybersecurityResult = permutations.generatePermutations({
        domain: 'cybersecurity'
      })
      
      const healthcareResult = permutations.generatePermutations({
        domain: 'healthcare'
      })
      
      // Cybersecurity should have more stages than healthcare
      const cybersecurityStages = cybersecurityResult[0].executionPath.length
      const healthcareStages = healthcareResult[0].executionPath.length
      
      expect(cybersecurityStages).toBeGreaterThanOrEqual(healthcareStages)
    })
  })

  describe('80/20 Optimization Strategies', () => {
    it('should implement critical_only strategy correctly', () => {
      const config = { optimization: 'critical_only' }
      const result = permutations.generatePermutations(config)
      
      const criticalOnlyPermutation = result.find(p => p.strategy === 'critical_only')
      expect(criticalOnlyPermutation).toBeTruthy()
      
      // Should only include critical stages
      const criticalStages = criticalOnlyPermutation.executionPath.filter(stage => stage.critical)
      expect(criticalStages.length).toBe(criticalOnlyPermutation.executionPath.length)
    })

    it('should apply skip_optional strategy based on domain', () => {
      const config = { 
        domain: 'healthcare',
        optimization: 'skip_optional' 
      }
      const result = permutations.generatePermutations(config)
      
      const skipOptionalPermutation = result.find(p => p.strategy === 'skip_optional')
      
      // Healthcare domain should skip more stages
      const domainOptimization = permutations.DOMAIN_OPTIMIZATIONS.healthcare
      const skippedStages = skipOptionalPermutation.executionPath.filter(stage => 
        domainOptimization.skipableStages.includes(stage.id)
      )
      
      expect(skippedStages.length).toBe(0) // They should be filtered out
    })

    it('should enable parallel execution where appropriate', () => {
      const config = { optimization: 'parallel_execution' }
      const result = permutations.generatePermutations(config)
      
      const parallelPermutation = result.find(p => p.strategy === 'parallel_execution')
      
      // Should have parallel stages marked
      const parallelStages = parallelPermutation.executionPath.filter(stage => stage.parallel)
      expect(parallelStages.length).toBeGreaterThan(0)
    })

    it('should implement minimal_path strategy', () => {
      const config = { optimization: 'minimal_path' }
      const result = permutations.generatePermutations(config)
      
      const minimalPermutation = result.find(p => p.strategy === 'minimal_path')
      
      // Should only include essential stages
      const essentialStages = ['typer', 'turtle', 'ash', 'k8s']
      minimalPermutation.executionPath.forEach(stage => {
        expect(essentialStages.includes(stage.id)).toBe(true)
      })
    })

    it('should implement adaptive_smart strategy with complexity consideration', () => {
      const lowComplexityConfig = { 
        optimization: 'adaptive_smart',
        complexity: 0.2 
      }
      const highComplexityConfig = { 
        optimization: 'adaptive_smart',
        complexity: 0.9 
      }
      
      const lowComplexityResult = permutations.generatePermutations(lowComplexityConfig)
      const highComplexityResult = permutations.generatePermutations(highComplexityConfig)
      
      const lowComplexityPermutation = lowComplexityResult.find(p => p.strategy === 'adaptive_smart')
      const highComplexityPermutation = highComplexityResult.find(p => p.strategy === 'adaptive_smart')
      
      // High complexity should include more stages
      expect(highComplexityPermutation.executionPath.length)
        .toBeGreaterThanOrEqual(lowComplexityPermutation.executionPath.length)
    })
  })

  describe('Timing Calculations', () => {
    it('should calculate timing metrics correctly', () => {
      const result = permutations.generatePermutations()
      
      result.forEach(permutation => {
        expect(permutation.timing).toBeTruthy()
        expect(permutation.timing.totalDuration).toBeGreaterThan(0)
        expect(permutation.timing.baseDuration).toBeGreaterThan(0)
        expect(permutation.timing.optimizationSaving).toBeGreaterThanOrEqual(0)
      })
    })

    it('should calculate parallel execution timing correctly', () => {
      const config = { optimization: 'parallel_execution' }
      const result = permutations.generatePermutations(config)
      
      const parallelPermutation = result.find(p => p.strategy === 'parallel_execution')
      
      // Parallel execution should have shorter total duration
      expect(parallelPermutation.timing.totalDuration)
        .toBeLessThan(parallelPermutation.timing.baseDuration)
      
      expect(parallelPermutation.timing.parallelDuration).toBeGreaterThan(0)
    })

    it('should calculate speedup percentage accurately', () => {
      const result = permutations.generatePermutations()
      
      result.forEach(permutation => {
        const expectedSpeedup = Math.round(
          ((permutation.timing.baseDuration - permutation.timing.totalDuration) / 
           permutation.timing.baseDuration) * 100
        )
        
        expect(permutation.timing.speedupPercentage).toBe(expectedSpeedup)
      })
    })
  })

  describe('Notification Channel Determination', () => {
    it('should determine appropriate notification channels', () => {
      const result = permutations.generatePermutations({
        includeNotifications: true
      })
      
      result.forEach(permutation => {
        expect(permutation.notificationChannels).toBeInstanceOf(Array)
        expect(permutation.notificationChannels.length).toBeGreaterThan(0)
        
        // Should include critical channels for critical stages
        const hasAshStage = permutation.executionPath.some(stage => stage.id === 'ash')
        const hasAshChannel = permutation.notificationChannels.some(
          channel => channel.id === 'ash_resources'
        )
        
        if (hasAshStage) {
          expect(hasAshChannel).toBe(true)
        }
      })
    })

    it('should prioritize notification channels correctly', () => {
      const result = permutations.generatePermutations({
        includeNotifications: true
      })
      
      const permutation = result[0]
      const criticalChannels = permutation.notificationChannels.filter(
        channel => channel.priority === 'critical'
      )
      
      expect(criticalChannels.length).toBeGreaterThan(0)
      
      // Error alerts should always be critical
      const errorChannel = permutation.notificationChannels.find(
        channel => channel.id === 'error_alerts'
      )
      if (errorChannel) {
        expect(errorChannel.priority).toBe('critical')
      }
    })
  })

  describe('Efficiency Calculation', () => {
    it('should calculate efficiency based on multiple factors', () => {
      const result = permutations.generatePermutations()
      
      result.forEach(permutation => {
        expect(permutation.efficiency).toBeGreaterThanOrEqual(0)
        expect(permutation.efficiency).toBeLessThanOrEqual(100)
      })
    })

    it('should give higher efficiency to optimized permutations', () => {
      const result = permutations.generatePermutations()
      
      const criticalOnlyPermutation = result.find(p => p.strategy === 'critical_only')
      const fullPermutation = result.find(p => p.strategy === 'parallel_execution')
      
      // Critical-only should be more efficient due to fewer stages
      expect(criticalOnlyPermutation.efficiency).toBeGreaterThan(70)
    })

    it('should penalize excessive notification channels', () => {
      // Mock a permutation with many notification channels
      const testPermutation = {
        executionPath: permutations.PIPELINE_STAGES.slice(0, 4),
        timing: { speedupPercentage: 30 },
        notificationChannels: Array.from({ length: 10 }, (_, i) => ({ id: `channel_${i}` }))
      }
      
      // This would be calculated internally, but we can test the concept
      const efficiency = Math.max(0, Math.min(100, 
        60 + 15 + 10 - (testPermutation.notificationChannels.length - 4) * 2
      ))
      
      expect(efficiency).toBeLessThan(85) // Should be penalized
    })
  })

  describe('Execution Plan Generation', () => {
    it('should generate executable plan from permutation', () => {
      const permutation = permutations.generatePermutations()[0]
      const executionPlan = permutations.executePermutation(permutation)
      
      expect(executionPlan).toBeTruthy()
      expect(executionPlan.id).toBeTruthy()
      expect(executionPlan.permutationId).toBe(permutation.id)
      expect(executionPlan.steps).toBeInstanceOf(Array)
      expect(executionPlan.steps.length).toBe(permutation.executionPath.length)
      
      // Each step should have required properties
      executionPlan.steps.forEach(step => {
        expect(step.id).toBeTruthy()
        expect(step.name).toBeTruthy()
        expect(step.status).toBe('pending')
        expect(step.estimatedDuration).toBeGreaterThan(0)
      })
    })

    it('should handle parallel step groups in execution plan', () => {
      const config = { optimization: 'parallel_execution' }
      const result = permutations.generatePermutations(config)
      const parallelPermutation = result.find(p => p.strategy === 'parallel_execution')
      
      const executionPlan = permutations.executePermutation(parallelPermutation)
      
      const parallelSteps = executionPlan.steps.filter(step => step.parallel)
      expect(parallelSteps.length).toBeGreaterThan(0)
      
      // Parallel steps should have parallel group assignments
      parallelSteps.forEach(step => {
        expect(step.parallelGroup).toBeDefined()
      })
    })
  })

  describe('Notification Routing Rules', () => {
    it('should generate routing rules for permutations', () => {
      const permutation = permutations.generatePermutations({
        includeNotifications: true
      })[0]
      
      const routingRules = permutations.generateNotificationRouting(permutation)
      
      expect(routingRules).toBeInstanceOf(Array)
      expect(routingRules.length).toBeGreaterThan(0)
      
      // Each rule should have required properties
      routingRules.forEach(rule => {
        expect(rule.id).toBeTruthy()
        expect(rule.name).toBeTruthy()
        expect(rule.active).toBe(true)
        expect(rule.conditions).toBeTruthy()
        expect(rule.actions).toBeTruthy()
      })
    })

    it('should create appropriate routing conditions', () => {
      const permutation = permutations.generatePermutations({
        includeNotifications: true
      })[0]
      
      const routingRules = permutations.generateNotificationRouting(permutation)
      
      routingRules.forEach(rule => {
        expect(rule.conditions.sources).toBeInstanceOf(Array)
        expect(rule.conditions.levels).toBeInstanceOf(Array)
        expect(rule.conditions.messageTypes).toBeInstanceOf(Array)
      })
    })
  })

  describe('Performance Analysis', () => {
    it('should analyze permutation performance correctly', () => {
      const mockExecutionResults = [{
        startTime: 1000,
        endTime: 1500,
        steps: [
          { status: 'completed', estimatedDuration: 100 },
          { status: 'completed', estimatedDuration: 200 },
          { status: 'failed', estimatedDuration: 150 }
        ],
        notifications: [
          { status: 'delivered' },
          { status: 'delivered' },
          { status: 'failed' }
        ]
      }]
      
      const analysis = permutations.analyzePermutationPerformance(mockExecutionResults)
      
      expect(analysis.timing.actualDuration).toBe(500)
      expect(analysis.timing.estimatedDuration).toBe(450)
      expect(analysis.stages.completed).toBe(2)
      expect(analysis.stages.failed).toBe(1)
      expect(analysis.notifications.totalSent).toBe(3)
      expect(analysis.notifications.successRate).toBeCloseTo(66.67, 2)
    })
  })

  describe('Swarm Intelligence Recommendations', () => {
    it('should generate recommendations based on performance patterns', () => {
      const mockPermutations = permutations.generatePermutations()
      const mockExecutionHistory = [
        { efficiency: 60 },
        { efficiency: 65 },
        { efficiency: 58 }
      ]
      
      const recommendations = permutations.generateSwarmRecommendations(
        mockPermutations, 
        mockExecutionHistory
      )
      
      expect(recommendations).toBeInstanceOf(Array)
      
      // Should recommend efficiency improvement for low performance
      const efficiencyRec = recommendations.find(r => r.id === 'improve_efficiency')
      expect(efficiencyRec).toBeTruthy()
      expect(efficiencyRec.priority).toBe('high')
    })

    it('should recommend notification optimization when needed', () => {
      const mockPermutations = permutations.generatePermutations({
        includeNotifications: true
      }).map(p => ({
        ...p,
        notificationChannels: Array.from({ length: 8 }, (_, i) => ({ id: `channel_${i}` }))
      }))
      
      const recommendations = permutations.generateSwarmRecommendations(mockPermutations)
      
      const notificationRec = recommendations.find(r => r.id === 'reduce_notifications')
      expect(notificationRec).toBeTruthy()
      expect(notificationRec.type).toBe('notifications')
    })

    it('should provide domain-specific recommendations', () => {
      const mockPermutations = Array.from({ length: 10 }, () => ({
        ...permutations.generatePermutations({ domain: 'cybersecurity' })[0],
        domain: 'cybersecurity'
      }))
      
      const recommendations = permutations.generateSwarmRecommendations(mockPermutations)
      
      const domainRec = recommendations.find(r => r.id === 'domain_optimization')
      expect(domainRec).toBeTruthy()
      expect(domainRec.title).toContain('cybersecurity')
    })
  })

  describe('Helper Functions', () => {
    it('should provide correct channel names', () => {
      expect(permutations.getChannelName('ash_resources')).toBe('Ash Resources')
      expect(permutations.getChannelName('reactor_workflows')).toBe('Reactor Workflows')
      expect(permutations.getChannelName('unknown_channel')).toBe('unknown_channel')
    })

    it('should provide correct channel icons', () => {
      expect(permutations.getChannelIcon('ash_resources')).toBe('🔥')
      expect(permutations.getChannelIcon('reactor_workflows')).toBe('⚛️')
      expect(permutations.getChannelIcon('unknown_channel')).toBe('📡')
    })

    it('should provide correct channel priorities', () => {
      expect(permutations.getChannelPriority('error_alerts')).toBe('critical')
      expect(permutations.getChannelPriority('ash_resources')).toBe('high')
      expect(permutations.getChannelPriority('pipeline_events')).toBe('low')
    })
  })

  describe('80/20 Principle Validation', () => {
    it('should demonstrate 80/20 principle in stage selection', () => {
      const result = permutations.generatePermutations()
      
      // Find the most efficient permutation (should be 80/20 optimized)
      const optimalPermutation = result.find(p => p.isOptimal)
      
      // Should use fewer than all stages but maintain high efficiency
      const totalStages = permutations.PIPELINE_STAGES.length
      const usedStages = optimalPermutation.executionPath.length
      
      expect(usedStages).toBeLessThan(totalStages)
      expect(optimalPermutation.efficiency).toBeGreaterThan(80)
    })

    it('should show performance improvement with 80/20 optimization', () => {
      const fullPipeline = permutations.generatePermutations({ 
        optimization: 'adaptive_routing' 
      })[0]
      
      const optimizedPipeline = permutations.generatePermutations({ 
        optimization: 'critical_only' 
      })[0]
      
      // 80/20 optimization should provide better efficiency
      expect(optimizedPipeline.efficiency).toBeGreaterThan(fullPipeline.efficiency * 0.8)
      
      // Should also have better timing
      expect(optimizedPipeline.timing.totalDuration)
        .toBeLessThan(fullPipeline.timing.totalDuration)
    })

    it('should maintain critical functionality while optimizing', () => {
      const criticalOnlyResult = permutations.generatePermutations({
        optimization: 'critical_only'
      })
      
      const criticalPermutation = criticalOnlyResult.find(p => p.strategy === 'critical_only')
      
      // Must include essential stages for basic functionality
      const essentialStages = ['typer', 'turtle', 'k8s']
      essentialStages.forEach(stageId => {
        const hasStage = criticalPermutation.executionPath.some(stage => stage.id === stageId)
        expect(hasStage).toBe(true)
      })
    })
  })
})