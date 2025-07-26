import { describe, it, expect, beforeEach, vi, afterEach } from 'vitest'
import { mount } from '@vue/test-utils'
import UltrathinkPipelineConnector from '../../../nuxt_ui/components/UltrathinkPipelineConnector.vue'

describe('UltrathinkPipelineConnector', () => {
  let wrapper
  let mockWebSocket

  beforeEach(() => {
    mockWebSocket = testHelpers.mockWebSocketConnection()
    global.WebSocket = vi.fn(() => mockWebSocket)
    
    wrapper = mount(UltrathinkPipelineConnector, {
      global: {
        mocks: {
          $emit: vi.fn()
        }
      }
    })
  })

  afterEach(() => {
    if (wrapper) {
      wrapper.unmount()
    }
  })

  describe('Component Initialization', () => {
    it('should render with default 80/20 optimization mode', () => {
      expect(wrapper.find('.ultrathink-pipeline-connector').exists()).toBe(true)
      expect(wrapper.vm.optimizationStrategy).toBe('skip_non_critical')
      expect(wrapper.vm.selectedDomain).toBe('cybersecurity')
    })

    it('should initialize all pipeline stages', () => {
      expect(wrapper.vm.pipelineStages).toHaveLength(8)
      
      const criticalStages = wrapper.vm.pipelineStages.filter(s => s.critical)
      expect(criticalStages).toHaveLength(6) // typer, turtle, bitactor, ash, reactor, k8s
      
      const nonCriticalStages = wrapper.vm.pipelineStages.filter(s => !s.critical)
      expect(nonCriticalStages).toHaveLength(2) // ttl2dspy, erlang
    })

    it('should setup WebSocket connection on mount', () => {
      expect(global.WebSocket).toHaveBeenCalledWith('ws://localhost:4000/socket/websocket')
      expect(wrapper.vm.ws).toBe(mockWebSocket)
    })
  })

  describe('80/20 Optimization', () => {
    it('should filter stages based on optimization strategy', async () => {
      await wrapper.vm.updateOptimizationMatrix()
      
      const optimizedFlow = wrapper.vm.optimizedFlow
      const criticalStages = ['typer', 'turtle', 'ash', 'reactor', 'k8s']
      
      // In skip_non_critical mode, should only include critical stages
      optimizedFlow.forEach(stage => {
        expect(criticalStages.includes(stage.id)).toBe(true)
      })
    })

    it('should apply time optimization to non-critical stages', async () => {
      wrapper.vm.optimizationStrategy = 'skip_non_critical'
      await wrapper.vm.updateOptimizationMatrix()
      
      const optimizedStages = wrapper.vm.optimizedFlow.filter(s => s.optimized)
      expect(optimizedStages.length).toBeGreaterThan(0)
      
      optimizedStages.forEach(stage => {
        expect(stage.estimatedDuration).toBeLessThan(stage.baseDuration || 100)
      })
    })

    it('should calculate correct estimated duration for optimized flow', async () => {
      const originalStrategy = wrapper.vm.optimizationStrategy
      wrapper.vm.optimizationStrategy = 'skip_non_critical'
      await wrapper.vm.updateOptimizationMatrix()
      
      const optimizedDuration = wrapper.vm.optimizedFlow.reduce(
        (sum, stage) => sum + stage.estimatedDuration, 0
      )
      
      wrapper.vm.optimizationStrategy = 'adaptive_routing'
      await wrapper.vm.updateOptimizationMatrix()
      
      const fullDuration = wrapper.vm.optimizedFlow.reduce(
        (sum, stage) => sum + stage.estimatedDuration, 0
      )
      
      expect(optimizedDuration).toBeLessThan(fullDuration)
    })
  })

  describe('Pipeline Execution', () => {
    it('should execute optimized pipeline with stage notifications', async () => {
      const mockEmit = vi.fn()
      wrapper.vm.$emit = mockEmit
      
      // Mock successful stage execution
      wrapper.vm.executeStageWithProgress = vi.fn().mockResolvedValue()
      wrapper.vm.generateStageOutputs = vi.fn().mockReturnValue([
        { type: 'processed_data', count: 1 }
      ])
      
      await wrapper.vm.executeOptimizedPipeline()
      
      expect(wrapper.vm.currentExecution).toBeTruthy()
      expect(wrapper.vm.currentExecution.status).toBe('completed')
      expect(mockEmit).toHaveBeenCalledWith('pipeline-executed', expect.any(Object))
    })

    it('should emit stage events during execution', async () => {
      const stageData = {
        id: 'ash',
        name: 'Ash',
        estimatedDuration: 150,
        optimized: true
      }
      
      const emitSpy = vi.spyOn(wrapper.vm, 'emitNotificationEvent')
      
      wrapper.vm.handlePipelineStageStarted(stageData)
      
      expect(emitSpy).toHaveBeenCalledWith(
        expect.objectContaining({
          type: 'pipeline_events',
          level: 'info',
          message: 'Pipeline stage started: Ash'
        })
      )
    })

    it('should handle stage completion with optimization metrics', async () => {
      const stageData = {
        id: 'reactor',
        name: 'Reactor',
        duration: 100,
        estimatedDuration: 120,
        efficiency: 88,
        optimized: true,
        outputs: [{ type: 'reactor_workflow', count: 2 }]
      }
      
      const emitSpy = vi.spyOn(wrapper.vm, 'emitNotificationEvent')
      
      wrapper.vm.handlePipelineStageCompleted(stageData)
      
      expect(emitSpy).toHaveBeenCalledWith(
        expect.objectContaining({
          type: 'reactor_workflows',
          level: 'success',
          message: 'Reactor workflows generated successfully'
        })
      )
    })
  })

  describe('Permutation Generation', () => {
    it('should generate optimized permutations', async () => {
      wrapper.vm.generatePermutations()
      
      const permutations = wrapper.vm.permutationResults
      expect(permutations.length).toBeGreaterThan(0)
      
      // Should have optimal permutation marked
      const optimal = permutations.find(p => p.isOptimal)
      expect(optimal).toBeTruthy()
      expect(optimal.efficiency).toBeGreaterThan(80)
    })

    it('should calculate efficiency correctly', () => {
      const strategy = 'skip_non_critical'
      const efficiency = wrapper.vm.calculatePermutationEfficiency(strategy)
      
      expect(efficiency).toBeGreaterThanOrEqual(70)
      expect(efficiency).toBeLessThanOrEqual(100)
    })

    it('should filter permutation path based on strategy', () => {
      const minimalPath = wrapper.vm.generatePermutationPath('minimal_path')
      const essentialStages = ['Typer', 'Turtle', 'Ash', 'Kubernetes']
      
      const activeStages = minimalPath.filter(stage => !stage.skipped)
      
      activeStages.forEach(stage => {
        expect(essentialStages.includes(stage.name)).toBe(true)
      })
    })
  })

  describe('Swarm Intelligence Integration', () => {
    it('should track optimization metrics', () => {
      const result = testHelpers.createMockOptimization('parallel_execution')
      
      wrapper.vm.handleOptimizationApplied({
        type: 'parallel_execution',
        improvement: 25,
        timeSaved: 150,
        stagesOptimized: 3
      })
      
      expect(wrapper.vm.swarmMetrics.optimizationsApplied).toBeGreaterThan(0)
    })

    it('should apply swarm recommendations', async () => {
      const recommendation = {
        id: 'skip_ttl2dspy',
        type: 'skip_non_critical'
      }
      
      await wrapper.vm.applyRecommendation(recommendation)
      
      expect(wrapper.vm.optimizationStrategy).toBe('skip_non_critical')
      expect(wrapper.vm.complexityLevel).toBe(4)
    })

    it('should update metrics in real-time', () => {
      const initialOptimizations = wrapper.vm.swarmMetrics.optimizationsApplied
      
      wrapper.vm.updateSwarmMetrics()
      
      expect(wrapper.vm.swarmMetrics.optimizationsApplied).toBeGreaterThanOrEqual(initialOptimizations)
    })
  })

  describe('Notification Integration', () => {
    it('should emit pipeline connected event with correct structure', () => {
      const mockEmit = vi.fn()
      wrapper.vm.$emit = mockEmit
      
      const notificationData = {
        type: 'pipeline_events',
        level: 'info',
        message: 'Test notification'
      }
      
      wrapper.vm.emitNotificationEvent(notificationData)
      
      expect(mockEmit).toHaveBeenCalledWith('pipeline-connected', 
        expect.objectContaining({
          stages: expect.any(Array),
          optimizations: expect.arrayContaining([notificationData]),
          selectedPermutation: expect.any(Object),
          config: expect.any(Object)
        })
      )
    })

    it('should handle real-time pipeline state sync', () => {
      const stateUpdate = {
        activeStages: ['ash', 'reactor'],
        completedStages: ['typer', 'turtle']
      }
      
      wrapper.vm.syncExternalPipelineState(stateUpdate)
      
      const ashStage = wrapper.vm.pipelineStages.find(s => s.id === 'ash')
      const typerStage = wrapper.vm.pipelineStages.find(s => s.id === 'typer')
      
      expect(ashStage.active).toBe(true)
      expect(ashStage.status).toBe('Executing')
      expect(typerStage.status).toBe('Completed')
    })
  })

  describe('Performance Optimization', () => {
    it('should calculate time saved correctly', () => {
      wrapper.vm.optimizedFlow = [
        { estimatedDuration: 50 },
        { estimatedDuration: 30 },
        { estimatedDuration: 150 }
      ]
      
      const timeSaved = wrapper.vm.calculateTimeSaved()
      expect(timeSaved).toBeGreaterThanOrEqual(0)
    })

    it('should optimize based on domain', async () => {
      wrapper.vm.selectedDomain = 'healthcare'
      await wrapper.vm.updateOptimizationMatrix()
      
      // Healthcare should have minimal stages
      const healthcareFlow = wrapper.vm.optimizedFlow
      expect(healthcareFlow.length).toBeLessThanOrEqual(5)
    })

    it('should handle complexity level changes', async () => {
      wrapper.vm.complexityLevel = 3
      await wrapper.vm.updateOptimizationMatrix()
      
      const lowComplexityFlow = wrapper.vm.optimizedFlow
      
      wrapper.vm.complexityLevel = 9
      await wrapper.vm.updateOptimizationMatrix()
      
      const highComplexityFlow = wrapper.vm.optimizedFlow
      
      expect(highComplexityFlow.length).toBeGreaterThanOrEqual(lowComplexityFlow.length)
    })
  })

  describe('Error Handling', () => {
    it('should handle pipeline execution errors gracefully', async () => {
      wrapper.vm.executeStageWithProgress = vi.fn().mockRejectedValue(new Error('Stage failed'))
      
      await wrapper.vm.executeOptimizedPipeline()
      
      expect(wrapper.vm.currentExecution.status).toBe('failed')
      expect(wrapper.vm.executing).toBe(false)
    })

    it('should emit error notifications for failed stages', () => {
      const emitSpy = vi.spyOn(wrapper.vm, 'emitNotificationEvent')
      
      wrapper.vm.handlePipelineStageError({
        id: 'ash',
        name: 'Ash',
        error: 'Resource generation failed'
      })
      
      expect(emitSpy).toHaveBeenCalledWith(
        expect.objectContaining({
          type: 'error_alerts',
          level: 'error',
          message: 'Pipeline stage failed: Ash - Resource generation failed'
        })
      )
    })

    it('should handle WebSocket connection failures', () => {
      const consoleSpy = vi.spyOn(console, 'error')
      
      // Simulate WebSocket error
      if (mockWebSocket.onerror) {
        mockWebSocket.onerror(new Error('Connection failed'))
      }
      
      expect(consoleSpy).toHaveBeenCalledWith(
        expect.stringContaining('WebSocket connection error')
      )
    })
  })

  describe('80/20 Optimization Validation', () => {
    it('should meet 80/20 optimization criteria', async () => {
      wrapper.vm.optimizationStrategy = 'skip_non_critical'
      await wrapper.vm.updateOptimizationMatrix()
      
      const optimizedData = {
        stages: wrapper.vm.optimizedFlow
      }
      
      expect(testHelpers.is80_20Optimized(optimizedData)).toBe(true)
    })

    it('should show performance improvement with optimization', async () => {
      // Measure baseline performance
      wrapper.vm.optimizationStrategy = 'adaptive_routing'
      await wrapper.vm.updateOptimizationMatrix()
      const baselineDuration = wrapper.vm.optimizedFlow.reduce(
        (sum, stage) => sum + stage.estimatedDuration, 0
      )
      
      // Apply 80/20 optimization
      wrapper.vm.optimizationStrategy = 'skip_non_critical'
      await wrapper.vm.updateOptimizationMatrix()
      const optimizedDuration = wrapper.vm.optimizedFlow.reduce(
        (sum, stage) => sum + stage.estimatedDuration, 0
      )
      
      expect(testHelpers.assertPerformanceImprovement(
        baselineDuration, 
        optimizedDuration, 
        0.2 // 20% improvement threshold
      )).toBe(true)
    })
  })
})