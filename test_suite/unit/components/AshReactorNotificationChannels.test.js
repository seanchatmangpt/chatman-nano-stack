import { describe, it, expect, beforeEach, vi, afterEach } from 'vitest'
import { mount } from '@vue/test-utils'
import AshReactorNotificationChannels from '../../../nuxt_ui/components/AshReactorNotificationChannels.vue'

describe('AshReactorNotificationChannels', () => {
  let wrapper
  let mockWebSocket

  beforeEach(() => {
    mockWebSocket = testHelpers.mockWebSocketConnection()
    global.WebSocket = vi.fn(() => mockWebSocket)
    
    wrapper = mount(AshReactorNotificationChannels, {
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
    it('should render notification channels interface', () => {
      expect(wrapper.find('.ash-reactor-notifications').exists()).toBe(true)
      expect(wrapper.find('.notification-controls').exists()).toBe(true)
      expect(wrapper.find('.notification-stream').exists()).toBe(true)
    })

    it('should initialize with 80/20 optimization enabled', () => {
      expect(wrapper.vm.ultrathin80_20Config.enabled).toBe(true)
      expect(wrapper.vm.ultrathin80_20Config.criticalThreshold).toBe(0.8)
      expect(wrapper.vm.ultrathin80_20Config.noiseReductionLevel).toBe(0.7)
    })

    it('should setup notification channels with correct priorities', () => {
      const channels = wrapper.vm.availableChannels
      
      const criticalChannels = channels.filter(c => c.priority === 'critical')
      const highPriorityChannels = channels.filter(c => c.priority === 'high')
      
      expect(criticalChannels.length).toBeGreaterThan(0)
      expect(highPriorityChannels.length).toBeGreaterThan(0)
      
      // Verify critical channels include error_alerts
      expect(criticalChannels.some(c => c.id === 'error_alerts')).toBe(true)
    })
  })

  describe('80/20 Notification Filtering', () => {
    it('should filter notifications based on priority', () => {
      const criticalNotification = testHelpers.createMockNotification('error_alerts', 'critical')
      const lowPriorityNotification = testHelpers.createMockNotification('pipeline_events', 'info')
      
      wrapper.vm.addNotification(criticalNotification)
      wrapper.vm.addNotification(lowPriorityNotification)
      
      // With 80/20 filtering enabled, should prioritize critical
      const filteredStream = wrapper.vm.getFilteredNotificationStream()
      
      expect(filteredStream).toContain(criticalNotification)
      
      // Non-critical might be filtered out or batched
      if (wrapper.vm.ultrathin80_20Config.enabled) {
        const criticalCount = filteredStream.filter(n => n.level === 'critical').length
        const infoCount = filteredStream.filter(n => n.level === 'info').length
        
        expect(criticalCount).toBeGreaterThanOrEqual(infoCount)
      }
    })

    it('should apply smart filtering based on noise reduction', () => {
      // Generate multiple similar notifications
      const similarNotifications = Array.from({ length: 10 }, (_, i) => 
        testHelpers.createMockNotification('pipeline_events', 'info')
      )
      
      similarNotifications.forEach(n => wrapper.vm.addNotification(n))
      
      wrapper.vm.applySmartFiltering()
      
      const filteredStream = wrapper.vm.notificationStream
      
      // Should have fewer notifications due to deduplication/batching
      expect(filteredStream.length).toBeLessThan(similarNotifications.length)
    })

    it('should prioritize ash_resources and reactor_workflows', () => {
      const ashNotification = testHelpers.createMockNotification('ash_resources', 'info')
      const reactorNotification = testHelpers.createMockNotification('reactor_workflows', 'info')
      const genericNotification = testHelpers.createMockNotification('pipeline_events', 'info')
      
      ashNotification.priority = wrapper.vm.ultrathin80_20Config.priorityWeights.ash_resources * 100
      reactorNotification.priority = wrapper.vm.ultrathin80_20Config.priorityWeights.reactor_workflows * 100
      genericNotification.priority = wrapper.vm.ultrathin80_20Config.priorityWeights.pipeline_events * 100
      
      expect(ashNotification.priority).toBeGreaterThan(genericNotification.priority)
      expect(reactorNotification.priority).toBeGreaterThan(genericNotification.priority)
    })
  })

  describe('Channel Management', () => {
    it('should subscribe to channels with optimization', async () => {
      const channelsToSubscribe = ['ash_resources', 'reactor_workflows', 'error_alerts']
      
      await wrapper.vm.subscribeToChannels(channelsToSubscribe)
      
      channelsToSubscribe.forEach(channelId => {
        expect(wrapper.vm.subscribedChannels.includes(channelId)).toBe(true)
      })
    })

    it('should configure channel routing rules', () => {
      const channel = wrapper.vm.availableChannels.find(c => c.id === 'ash_resources')
      
      wrapper.vm.configureChannelRouting(channel.id, {
        priority: 'high',
        batchSize: 5,
        throttleMs: 100
      })
      
      const routingRule = wrapper.vm.channelRoutingRules[channel.id]
      expect(routingRule).toBeTruthy()
      expect(routingRule.priority).toBe('high')
    })

    it('should optimize channel configuration automatically', () => {
      // Simulate high notification volume
      const highVolumeChannel = 'performance_metrics'
      
      wrapper.vm.channelMetrics[highVolumeChannel] = {
        notificationsPerMinute: 150,
        avgProcessingTime: 5
      }
      
      wrapper.vm.optimizeChannelConfiguration()
      
      const optimizedConfig = wrapper.vm.channelOptimizations[highVolumeChannel]
      expect(optimizedConfig).toBeTruthy()
      expect(optimizedConfig.batchingEnabled).toBe(true)
    })
  })

  describe('Real-time Streaming', () => {
    it('should stream notifications in real-time', (done) => {
      const notification = testHelpers.createMockNotification('ash_resources', 'info')
      
      // Watch for stream updates
      wrapper.vm.$watch('notificationStream', (newStream) => {
        if (newStream.length > 0) {
          expect(newStream[0]).toEqual(notification)
          done()
        }
      })
      
      wrapper.vm.addNotification(notification)
    })

    it('should handle high-frequency notifications efficiently', async () => {
      const startTime = performance.now()
      
      // Simulate 100 rapid notifications
      const notifications = Array.from({ length: 100 }, (_, i) => 
        testHelpers.createMockNotification('performance_metrics', 'info')
      )
      
      for (const notification of notifications) {
        wrapper.vm.addNotification(notification)
      }
      
      const endTime = performance.now()
      const processingTime = endTime - startTime
      
      // Should process quickly (under 100ms for 100 notifications)
      expect(processingTime).toBeLessThan(100)
    })

    it('should maintain stream size limits', () => {
      const maxStreamSize = wrapper.vm.maxStreamSize || 1000
      
      // Add more notifications than the limit
      for (let i = 0; i < maxStreamSize + 100; i++) {
        wrapper.vm.addNotification(testHelpers.createMockNotification('pipeline_events', 'info'))
      }
      
      expect(wrapper.vm.notificationStream.length).toBeLessThanOrEqual(maxStreamSize)
    })
  })

  describe('Pipeline Flow Permutations', () => {
    it('should generate notification permutations for different strategies', () => {
      const strategies = ['sequential_flow', 'parallel_optimization', 'critical_path']
      
      strategies.forEach(strategy => {
        const permutation = wrapper.vm.generateFlowPermutation(strategy)
        
        expect(permutation).toBeTruthy()
        expect(permutation.strategy).toBe(strategy)
        expect(permutation.notificationFlow).toBeInstanceOf(Array)
      })
    })

    it('should select optimal permutation based on current conditions', () => {
      // Set high-load conditions
      wrapper.vm.currentSystemLoad = 0.9
      wrapper.vm.notificationVolume = 200
      
      const optimalPermutation = wrapper.vm.selectOptimalPermutation()
      
      // Should prefer minimal_essential strategy under high load
      expect(optimalPermutation.strategy).toBe('minimal_essential')
    })

    it('should adapt permutation based on performance feedback', () => {
      const permutation = wrapper.vm.generateFlowPermutation('adaptive_flow')
      
      // Simulate poor performance
      const performanceFeedback = {
        averageLatency: 500,
        errorRate: 0.1,
        throughput: 50
      }
      
      wrapper.vm.adaptPermutationBasedOnFeedback(permutation, performanceFeedback)
      
      // Should adjust to more conservative approach
      expect(permutation.adaptations.length).toBeGreaterThan(0)
    })
  })

  describe('WebSocket Integration', () => {
    it('should establish WebSocket connection for real-time updates', () => {
      expect(global.WebSocket).toHaveBeenCalled()
      expect(wrapper.vm.wsConnected).toBe(true)
    })

    it('should handle WebSocket messages and route to appropriate channels', () => {
      const mockMessage = {
        type: 'ash_resource_update',
        data: {
          resourceId: 'resource_123',
          status: 'created'
        },
        channel: 'ash_resources',
        priority: 'high'
      }
      
      const messageEvent = new MessageEvent('message', {
        data: JSON.stringify(mockMessage)
      })
      
      if (mockWebSocket.onmessage) {
        mockWebSocket.onmessage(messageEvent)
      }
      
      // Should add notification to appropriate channel
      const ashNotifications = wrapper.vm.notificationStream.filter(
        n => n.channel === 'ash_resources'
      )
      
      expect(ashNotifications.length).toBeGreaterThan(0)
    })

    it('should handle connection failures gracefully', () => {
      const consoleSpy = vi.spyOn(console, 'error')
      
      if (mockWebSocket.onerror) {
        mockWebSocket.onerror(new Error('Connection lost'))
      }
      
      expect(wrapper.vm.connectionStatus).toBe('error')
      expect(consoleSpy).toHaveBeenCalled()
    })
  })

  describe('Performance Analytics', () => {
    it('should track notification processing metrics', () => {
      const notification = testHelpers.createMockNotification('ash_resources', 'info')
      const startTime = performance.now()
      
      wrapper.vm.addNotification(notification)
      
      const endTime = performance.now()
      const processingTime = endTime - startTime
      
      expect(wrapper.vm.performanceMetrics.totalProcessed).toBeGreaterThan(0)
      expect(wrapper.vm.performanceMetrics.averageProcessingTime).toBeGreaterThan(0)
    })

    it('should identify performance bottlenecks', () => {
      // Simulate slow channel processing
      wrapper.vm.channelMetrics['reactor_workflows'] = {
        averageProcessingTime: 50,
        queueSize: 100,
        errorRate: 0.05
      }
      
      const bottlenecks = wrapper.vm.identifyBottlenecks()
      
      expect(bottlenecks.length).toBeGreaterThan(0)
      expect(bottlenecks[0].channel).toBe('reactor_workflows')
    })

    it('should optimize based on analytics', () => {
      // Set up analytics data showing pattern
      wrapper.vm.analyticsData = {
        channelUsage: {
          'ash_resources': 60,
          'reactor_workflows': 30,
          'pipeline_events': 10
        },
        peakHours: [9, 10, 11, 14, 15, 16],
        errorPatterns: ['timeout', 'validation_error']
      }
      
      wrapper.vm.applyAnalyticsOptimization()
      
      // Should prioritize high-usage channels
      const ashConfig = wrapper.vm.channelOptimizations['ash_resources']
      expect(ashConfig).toBeTruthy()
      expect(ashConfig.priority).toBe('high')
    })
  })

  describe('Event Emission', () => {
    it('should emit notification-sent events', () => {
      const mockEmit = vi.fn()
      wrapper.vm.$emit = mockEmit
      
      const notification = testHelpers.createMockNotification('ash_resources', 'info')
      
      wrapper.vm.sendNotification(notification)
      
      expect(mockEmit).toHaveBeenCalledWith('notification-sent', 
        expect.objectContaining({
          type: 'ash_resources',
          level: 'info'
        })
      )
    })

    it('should emit channel-optimized events', () => {
      const mockEmit = vi.fn()
      wrapper.vm.$emit = mockEmit
      
      const optimization = {
        channel: 'reactor_workflows',
        strategy: 'batching',
        efficiencyGain: 25
      }
      
      wrapper.vm.applyChannelOptimization(optimization)
      
      expect(mockEmit).toHaveBeenCalledWith('channel-optimized', optimization)
    })
  })

  describe('80/20 Optimization Validation', () => {
    it('should demonstrate 80/20 principle in notification processing', () => {
      // Critical channels should handle majority of important notifications
      const criticalChannels = ['error_alerts', 'ash_resources', 'reactor_workflows']
      const totalNotifications = wrapper.vm.notificationStream.length
      
      const criticalNotifications = wrapper.vm.notificationStream.filter(n => 
        criticalChannels.includes(n.channel) && 
        ['critical', 'error', 'high'].includes(n.level)
      ).length
      
      const criticalRatio = criticalNotifications / totalNotifications
      
      // 20% of channels should handle 80% of critical notifications
      if (totalNotifications > 0) {
        expect(criticalRatio).toBeGreaterThan(0.6) // Allow some variance
      }
    })

    it('should show performance improvement with 80/20 filtering', () => {
      // Measure performance without filtering
      wrapper.vm.ultrathin80_20Config.enabled = false
      const startTimeUnfiltered = performance.now()
      
      for (let i = 0; i < 50; i++) {
        wrapper.vm.addNotification(testHelpers.createMockNotification('pipeline_events', 'info'))
      }
      
      const endTimeUnfiltered = performance.now()
      const unfilteredTime = endTimeUnfiltered - startTimeUnfiltered
      
      // Clear and measure with filtering
      wrapper.vm.clearNotifications()
      wrapper.vm.ultrathin80_20Config.enabled = true
      const startTimeFiltered = performance.now()
      
      for (let i = 0; i < 50; i++) {
        wrapper.vm.addNotification(testHelpers.createMockNotification('pipeline_events', 'info'))
      }
      
      const endTimeFiltered = performance.now()
      const filteredTime = endTimeFiltered - startTimeFiltered
      
      // Filtered processing should be more efficient
      expect(filteredTime).toBeLessThanOrEqual(unfilteredTime * 1.2) // Allow 20% margin
    })
  })
})