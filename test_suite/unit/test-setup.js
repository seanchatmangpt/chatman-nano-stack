import { vi } from 'vitest'
import { config } from '@vue/test-utils'

// Mock WebSocket for testing
global.WebSocket = vi.fn().mockImplementation(() => ({
  send: vi.fn(),
  close: vi.fn(),
  addEventListener: vi.fn(),
  removeEventListener: vi.fn(),
  readyState: 1,
  CONNECTING: 0,
  OPEN: 1,
  CLOSING: 2,
  CLOSED: 3
}))

// Mock performance API
global.performance = {
  now: vi.fn(() => Date.now()),
  mark: vi.fn(),
  measure: vi.fn()
}

// Mock localStorage
const localStorageMock = {
  getItem: vi.fn(),
  setItem: vi.fn(),
  removeItem: vi.fn(),
  clear: vi.fn()
}
global.localStorage = localStorageMock

// Mock crypto for ID generation
global.crypto = {
  getRandomValues: vi.fn((arr) => {
    for (let i = 0; i < arr.length; i++) {
      arr[i] = Math.floor(Math.random() * 256)
    }
    return arr
  }),
  randomUUID: vi.fn(() => 'mock-uuid-1234'),
  subtle: {}
}

// Vue Test Utils global config
config.global.mocks = {
  $emit: vi.fn(),
  $on: vi.fn(),
  $off: vi.fn(),
  $refs: {}
}

// Global test helpers for 80/20 optimization testing
global.testHelpers = {
  // Mock pipeline stage data
  createMockStage: (id, critical = true) => ({
    id,
    name: id.charAt(0).toUpperCase() + id.slice(1),
    icon: '⚡',
    status: 'Ready',
    connected: true,
    active: false,
    critical,
    latency: Math.floor(Math.random() * 100) + 10,
    throughput: Math.floor(Math.random() * 1000) + 100
  }),

  // Mock optimization strategies
  createMockOptimization: (strategy = 'skip_non_critical') => ({
    strategy,
    efficiency: Math.floor(Math.random() * 30) + 70,
    timeSaved: Math.floor(Math.random() * 200) + 50,
    stagesOptimized: Math.floor(Math.random() * 3) + 1
  }),

  // Mock notification data
  createMockNotification: (type = 'pipeline_events', level = 'info') => ({
    id: `notif_${Date.now()}`,
    type,
    level,
    message: `Mock ${type} notification`,
    source: 'test',
    channel: type,
    timestamp: Date.now(),
    priority: level === 'critical' ? 100 : 50
  }),

  // Mock swarm metrics
  createMockSwarmMetrics: () => ({
    avgPipelineTime: Math.floor(Math.random() * 500) + 200,
    successRate: Math.random() * 20 + 80,
    optimizationsApplied: Math.floor(Math.random() * 100),
    activeAgents: Math.floor(Math.random() * 8) + 1
  }),

  // 80/20 optimization test helpers
  is80_20Optimized: (data) => {
    // Check if data shows 80/20 optimization patterns
    const criticalStages = ['typer', 'turtle', 'ash', 'reactor', 'k8s']
    const nonCriticalStages = ['ttl2dspy', 'erlang', 'bitactor']
    
    if (data.stages) {
      const activeCritical = data.stages.filter(s => 
        criticalStages.includes(s.id) && s.active
      ).length
      const activeNonCritical = data.stages.filter(s => 
        nonCriticalStages.includes(s.id) && s.active
      ).length
      
      return activeCritical > activeNonCritical
    }
    
    return false
  },

  // Performance assertion helpers
  assertPerformanceImprovement: (before, after, threshold = 0.1) => {
    const improvement = (before - after) / before
    return improvement >= threshold
  },

  // WebSocket mock helpers
  mockWebSocketConnection: () => {
    const mockWs = {
      send: vi.fn(),
      close: vi.fn(),
      addEventListener: vi.fn(),
      removeEventListener: vi.fn(),
      readyState: 1,
      onopen: null,
      onmessage: null,
      onerror: null,
      onclose: null
    }
    
    // Simulate successful connection
    setTimeout(() => {
      if (mockWs.onopen) mockWs.onopen()
    }, 10)
    
    return mockWs
  }
}

// Console overrides for cleaner test output
const originalConsole = { ...console }
global.console = {
  ...originalConsole,
  log: vi.fn(),
  info: vi.fn(),
  warn: vi.fn(),
  error: vi.fn(),
  debug: vi.fn()
}