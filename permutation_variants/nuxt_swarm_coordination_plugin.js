// BitActor Nuxt.js Swarm Coordination Plugin Variant
// This variant explores Nuxt plugin patterns with:
// - Multi-swarm coordination and management
// - TTL-aware swarm lifecycle operations  
// - Plugin-based swarm service injection
// - Hierarchical swarm topology management
// - Real-time swarm health monitoring
// - Cross-swarm communication protocols

// =============================================================================
// Nuxt Plugin Configuration
// =============================================================================

export default defineNuxtPlugin({
  name: 'bitactor-swarm-coordination',
  dependsOn: ['websocket-module'],
  async setup() {
    const swarmCoordinator = new BitActorSwarmCoordinator()
    await swarmCoordinator.initialize()
    
    return {
      provide: {
        swarmCoordinator
      }
    }
  }
})

// =============================================================================
// Core Swarm Coordination Class
// =============================================================================

class BitActorSwarmCoordinator {
  constructor() {
    this.swarms = new Map()
    this.topology = 'hierarchical'
    this.coordinationProtocols = new Map()
    this.healthMonitor = new SwarmHealthMonitor()
    this.communicationBridge = new SwarmCommunicationBridge()
    
    // TTL configuration for swarm operations
    this.ttlConfig = {
      swarmInitTTLMs: 15,
      coordinationTTLMs: 8,
      healthCheckTTLMs: 3,
      messagingTTLMs: 2,
      precisionLevel: 'nanosecond'
    }
    
    // Swarm coordination states
    this.coordinationStates = {
      INITIALIZING: 'initializing',
      ACTIVE: 'active', 
      COORDINATING: 'coordinating',
      SUSPENDED: 'suspended',
      ERROR: 'error',
      SHUTTING_DOWN: 'shutting_down'
    }
  }
  
  async initialize() {
    console.log('🐝 Initializing BitActor Swarm Coordinator Plugin')
    
    // Register coordination protocols
    this.registerCoordinationProtocol('mesh', new MeshCoordinationProtocol())
    this.registerCoordinationProtocol('hierarchical', new HierarchicalCoordinationProtocol())
    this.registerCoordinationProtocol('ring', new RingCoordinationProtocol())
    this.registerCoordinationProtocol('star', new StarCoordinationProtocol())
    
    // Start health monitoring
    await this.healthMonitor.start(this.ttlConfig.healthCheckTTLMs)
    
    // Initialize communication bridge
    await this.communicationBridge.initialize()
    
    console.log('✅ BitActor Swarm Coordinator Plugin initialized')
  }
  
  // =============================================================================
  // Swarm Management Operations
  // =============================================================================
  
  async createSwarm(config) {
    const startTime = performance.now()
    const swarmId = config.id || `swarm_${Date.now()}`
    
    try {
      const swarm = new BitActorSwarm({
        id: swarmId,
        name: config.name || 'Unnamed Swarm',
        topology: config.topology || this.topology,
        maxAgents: config.maxAgents || 8,
        ttlBudgetMs: config.ttlBudgetMs || this.ttlConfig.coordinationTTLMs,
        pipelineStages: config.pipelineStages || [
          'typer', 'turtle', 'ttl2dspy', 'bitactor', 
          'erlang', 'ash', 'reactor', 'k8s'
        ]
      })
      
      // Initialize swarm with TTL enforcement
      await this.enforceOperationTTL(
        () => swarm.initialize(),
        this.ttlConfig.swarmInitTTLMs,
        `Swarm ${swarmId} initialization`
      )
      
      this.swarms.set(swarmId, swarm)
      this.healthMonitor.registerSwarm(swarm)
      
      const executionTime = performance.now() - startTime
      
      console.log(`🐝 Created swarm ${swarmId} in ${executionTime.toFixed(2)}ms`)
      
      return {
        success: true,
        swarmId,
        swarm,
        executionTimeMs: executionTime,
        ttlCompliant: executionTime <= this.ttlConfig.swarmInitTTLMs
      }
      
    } catch (error) {
      const executionTime = performance.now() - startTime
      console.error(`❌ Failed to create swarm ${swarmId}:`, error)
      
      return {
        success: false,
        error: error.message,
        executionTimeMs: executionTime,
        ttlCompliant: executionTime <= this.ttlConfig.swarmInitTTLMs
      }
    }
  }
  
  async coordinateSwarms(swarmIds, coordinationType = 'sequential') {
    const startTime = performance.now()
    
    try {
      const swarms = swarmIds.map(id => this.swarms.get(id)).filter(Boolean)
      
      if (swarms.length !== swarmIds.length) {
        throw new Error('Some swarms not found')
      }
      
      const protocol = this.coordinationProtocols.get(coordinationType)
      if (!protocol) {
        throw new Error(`Unknown coordination type: ${coordinationType}`)
      }
      
      // Execute coordination with TTL enforcement
      const result = await this.enforceOperationTTL(
        () => protocol.coordinate(swarms),
        this.ttlConfig.coordinationTTLMs,
        `Swarm coordination (${coordinationType})`
      )
      
      const executionTime = performance.now() - startTime
      
      return {
        success: true,
        coordinationType,
        result,
        executionTimeMs: executionTime,
        ttlCompliant: executionTime <= this.ttlConfig.coordinationTTLMs,
        swarmsCoordinated: swarms.length
      }
      
    } catch (error) {
      const executionTime = performance.now() - startTime
      console.error(`❌ Swarm coordination failed:`, error)
      
      return {
        success: false,
        error: error.message,
        executionTimeMs: executionTime,
        ttlCompliant: executionTime <= this.ttlConfig.coordinationTTLMs
      }
    }
  }
  
  async executePipelineAcrossSwarms(swarmIds, pipelineData, distributionStrategy = 'round_robin') {
    const startTime = performance.now()
    
    try {
      const activeSwarms = swarmIds
        .map(id => this.swarms.get(id))
        .filter(swarm => swarm && swarm.state === this.coordinationStates.ACTIVE)
      
      if (activeSwarms.length === 0) {
        throw new Error('No active swarms available')
      }
      
      // Distribute pipeline execution across swarms
      const distributedResults = await this.distributePipelineExecution(
        activeSwarms,
        pipelineData,
        distributionStrategy
      )
      
      const executionTime = performance.now() - startTime
      
      return {
        success: true,
        distributionStrategy,
        results: distributedResults,
        executionTimeMs: executionTime,
        ttlCompliant: executionTime <= this.ttlConfig.coordinationTTLMs,
        swarmsUsed: activeSwarms.length
      }
      
    } catch (error) {
      const executionTime = performance.now() - startTime
      console.error(`❌ Distributed pipeline execution failed:`, error)
      
      return {
        success: false,
        error: error.message,
        executionTimeMs: executionTime,
        ttlCompliant: executionTime <= this.ttlConfig.coordinationTTLMs
      }
    }
  }
  
  // =============================================================================
  // Swarm Health and Monitoring
  // =============================================================================
  
  getSwarmHealth(swarmId) {
    const swarm = this.swarms.get(swarmId)
    if (!swarm) {
      return { exists: false }
    }
    
    return {
      exists: true,
      id: swarm.id,
      name: swarm.name,
      state: swarm.state,
      agentCount: swarm.agents.length,
      activeAgents: swarm.agents.filter(a => a.status === 'active').length,
      avgResponseTimeMs: swarm.getAverageResponseTime(),
      ttlViolations: swarm.getTTLViolations(),
      lastActivity: swarm.lastActivity,
      uptime: swarm.getUptime()
    }
  }
  
  getAllSwarmsHealth() {
    const swarmHealths = []
    
    for (const [swarmId, swarm] of this.swarms) {
      swarmHealths.push(this.getSwarmHealth(swarmId))
    }
    
    return {
      totalSwarms: this.swarms.size,
      activeSwarms: swarmHealths.filter(h => h.state === this.coordinationStates.ACTIVE).length,
      totalAgents: swarmHealths.reduce((sum, h) => sum + h.agentCount, 0),
      totalActiveAgents: swarmHealths.reduce((sum, h) => sum + h.activeAgents, 0),
      swarms: swarmHealths
    }
  }
  
  // =============================================================================
  // TTL Enforcement Utilities
  // =============================================================================
  
  async enforceOperationTTL(operation, ttlMs, operationName) {
    return new Promise((resolve, reject) => {
      const timeout = setTimeout(() => {
        reject(new Error(`${operationName} exceeded TTL budget of ${ttlMs}ms`))
      }, ttlMs)
      
      operation()
        .then(result => {
          clearTimeout(timeout)
          resolve(result)
        })
        .catch(error => {
          clearTimeout(timeout)
          reject(error)
        })
    })
  }
  
  // =============================================================================
  // Internal Utilities
  // =============================================================================
  
  registerCoordinationProtocol(name, protocol) {
    this.coordinationProtocols.set(name, protocol)
  }
  
  async distributePipelineExecution(swarms, pipelineData, strategy) {
    switch (strategy) {
      case 'round_robin':
        return await this.roundRobinDistribution(swarms, pipelineData)
      case 'load_balanced':
        return await this.loadBalancedDistribution(swarms, pipelineData)
      case 'parallel':
        return await this.parallelDistribution(swarms, pipelineData)
      default:
        throw new Error(`Unknown distribution strategy: ${strategy}`)
    }
  }
  
  async roundRobinDistribution(swarms, pipelineData) {
    const results = []
    let swarmIndex = 0
    
    for (const data of pipelineData) {
      const swarm = swarms[swarmIndex % swarms.length]
      
      try {
        const result = await swarm.executePipeline(data)
        results.push({
          swarmId: swarm.id,
          success: true,
          result
        })
      } catch (error) {
        results.push({
          swarmId: swarm.id,
          success: false,
          error: error.message
        })
      }
      
      swarmIndex++
    }
    
    return results
  }
  
  async loadBalancedDistribution(swarms, pipelineData) {
    // Sort swarms by current load (ascending)
    const sortedSwarms = [...swarms].sort((a, b) => 
      a.getCurrentLoad() - b.getCurrentLoad()
    )
    
    return await this.roundRobinDistribution(sortedSwarms, pipelineData)
  }
  
  async parallelDistribution(swarms, pipelineData) {
    const chunkSize = Math.ceil(pipelineData.length / swarms.length)
    const chunks = []
    
    for (let i = 0; i < pipelineData.length; i += chunkSize) {
      chunks.push(pipelineData.slice(i, i + chunkSize))
    }
    
    const promises = swarms.map((swarm, index) => {
      const chunk = chunks[index] || []
      return Promise.all(chunk.map(data => swarm.executePipeline(data)))
    })
    
    const results = await Promise.allSettled(promises)
    
    return results.flatMap((result, swarmIndex) => {
      if (result.status === 'fulfilled') {
        return result.value.map(r => ({
          swarmId: swarms[swarmIndex].id,
          success: true,
          result: r
        }))
      } else {
        return [{
          swarmId: swarms[swarmIndex].id,
          success: false,
          error: result.reason.message
        }]
      }
    })
  }
}

// =============================================================================
// BitActor Swarm Class
// =============================================================================

class BitActorSwarm {
  constructor(config) {
    this.id = config.id
    this.name = config.name
    this.topology = config.topology
    this.maxAgents = config.maxAgents
    this.ttlBudgetMs = config.ttlBudgetMs
    this.pipelineStages = config.pipelineStages
    
    this.agents = []
    this.state = 'initializing'
    this.createdAt = Date.now()
    this.lastActivity = Date.now()
    this.metrics = {
      totalExecutions: 0,
      successfulExecutions: 0,
      failedExecutions: 0,
      ttlViolations: 0,
      avgResponseTimeMs: 0
    }
  }
  
  async initialize() {
    // Create agents for each pipeline stage
    for (const stage of this.pipelineStages) {
      const agent = new BitActorSwarmAgent({
        id: `${this.id}_${stage}_agent`,
        stage,
        swarmId: this.id,
        ttlBudgetMs: this.ttlBudgetMs
      })
      
      await agent.initialize()
      this.agents.push(agent)
    }
    
    this.state = 'active'
    console.log(`✅ Swarm ${this.id} initialized with ${this.agents.length} agents`)
  }
  
  async executePipeline(data) {
    const startTime = performance.now()
    this.lastActivity = Date.now()
    this.metrics.totalExecutions++
    
    try {
      let currentData = data
      const results = []
      
      for (const agent of this.agents) {
        const agentResult = await agent.process(currentData)
        results.push(agentResult)
        
        if (!agentResult.success) {
          throw new Error(`Agent ${agent.id} failed: ${agentResult.error}`)
        }
        
        currentData = agentResult.result
      }
      
      const executionTime = performance.now() - startTime
      this.updateMetrics(executionTime, true)
      
      this.metrics.successfulExecutions++
      
      return {
        success: true,
        results,
        finalResult: currentData,
        executionTimeMs: executionTime,
        ttlCompliant: executionTime <= this.ttlBudgetMs
      }
      
    } catch (error) {
      const executionTime = performance.now() - startTime
      this.updateMetrics(executionTime, false)
      
      this.metrics.failedExecutions++
      
      return {
        success: false,
        error: error.message,
        executionTimeMs: executionTime,
        ttlCompliant: executionTime <= this.ttlBudgetMs
      }
    }
  }
  
  getCurrentLoad() {
    const activeAgents = this.agents.filter(a => a.status === 'processing').length
    return activeAgents / this.agents.length
  }
  
  getAverageResponseTime() {
    return this.metrics.avgResponseTimeMs
  }
  
  getTTLViolations() {
    return this.metrics.ttlViolations
  }
  
  getUptime() {
    return Date.now() - this.createdAt
  }
  
  updateMetrics(executionTime, success) {
    // Update average response time
    const totalExecutions = this.metrics.totalExecutions
    this.metrics.avgResponseTimeMs = (
      (this.metrics.avgResponseTimeMs * (totalExecutions - 1)) + executionTime
    ) / totalExecutions
    
    // Track TTL violations
    if (executionTime > this.ttlBudgetMs) {
      this.metrics.ttlViolations++
    }
  }
}

// =============================================================================
// BitActor Swarm Agent Class
// =============================================================================

class BitActorSwarmAgent {
  constructor(config) {
    this.id = config.id
    this.stage = config.stage
    this.swarmId = config.swarmId
    this.ttlBudgetMs = config.ttlBudgetMs
    this.status = 'initializing'
    this.processedCount = 0
    this.errorCount = 0
  }
  
  async initialize() {
    // Simulate agent initialization
    await new Promise(resolve => setTimeout(resolve, 1))
    this.status = 'active'
  }
  
  async process(data) {
    const startTime = performance.now()
    this.status = 'processing'
    
    try {
      // Simulate stage processing based on pipeline stage
      const processingTime = this.getStageProcessingTime(this.stage)
      await new Promise(resolve => setTimeout(resolve, processingTime))
      
      const result = {
        stage: this.stage,
        processedData: `${this.stage}_processed_${data}`,
        agentId: this.id,
        processingTimeMs: processingTime,
        timestamp: Date.now()
      }
      
      this.processedCount++
      this.status = 'active'
      
      const executionTime = performance.now() - startTime
      
      return {
        success: true,
        result,
        executionTimeMs: executionTime,
        ttlCompliant: executionTime <= this.ttlBudgetMs
      }
      
    } catch (error) {
      this.errorCount++
      this.status = 'error'
      
      const executionTime = performance.now() - startTime
      
      return {
        success: false,
        error: error.message,
        executionTimeMs: executionTime,
        ttlCompliant: executionTime <= this.ttlBudgetMs
      }
    }
  }
  
  getStageProcessingTime(stage) {
    const stageTimes = {
      typer: 2,
      turtle: 1,
      ttl2dspy: 3,
      bitactor: 2,
      erlang: 1,
      ash: 2,
      reactor: 3,
      k8s: 1
    }
    
    return stageTimes[stage] || 2
  }
}

// =============================================================================
// Coordination Protocols
// =============================================================================

class MeshCoordinationProtocol {
  async coordinate(swarms) {
    // Mesh coordination: each swarm communicates with every other swarm
    const coordinationResults = []
    
    for (let i = 0; i < swarms.length; i++) {
      for (let j = i + 1; j < swarms.length; j++) {
        const result = await this.coordinatePair(swarms[i], swarms[j])
        coordinationResults.push(result)
      }
    }
    
    return {
      type: 'mesh',
      coordinationCount: coordinationResults.length,
      successful: coordinationResults.filter(r => r.success).length,
      details: coordinationResults
    }
  }
  
  async coordinatePair(swarmA, swarmB) {
    // Simulate mesh coordination between two swarms
    await new Promise(resolve => setTimeout(resolve, 2))
    
    return {
      success: true,
      swarmA: swarmA.id,
      swarmB: swarmB.id,
      coordinationType: 'mesh_peer'
    }
  }
}

class HierarchicalCoordinationProtocol {
  async coordinate(swarms) {
    // Hierarchical coordination: designate leader and coordinate through hierarchy
    const leader = swarms[0]
    const followers = swarms.slice(1)
    
    const coordinationResults = []
    
    for (const follower of followers) {
      const result = await this.coordinateHierarchy(leader, follower)
      coordinationResults.push(result)
    }
    
    return {
      type: 'hierarchical',
      leader: leader.id,
      followers: followers.map(s => s.id),
      coordinationCount: coordinationResults.length,
      successful: coordinationResults.filter(r => r.success).length,
      details: coordinationResults
    }
  }
  
  async coordinateHierarchy(leader, follower) {
    // Simulate hierarchical coordination
    await new Promise(resolve => setTimeout(resolve, 1))
    
    return {
      success: true,
      leader: leader.id,
      follower: follower.id,
      coordinationType: 'hierarchical_command'
    }
  }
}

class RingCoordinationProtocol {
  async coordinate(swarms) {
    // Ring coordination: each swarm coordinates with the next in sequence
    const coordinationResults = []
    
    for (let i = 0; i < swarms.length; i++) {
      const currentSwarm = swarms[i]
      const nextSwarm = swarms[(i + 1) % swarms.length]
      
      const result = await this.coordinateRing(currentSwarm, nextSwarm)
      coordinationResults.push(result)
    }
    
    return {
      type: 'ring',
      coordinationCount: coordinationResults.length,
      successful: coordinationResults.filter(r => r.success).length,
      details: coordinationResults
    }
  }
  
  async coordinateRing(current, next) {
    // Simulate ring coordination
    await new Promise(resolve => setTimeout(resolve, 1))
    
    return {
      success: true,
      current: current.id,
      next: next.id,
      coordinationType: 'ring_sequence'
    }
  }
}

class StarCoordinationProtocol {
  async coordinate(swarms) {
    // Star coordination: central coordinator manages all swarms
    const coordinator = swarms[0]
    const satellites = swarms.slice(1)
    
    const coordinationResults = []
    
    // Parallel coordination from center to all satellites
    const coordinationPromises = satellites.map(satellite => 
      this.coordinateStar(coordinator, satellite)
    )
    
    const results = await Promise.allSettled(coordinationPromises)
    
    results.forEach((result, index) => {
      if (result.status === 'fulfilled') {
        coordinationResults.push(result.value)
      } else {
        coordinationResults.push({
          success: false,
          coordinator: coordinator.id,
          satellite: satellites[index].id,
          error: result.reason.message,
          coordinationType: 'star_hub'
        })
      }
    })
    
    return {
      type: 'star',
      coordinator: coordinator.id,
      satellites: satellites.map(s => s.id),
      coordinationCount: coordinationResults.length,
      successful: coordinationResults.filter(r => r.success).length,
      details: coordinationResults
    }
  }
  
  async coordinateStar(coordinator, satellite) {
    // Simulate star coordination
    await new Promise(resolve => setTimeout(resolve, 1))
    
    return {
      success: true,
      coordinator: coordinator.id,
      satellite: satellite.id,
      coordinationType: 'star_hub'
    }
  }
}

// =============================================================================
// Swarm Health Monitor
// =============================================================================

class SwarmHealthMonitor {
  constructor() {
    this.swarms = new Map()
    this.monitoring = false
    this.healthCheckInterval = null
  }
  
  async start(intervalMs = 3000) {
    this.monitoring = true
    
    this.healthCheckInterval = setInterval(() => {
      this.performHealthChecks()
    }, intervalMs)
    
    console.log(`🏥 Swarm health monitor started (interval: ${intervalMs}ms)`)
  }
  
  async stop() {
    this.monitoring = false
    
    if (this.healthCheckInterval) {
      clearInterval(this.healthCheckInterval)
      this.healthCheckInterval = null
    }
    
    console.log('🏥 Swarm health monitor stopped')
  }
  
  registerSwarm(swarm) {
    this.swarms.set(swarm.id, {
      swarm,
      lastHealthCheck: Date.now(),
      healthHistory: []
    })
  }
  
  unregisterSwarm(swarmId) {
    this.swarms.delete(swarmId)
  }
  
  performHealthChecks() {
    for (const [swarmId, swarmData] of this.swarms) {
      const healthStatus = this.checkSwarmHealth(swarmData.swarm)
      
      swarmData.lastHealthCheck = Date.now()
      swarmData.healthHistory.push({
        timestamp: Date.now(),
        status: healthStatus
      })
      
      // Keep only last 100 health checks
      if (swarmData.healthHistory.length > 100) {
        swarmData.healthHistory = swarmData.healthHistory.slice(-100)
      }
      
      // Log health issues
      if (!healthStatus.healthy) {
        console.warn(`⚠️ Swarm ${swarmId} health issue:`, healthStatus.issues)
      }
    }
  }
  
  checkSwarmHealth(swarm) {
    const issues = []
    
    // Check agent health
    const inactiveAgents = swarm.agents.filter(a => a.status === 'error').length
    if (inactiveAgents > 0) {
      issues.push(`${inactiveAgents} agents in error state`)
    }
    
    // Check TTL violations
    if (swarm.metrics.ttlViolations > 10) {
      issues.push(`High TTL violations: ${swarm.metrics.ttlViolations}`)
    }
    
    // Check response time
    if (swarm.metrics.avgResponseTimeMs > swarm.ttlBudgetMs * 0.8) {
      issues.push(`High response time: ${swarm.metrics.avgResponseTimeMs.toFixed(2)}ms`)
    }
    
    // Check failure rate
    const failureRate = swarm.metrics.failedExecutions / Math.max(swarm.metrics.totalExecutions, 1)
    if (failureRate > 0.1) { // 10% failure rate threshold
      issues.push(`High failure rate: ${(failureRate * 100).toFixed(1)}%`)
    }
    
    return {
      healthy: issues.length === 0,
      issues,
      metrics: {
        activeAgents: swarm.agents.filter(a => a.status === 'active').length,
        totalAgents: swarm.agents.length,
        avgResponseTime: swarm.metrics.avgResponseTimeMs,
        ttlViolations: swarm.metrics.ttlViolations,
        failureRate: failureRate * 100
      }
    }
  }
}

// =============================================================================
// Swarm Communication Bridge
// =============================================================================

class SwarmCommunicationBridge {
  constructor() {
    this.messageQueues = new Map()
    this.subscriptions = new Map()
    this.bridgeActive = false
  }
  
  async initialize() {
    this.bridgeActive = true
    console.log('🌉 Swarm communication bridge initialized')
  }
  
  async shutdown() {
    this.bridgeActive = false
    this.messageQueues.clear()
    this.subscriptions.clear()
    console.log('🌉 Swarm communication bridge shutdown')
  }
  
  async sendMessage(fromSwarmId, toSwarmId, message) {
    if (!this.bridgeActive) {
      throw new Error('Communication bridge not active')
    }
    
    const messageEnvelope = {
      id: `msg_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`,
      from: fromSwarmId,
      to: toSwarmId,
      message,
      timestamp: Date.now(),
      ttl: 5000 // 5 second TTL for messages
    }
    
    // Add to target swarm's message queue
    if (!this.messageQueues.has(toSwarmId)) {
      this.messageQueues.set(toSwarmId, [])
    }
    
    this.messageQueues.get(toSwarmId).push(messageEnvelope)
    
    // Notify subscribers
    this.notifySubscribers(toSwarmId, messageEnvelope)
    
    return messageEnvelope.id
  }
  
  async getMessages(swarmId) {
    if (!this.messageQueues.has(swarmId)) {
      return []
    }
    
    const messages = this.messageQueues.get(swarmId)
    const currentTime = Date.now()
    
    // Filter out expired messages
    const validMessages = messages.filter(msg => 
      (currentTime - msg.timestamp) < msg.ttl
    )
    
    // Update queue with valid messages
    this.messageQueues.set(swarmId, validMessages)
    
    return validMessages
  }
  
  subscribe(swarmId, callback) {
    if (!this.subscriptions.has(swarmId)) {
      this.subscriptions.set(swarmId, [])
    }
    
    this.subscriptions.get(swarmId).push(callback)
    
    return () => {
      // Unsubscribe function
      const callbacks = this.subscriptions.get(swarmId) || []
      const index = callbacks.indexOf(callback)
      if (index > -1) {
        callbacks.splice(index, 1)
      }
    }
  }
  
  notifySubscribers(swarmId, message) {
    const callbacks = this.subscriptions.get(swarmId) || []
    
    callbacks.forEach(callback => {
      try {
        callback(message)
      } catch (error) {
        console.error('Error in message subscription callback:', error)
      }
    })
  }
}

// =============================================================================
// Plugin Composables and Utilities
// =============================================================================

// Composable for using swarm coordination in components
export const useSwarmCoordination = () => {
  const { $swarmCoordinator } = useNuxtApp()
  
  return {
    createSwarm: (config) => $swarmCoordinator.createSwarm(config),
    coordinateSwarms: (swarmIds, type) => $swarmCoordinator.coordinateSwarms(swarmIds, type),
    executePipelineAcrossSwarms: (swarmIds, data, strategy) => 
      $swarmCoordinator.executePipelineAcrossSwarms(swarmIds, data, strategy),
    getSwarmHealth: (swarmId) => $swarmCoordinator.getSwarmHealth(swarmId),
    getAllSwarmsHealth: () => $swarmCoordinator.getAllSwarmsHealth()
  }
}

// Composable for swarm messaging
export const useSwarmMessaging = () => {
  const { $swarmCoordinator } = useNuxtApp()
  const bridge = $swarmCoordinator.communicationBridge
  
  return {
    sendMessage: (from, to, message) => bridge.sendMessage(from, to, message),
    getMessages: (swarmId) => bridge.getMessages(swarmId),
    subscribe: (swarmId, callback) => bridge.subscribe(swarmId, callback)
  }
}

console.log('🔌 BitActor Swarm Coordination Plugin loaded')