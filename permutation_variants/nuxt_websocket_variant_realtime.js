// BitActor Nuxt.js WebSocket Variant - Real-Time Pipeline Updates
// This variant explores WebSocket patterns with:
// - Real-time BitActor pipeline status streaming
// - TTL constraint monitoring via WebSocket
// - Live swarm coordination updates
// - Bidirectional TTL-aware communication
// - WebSocket connection TTL enforcement
// - Real-time cybersecurity event visualization

// =============================================================================
// Nuxt Configuration with WebSocket Support
// =============================================================================

export default defineNuxtConfig({
  // Enable WebSocket support
  nitro: {
    experimental: {
      websocket: true
    },
    // WebSocket server configuration
    plugins: [
      '~/server/plugins/websocket-server.js'
    ]
  },
  
  // Runtime configuration for WebSocket
  runtimeConfig: {
    // Server-side WebSocket config
    websocket: {
      port: 3001,
      ttlBudgetMs: 8,
      heartbeatIntervalMs: 1000,
      connectionTimeoutMs: 30000
    },
    
    // Public config for client WebSocket
    public: {
      websocketUrl: process.env.WEBSOCKET_URL || 'ws://localhost:3001',
      enableRealtimeTTL: true,
      ttlStreamingIntervalMs: 100
    }
  },
  
  // Modules for WebSocket integration
  modules: [
    '~/modules/websocket-module.js'
  ],
  
  // Auto-import WebSocket composables
  imports: {
    dirs: [
      'composables/websocket'
    ]
  }
})

// =============================================================================
// WebSocket Server Plugin
// =============================================================================

// ~/server/plugins/websocket-server.js
import { WebSocketServer } from 'ws'
import { createHash } from 'crypto'

export default defineServerPlugin(() => {
  const wss = new WebSocketServer({ port: 3001 })
  
  // Connection registry with TTL tracking
  const connections = new Map()
  
  wss.on('connection', (ws, request) => {
    const connectionId = createHash('md5').update(request.socket.remoteAddress + Date.now()).digest('hex')
    const connectionStart = process.hrtime.bigint()
    
    // Initialize connection with TTL context
    const connectionContext = {
      id: connectionId,
      ws,
      startTime: connectionStart,
      ttlBudgetNs: 8_000_000n, // 8ms default TTL budget
      lastActivity: connectionStart,
      subscriptions: new Set(),
      metrics: {
        messagesSent: 0,
        messagesReceived: 0,
        totalBytesTransferred: 0,
        ttlViolations: 0
      }
    }
    
    connections.set(connectionId, connectionContext)
    
    console.log(`WebSocket connected: ${connectionId}`)
    
    // Send initial connection confirmation with TTL info
    sendMessage(connectionContext, {
      type: 'connection_established',
      connectionId,
      ttlBudgetMs: Number(connectionContext.ttlBudgetNs) / 1_000_000,
      serverTime: Date.now()
    })
    
    // Handle incoming messages with TTL enforcement
    ws.on('message', async (data) => {
      const messageStart = process.hrtime.bigint()
      
      try {
        const message = JSON.parse(data.toString())
        message.receivedAt = messageStart
        
        // Update connection activity
        connectionContext.lastActivity = messageStart
        connectionContext.metrics.messagesReceived++
        connectionContext.metrics.totalBytesTransferred += data.length
        
        // Process message with TTL constraint
        await processWebSocketMessage(connectionContext, message)
        
        // Check processing time against TTL
        const processingTime = process.hrtime.bigint() - messageStart
        if (processingTime > connectionContext.ttlBudgetNs) {
          connectionContext.metrics.ttlViolations++
          
          sendMessage(connectionContext, {
            type: 'ttl_violation',
            processingTimeNs: processingTime.toString(),
            budgetNs: connectionContext.ttlBudgetNs.toString(),
            violationCount: connectionContext.metrics.ttlViolations
          })
        }
        
      } catch (error) {
        console.error(`WebSocket message error for ${connectionId}:`, error)
        sendMessage(connectionContext, {
          type: 'error',
          message: error.message,
          timestamp: Date.now()
        })
      }
    })
    
    // Handle connection close
    ws.on('close', () => {
      console.log(`WebSocket disconnected: ${connectionId}`)
      connections.delete(connectionId)
    })
    
    // Handle connection errors
    ws.on('error', (error) => {
      console.error(`WebSocket error for ${connectionId}:`, error)
      connections.delete(connectionId)
    })
  })
  
  // Start TTL monitoring and heartbeat
  startTTLMonitoring()
  startHeartbeat()
  
  console.log('BitActor WebSocket server started on port 3001')
})

// =============================================================================
// WebSocket Message Processing
// =============================================================================

async function processWebSocketMessage(connectionContext, message) {
  const { type, payload } = message
  
  switch (type) {
    case 'pipeline_execute':
      await handlePipelineExecution(connectionContext, payload)
      break
      
    case 'subscribe_pipeline':
      handlePipelineSubscription(connectionContext, payload)
      break
      
    case 'ttl_update':
      handleTTLUpdate(connectionContext, payload)
      break
      
    case 'swarm_coordinate':
      await handleSwarmCoordination(connectionContext, payload)
      break
      
    case 'heartbeat':
      handleHeartbeat(connectionContext, payload)
      break
      
    default:
      sendMessage(connectionContext, {
        type: 'unknown_message_type',
        originalType: type,
        timestamp: Date.now()
      })
  }
}

async function handlePipelineExecution(connectionContext, payload) {
  const { stages, input, ttlBudgetMs = 8 } = payload
  const executionId = createHash('md5').update(JSON.stringify(payload) + Date.now()).digest('hex')
  
  // Send execution start notification
  sendMessage(connectionContext, {
    type: 'pipeline_execution_started',
    executionId,
    stages,
    ttlBudgetMs,
    startTime: Date.now()
  })
  
  const results = []
  let currentInput = input
  const executionStart = process.hrtime.bigint()
  
  for (const stage of stages) {
    const stageStart = process.hrtime.bigint()
    
    try {
      // Execute pipeline stage (simplified simulation)
      const stageResult = await executePipelineStage(stage, currentInput)
      const stageTime = process.hrtime.bigint() - stageStart
      
      const result = {
        stage,
        success: true,
        result: stageResult,
        executionTimeNs: stageTime.toString(),
        timestamp: Date.now()
      }
      
      results.push(result)
      currentInput = stageResult
      
      // Send real-time stage completion
      sendMessage(connectionContext, {
        type: 'pipeline_stage_completed',
        executionId,
        stageResult: result,
        remainingStages: stages.length - results.length
      })
      
      // Check TTL compliance
      const totalTime = process.hrtime.bigint() - executionStart
      const ttlBudgetNs = BigInt(ttlBudgetMs * 1_000_000)
      
      if (totalTime > ttlBudgetNs) {
        sendMessage(connectionContext, {
          type: 'pipeline_ttl_exceeded',
          executionId,
          totalTimeNs: totalTime.toString(),
          budgetNs: ttlBudgetNs.toString(),
          completedStages: results.length,
          totalStages: stages.length
        })
        break
      }
      
    } catch (error) {
      const stageTime = process.hrtime.bigint() - stageStart
      
      const result = {
        stage,
        success: false,
        error: error.message,
        executionTimeNs: stageTime.toString(),
        timestamp: Date.now()
      }
      
      results.push(result)
      
      sendMessage(connectionContext, {
        type: 'pipeline_stage_failed',
        executionId,
        stageResult: result,
        error: error.message
      })
      break
    }
  }
  
  // Send final execution results
  const totalExecutionTime = process.hrtime.bigint() - executionStart
  
  sendMessage(connectionContext, {
    type: 'pipeline_execution_completed',
    executionId,
    results,
    totalExecutionTimeNs: totalExecutionTime.toString(),
    ttlCompliant: totalExecutionTime <= BigInt(ttlBudgetMs * 1_000_000),
    completedStages: results.filter(r => r.success).length,
    totalStages: stages.length
  })
}

async function executePipelineStage(stage, input) {
  // Simulate pipeline stage execution
  const processingTimeMs = {
    typer: 2,
    turtle: 1,
    ttl2dspy: 3,
    bitactor: 2,
    erlang: 1,
    ash: 2,
    reactor: 3,
    k8s: 1
  }[stage] || 2
  
  await new Promise(resolve => setTimeout(resolve, processingTimeMs))
  
  return {
    stage,
    processedData: `${stage}_processed_data`,
    inputSize: JSON.stringify(input).length,
    outputSize: 150 + Math.floor(Math.random() * 100),
    processingTimeMs,
    timestamp: Date.now()
  }
}

function handlePipelineSubscription(connectionContext, payload) {
  const { pipelineId, events } = payload
  
  // Add subscription to connection context
  events.forEach(event => {
    connectionContext.subscriptions.add(`${pipelineId}:${event}`)
  })
  
  sendMessage(connectionContext, {
    type: 'subscription_confirmed',
    pipelineId,
    events,
    subscriptionCount: connectionContext.subscriptions.size
  })
}

function handleTTLUpdate(connectionContext, payload) {
  const { ttlBudgetMs } = payload
  
  if (ttlBudgetMs && ttlBudgetMs > 0 && ttlBudgetMs <= 100) {
    connectionContext.ttlBudgetNs = BigInt(ttlBudgetMs * 1_000_000)
    
    sendMessage(connectionContext, {
      type: 'ttl_updated',
      newBudgetMs: ttlBudgetMs,
      budgetNs: connectionContext.ttlBudgetNs.toString()
    })
  } else {
    sendMessage(connectionContext, {
      type: 'ttl_update_rejected',
      reason: 'Invalid TTL budget (must be 1-100ms)',
      requestedBudget: ttlBudgetMs
    })
  }
}

async function handleSwarmCoordination(connectionContext, payload) {
  const { swarmId, coordinationType, participants } = payload
  
  // Simulate swarm coordination
  sendMessage(connectionContext, {
    type: 'swarm_coordination_started',
    swarmId,
    coordinationType,
    participantCount: participants.length
  })
  
  // Simulate coordination delay
  await new Promise(resolve => setTimeout(resolve, 10))
  
  sendMessage(connectionContext, {
    type: 'swarm_coordination_completed',
    swarmId,
    coordinationType,
    result: {
      successful: true,
      coordinationTimeMs: 10,
      participantsCoordinated: participants.length
    }
  })
}

function handleHeartbeat(connectionContext, payload) {
  connectionContext.lastActivity = process.hrtime.bigint()
  
  sendMessage(connectionContext, {
    type: 'heartbeat_response',
    serverTime: Date.now(),
    connectionUptime: Number(connectionContext.lastActivity - connectionContext.startTime) / 1_000_000,
    metrics: connectionContext.metrics
  })
}

// =============================================================================
// WebSocket Utilities
// =============================================================================

function sendMessage(connectionContext, message) {
  if (connectionContext.ws.readyState === 1) { // WebSocket.OPEN
    const messageData = JSON.stringify({
      ...message,
      connectionId: connectionContext.id,
      timestamp: message.timestamp || Date.now()
    })
    
    connectionContext.ws.send(messageData)
    connectionContext.metrics.messagesSent++
    connectionContext.metrics.totalBytesTransferred += messageData.length
  }
}

function broadcastToSubscribers(event, message) {
  connections.forEach(connectionContext => {
    if (connectionContext.subscriptions.has(event)) {
      sendMessage(connectionContext, {
        type: 'broadcast',
        event,
        data: message
      })
    }
  })
}

function startTTLMonitoring() {
  setInterval(() => {
    const currentTime = process.hrtime.bigint()
    
    connections.forEach(connectionContext => {
      const inactiveTime = currentTime - connectionContext.lastActivity
      const inactiveMs = Number(inactiveTime) / 1_000_000
      
      // Send TTL metrics update
      sendMessage(connectionContext, {
        type: 'ttl_metrics_update',
        ttlBudgetMs: Number(connectionContext.ttlBudgetNs) / 1_000_000,
        connectionUptimeMs: Number(currentTime - connectionContext.startTime) / 1_000_000,
        inactiveTimeMs: inactiveMs,
        metrics: connectionContext.metrics
      })
    })
  }, 1000) // Every second
}

function startHeartbeat() {
  setInterval(() => {
    broadcastToSubscribers('heartbeat', {
      type: 'server_heartbeat',
      serverTime: Date.now(),
      activeConnections: connections.size,
      totalMetrics: {
        messagesSent: Array.from(connections.values()).reduce((sum, ctx) => sum + ctx.metrics.messagesSent, 0),
        messagesReceived: Array.from(connections.values()).reduce((sum, ctx) => sum + ctx.metrics.messagesReceived, 0),
        totalTTLViolations: Array.from(connections.values()).reduce((sum, ctx) => sum + ctx.metrics.ttlViolations, 0)
      }
    })
  }, 5000) // Every 5 seconds
}

// =============================================================================
// Client-Side WebSocket Composables
// =============================================================================

// ~/composables/websocket/useWebSocketPipeline.js
export const useWebSocketPipeline = () => {
  const runtimeConfig = useRuntimeConfig()
  const ws = ref(null)
  const connectionStatus = ref('disconnected')
  const executionResults = ref([])
  const ttlMetrics = ref({
    budgetMs: 8,
    violations: 0,
    averageExecutionMs: 0
  })
  
  // Connect to WebSocket server
  const connect = () => {
    if (process.client) {
      ws.value = new WebSocket(runtimeConfig.public.websocketUrl)
      
      ws.value.onopen = () => {
        connectionStatus.value = 'connected'
        console.log('WebSocket connected to BitActor pipeline')
      }
      
      ws.value.onmessage = (event) => {
        const message = JSON.parse(event.data)
        handleWebSocketMessage(message)
      }
      
      ws.value.onclose = () => {
        connectionStatus.value = 'disconnected'
        console.log('WebSocket disconnected')
      }
      
      ws.value.onerror = (error) => {
        connectionStatus.value = 'error'
        console.error('WebSocket error:', error)
      }
    }
  }
  
  // Disconnect from WebSocket
  const disconnect = () => {
    if (ws.value) {
      ws.value.close()
      ws.value = null
    }
  }
  
  // Execute pipeline via WebSocket
  const executePipeline = (stages, input, ttlBudgetMs = 8) => {
    if (ws.value && ws.value.readyState === WebSocket.OPEN) {
      const message = {
        type: 'pipeline_execute',
        payload: {
          stages,
          input,
          ttlBudgetMs
        }
      }
      
      ws.value.send(JSON.stringify(message))
      return true
    }
    
    return false
  }
  
  // Subscribe to pipeline events
  const subscribeToPipeline = (pipelineId, events) => {
    if (ws.value && ws.value.readyState === WebSocket.OPEN) {
      const message = {
        type: 'subscribe_pipeline',
        payload: {
          pipelineId,
          events
        }
      }
      
      ws.value.send(JSON.stringify(message))
    }
  }
  
  // Handle incoming WebSocket messages
  const handleWebSocketMessage = (message) => {
    switch (message.type) {
      case 'pipeline_execution_started':
        executionResults.value = []
        break
        
      case 'pipeline_stage_completed':
        executionResults.value.push(message.stageResult)
        break
        
      case 'pipeline_execution_completed':
        console.log('Pipeline execution completed:', message)
        break
        
      case 'ttl_violation':
        ttlMetrics.value.violations++
        console.warn('TTL violation detected:', message)
        break
        
      case 'ttl_metrics_update':
        ttlMetrics.value = {
          ...ttlMetrics.value,
          ...message.metrics
        }
        break
    }
  }
  
  // Send heartbeat
  const sendHeartbeat = () => {
    if (ws.value && ws.value.readyState === WebSocket.OPEN) {
      ws.value.send(JSON.stringify({
        type: 'heartbeat',
        payload: {
          clientTime: Date.now()
        }
      }))
    }
  }
  
  // Auto-connect when composable is used
  onMounted(() => {
    connect()
    
    // Send periodic heartbeats
    const heartbeatInterval = setInterval(sendHeartbeat, 5000)
    
    onUnmounted(() => {
      clearInterval(heartbeatInterval)
      disconnect()
    })
  })
  
  return {
    connectionStatus: readonly(connectionStatus),
    executionResults: readonly(executionResults),
    ttlMetrics: readonly(ttlMetrics),
    executePipeline,
    subscribeToPipeline,
    connect,
    disconnect
  }
}

// ~/composables/websocket/useRealtimeTTL.js
export const useRealtimeTTL = () => {
  const realtimeMetrics = ref({
    currentBudgetMs: 8,
    usedTimeMs: 0,
    remainingTimeMs: 8,
    utilizationPercent: 0,
    violationCount: 0,
    lastViolationTime: null
  })
  
  const updateTTLMetrics = (executionTimeMs) => {
    realtimeMetrics.value.usedTimeMs += executionTimeMs
    realtimeMetrics.value.remainingTimeMs = 
      realtimeMetrics.value.currentBudgetMs - realtimeMetrics.value.usedTimeMs
    realtimeMetrics.value.utilizationPercent = 
      (realtimeMetrics.value.usedTimeMs / realtimeMetrics.value.currentBudgetMs) * 100
    
    if (realtimeMetrics.value.remainingTimeMs < 0) {
      realtimeMetrics.value.violationCount++
      realtimeMetrics.value.lastViolationTime = Date.now()
    }
  }
  
  const resetTTLMetrics = () => {
    realtimeMetrics.value.usedTimeMs = 0
    realtimeMetrics.value.remainingTimeMs = realtimeMetrics.value.currentBudgetMs
    realtimeMetrics.value.utilizationPercent = 0
  }
  
  const isWithinTTLBudget = computed(() => {
    return realtimeMetrics.value.remainingTimeMs >= 0
  })
  
  const ttlHealthStatus = computed(() => {
    const utilization = realtimeMetrics.value.utilizationPercent
    
    if (utilization > 100) return 'exceeded'
    if (utilization > 80) return 'warning'
    if (utilization > 60) return 'caution'
    return 'healthy'
  })
  
  return {
    realtimeMetrics: readonly(realtimeMetrics),
    updateTTLMetrics,
    resetTTLMetrics,
    isWithinTTLBudget,
    ttlHealthStatus
  }
}

// ~/composables/websocket/useSwarmCoordination.js
export const useSwarmCoordination = () => {
  const { executePipeline } = useWebSocketPipeline()
  
  const swarmStatus = ref({
    activeSwarms: [],
    totalParticipants: 0,
    coordinationEvents: []
  })
  
  const coordinateSwarm = (swarmId, coordinationType, participants) => {
    if (ws.value && ws.value.readyState === WebSocket.OPEN) {
      const message = {
        type: 'swarm_coordinate',
        payload: {
          swarmId,
          coordinationType,
          participants
        }
      }
      
      ws.value.send(JSON.stringify(message))
    }
  }
  
  const trackCoordinationEvent = (event) => {
    swarmStatus.value.coordinationEvents.unshift({
      ...event,
      timestamp: Date.now()
    })
    
    // Keep only last 100 events
    if (swarmStatus.value.coordinationEvents.length > 100) {
      swarmStatus.value.coordinationEvents = swarmStatus.value.coordinationEvents.slice(0, 100)
    }
  }
  
  return {
    swarmStatus: readonly(swarmStatus),
    coordinateSwarm,
    trackCoordinationEvent
  }
}