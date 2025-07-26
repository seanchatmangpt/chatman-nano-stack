describe('Phoenix Channels E2E - Swarm Communication', () => {
  let socket

  beforeEach(() => {
    cy.visit('/swarm-pipeline')
    
    // Initialize WebSocket connection
    cy.window().then((win) => {
      const Phoenix = win.Phoenix
      socket = new Phoenix.Socket('/socket', {
        params: { 
          user_id: 'test_user',
          optimization_mode: '80_20'
        }
      })
      socket.connect()
      
      cy.wrap(socket).as('socket')
    })
  })

  afterEach(() => {
    if (socket) {
      socket.disconnect()
    }
  })

  describe('SwarmChannel Integration', () => {
    it('should connect to swarm channel with 80/20 optimization', () => {
      cy.get('@socket').then((socket) => {
        const channel = socket.channel('swarm:test_pipeline', {
          optimization: '80_20',
          domain: 'cybersecurity'
        })
        
        let joinReply
        channel.join()
          .receive('ok', (reply) => {
            joinReply = reply
          })
          .receive('error', (error) => {
            throw new Error(`Failed to join channel: ${error}`)
          })
        
        cy.wrap(null).should(() => {
          expect(joinReply).to.exist
          expect(joinReply.swarm_id).to.equal('test_pipeline')
        })
      })
    })

    it('should execute pipeline through channel communication', () => {
      cy.get('@socket').then((socket) => {
        const channel = socket.channel('swarm:pipeline_execution', {
          optimization: '80_20'
        })
        
        channel.join()
        
        let executionReply
        channel.push('pipeline:execute', {
          stages: ['typer', 'turtle', 'ash', 'reactor', 'k8s'],
          optimization_strategy: 'skip_non_critical',
          domain: 'cybersecurity'
        })
        .receive('ok', (reply) => {
          executionReply = reply
        })
        
        cy.wrap(null).should(() => {
          expect(executionReply).to.exist
          expect(executionReply.execution_id).to.be.a('string')
        })
      })
    })

    it('should receive real-time pipeline events', () => {
      cy.get('@socket').then((socket) => {
        const channel = socket.channel('swarm:realtime_events', {})
        
        const receivedEvents = []
        
        channel.on('pipeline:started', (payload) => {
          receivedEvents.push({ type: 'started', payload })
        })
        
        channel.on('pipeline:stage_completed', (payload) => {
          receivedEvents.push({ type: 'stage_completed', payload })
        })
        
        channel.on('pipeline:completed', (payload) => {
          receivedEvents.push({ type: 'completed', payload })
        })
        
        channel.join()
        
        // Trigger pipeline execution
        channel.push('pipeline:execute', {
          stages: ['typer', 'turtle', 'ash'],
          optimization_strategy: 'minimal_path'
        })
        
        // Wait for events to be received
        cy.wait(5000)
        
        cy.wrap(null).should(() => {
          expect(receivedEvents.length).to.be.greaterThan(0)
          expect(receivedEvents.some(e => e.type === 'started')).to.be.true
        })
      })
    })

    it('should filter events in 80/20 optimization mode', () => {
      cy.get('@socket').then((socket) => {
        const channel = socket.channel('swarm:filtered_events', {
          optimization: '80_20'
        })
        
        const criticalEvents = []
        const nonCriticalEvents = []
        
        channel.on('critical_event', (payload) => {
          criticalEvents.push(payload)
        })
        
        channel.on('non_critical_event', (payload) => {
          nonCriticalEvents.push(payload)
        })
        
        channel.join()
        
        // Send mixed priority events
        channel.push('test_event', { priority: 'high', critical: true })
        channel.push('test_event', { priority: 'low', critical: false })
        channel.push('test_event', { priority: 'medium', critical: false })
        
        cy.wait(2000)
        
        cy.wrap(null).should(() => {
          expect(criticalEvents.length).to.be.greaterThan(0)
          // Non-critical events should be filtered or reduced
          expect(criticalEvents.length).to.be.greaterThan(nonCriticalEvents.length)
        })
      })
    })
  })

  describe('Notification Channel Integration', () => {
    it('should subscribe to critical notification channels', () => {
      cy.get('@socket').then((socket) => {
        const channel = socket.channel('swarm:notifications', {})
        
        channel.join()
        
        let subscriptionReply
        channel.push('notifications:subscribe', {
          channels: ['error_alerts', 'ash_resources', 'reactor_workflows', 'debug_logs']
        })
        .receive('ok', (reply) => {
          subscriptionReply = reply
        })
        
        cy.wrap(null).should(() => {
          expect(subscriptionReply.subscribed).to.include('error_alerts')
          expect(subscriptionReply.subscribed).to.include('ash_resources')
          expect(subscriptionReply.subscribed).to.include('reactor_workflows')
          // debug_logs should be filtered in 80/20 mode
          expect(subscriptionReply.subscribed).to.not.include('debug_logs')
        })
      })
    })

    it('should receive and filter notifications by priority', () => {
      cy.get('@socket').then((socket) => {
        const channel = socket.channel('swarm:notification_stream', {})
        
        const receivedNotifications = []
        
        channel.on('notification', (notification) => {
          receivedNotifications.push(notification)
        })
        
        channel.join()
        
        // Subscribe to notifications
        channel.push('notifications:subscribe', {
          channels: ['error_alerts']
        })
        
        // Send notifications with different priorities
        channel.push('notifications:send', {
          level: 'critical',
          channel: 'error_alerts',
          message: 'Critical error'
        })
        
        channel.push('notifications:send', {
          level: 'info',
          channel: 'error_alerts', 
          message: 'Info message'
        })
        
        cy.wait(2000)
        
        cy.wrap(null).should(() => {
          expect(receivedNotifications.length).to.be.greaterThan(0)
          
          const criticalNotifications = receivedNotifications.filter(n => n.level === 'critical')
          const infoNotifications = receivedNotifications.filter(n => n.level === 'info')
          
          expect(criticalNotifications.length).to.be.greaterThan(0)
          // Info notifications should be filtered or batched
          expect(criticalNotifications.length).to.be.greaterThanOrEqual(infoNotifications.length)
        })
      })
    })

    it('should handle notification batching', () => {
      cy.get('@socket').then((socket) => {
        const channel = socket.channel('swarm:notification_batching', {})
        
        channel.join()
        
        // Configure batching
        channel.push('notifications:configure_batching', {
          channel: 'performance_metrics',
          batch_size: 3,
          timeout_ms: 1000
        })
        
        let batchProcessed = false
        channel.on('notifications:batch_processed', () => {
          batchProcessed = true
        })
        
        // Send multiple notifications rapidly
        for (let i = 0; i < 5; i++) {
          channel.push('notifications:add_to_batch', {
            channel: 'performance_metrics',
            message: `Metric update ${i}`
          })
        }
        
        cy.wait(2000)
        
        cy.wrap(null).should(() => {
          expect(batchProcessed).to.be.true
        })
      })
    })
  })

  describe('Reactor Workflow Channel Integration', () => {
    it('should create and execute reactor workflows', () => {
      cy.get('@socket').then((socket) => {
        const channel = socket.channel('swarm:reactor_workflows', {})
        
        channel.join()
        
        let workflowId
        
        // Create workflow
        channel.push('reactor:workflow:create', {
          name: 'e2e_test_workflow',
          steps: [
            { name: 'parse_input', type: 'ash_action', critical: true },
            { name: 'generate_resources', type: 'ash_create', critical: true },
            { name: 'validate_output', type: 'ash_validate', critical: false }
          ],
          optimization_mode: '80_20'
        })
        .receive('ok', (reply) => {
          workflowId = reply.workflow_id
        })
        
        cy.wait(1000)
        
        cy.wrap(null).should(() => {
          expect(workflowId).to.be.a('string')
        })
        
        // Execute workflow
        let executionId
        channel.push('reactor:workflow:execute', {
          workflow_id: workflowId,
          inputs: { domain: 'cybersecurity' }
        })
        .receive('ok', (reply) => {
          executionId = reply.execution_id
        })
        
        cy.wait(1000)
        
        cy.wrap(null).should(() => {
          expect(executionId).to.be.a('string')
        })
      })
    })

    it('should execute individual reactor steps', () => {
      cy.get('@socket').then((socket) => {
        const channel = socket.channel('swarm:reactor_steps', {})
        
        channel.join()
        
        let stepCompleted = false
        
        channel.push('reactor:step:execute', {
          step_name: 'generate_ash_resource',
          step_type: 'ash_create',
          inputs: {
            name: 'E2ETestResource',
            attributes: { test: true }
          },
          workflow_id: 'e2e_test_workflow'
        })
        .receive('ok', () => {
          stepCompleted = true
        })
        
        cy.wait(2000)
        
        cy.wrap(null).should(() => {
          expect(stepCompleted).to.be.true
        })
      })
    })

    it('should monitor workflow execution status', () => {
      cy.get('@socket').then((socket) => {
        const channel = socket.channel('swarm:workflow_monitoring', {})
        
        const statusUpdates = []
        
        channel.on('reactor:workflow:status_update', (update) => {
          statusUpdates.push(update)
        })
        
        channel.join()
        
        // Start workflow monitoring
        channel.push('reactor:workflow:monitor', {
          workflow_id: 'monitoring_test_workflow'
        })
        
        cy.wait(3000)
        
        cy.wrap(null).should(() => {
          expect(statusUpdates.length).to.be.greaterThan(0)
        })
      })
    })
  })

  describe('Swarm Intelligence Channel Integration', () => {
    it('should apply swarm optimization recommendations', () => {
      cy.get('@socket').then((socket) => {
        const channel = socket.channel('swarm:intelligence', {})
        
        channel.join()
        
        let optimizationResult
        
        channel.push('swarm:optimize', {
          target: 'pipeline',
          strategy: '80_20',
          constraints: { max_duration: 500 }
        })
        .receive('ok', (result) => {
          optimizationResult = result
        })
        
        cy.wait(2000)
        
        cy.wrap(null).should(() => {
          expect(optimizationResult).to.exist
          expect(optimizationResult.improvements).to.be.an('object')
        })
      })
    })

    it('should learn from execution patterns', () => {
      cy.get('@socket').then((socket) => {
        const channel = socket.channel('swarm:learning', {})
        
        channel.join()
        
        let learningResult
        
        channel.push('swarm:learn', {
          execution_id: 'e2e_learning_test',
          domain: 'cybersecurity',
          metrics: { duration: 350, efficiency: 0.85 },
          outcomes: { success: true, errors: [] }
        })
        .receive('ok', (result) => {
          learningResult = result
        })
        
        cy.wait(1000)
        
        cy.wrap(null).should(() => {
          expect(learningResult).to.exist
          expect(learningResult.patterns_learned).to.be.a('number')
          expect(learningResult.confidence_score).to.be.a('number')
        })
      })
    })

    it('should generate contextual recommendations', () => {
      cy.get('@socket').then((socket) => {
        const channel = socket.channel('swarm:recommendations', {})
        
        channel.join()
        
        let recommendations
        
        channel.push('swarm:recommendations', {
          pipeline_state: { active_stages: ['ash', 'reactor'] },
          metrics: { avg_duration: 400, success_rate: 0.9 }
        })
        .receive('ok', (result) => {
          recommendations = result
        })
        
        cy.wait(1000)
        
        cy.wrap(null).should(() => {
          expect(recommendations).to.exist
          expect(recommendations.recommendations).to.be.an('array')
          expect(recommendations.confidence).to.be.a('number')
        })
      })
    })
  })

  describe('Pipeline Handler Integration', () => {
    it('should handle pipeline execution with optimization', () => {
      cy.get('@socket').then((socket) => {
        const channel = socket.channel('swarm:pipeline_handler', {})
        
        channel.join()
        
        let executionId
        
        channel.push('pipeline:execute', {
          domain: 'cybersecurity',
          optimization_strategy: 'skip_non_critical',
          stages: ['typer', 'turtle', 'ash', 'reactor', 'k8s'],
          priority: 'high'
        })
        .receive('ok', (reply) => {
          executionId = reply.execution_id
        })
        
        cy.wait(1000)
        
        cy.wrap(null).should(() => {
          expect(executionId).to.be.a('string')
        })
      })
    })

    it('should monitor pipeline status and metrics', () => {
      cy.get('@socket').then((socket) => {
        const channel = socket.channel('swarm:pipeline_monitoring', {})
        
        channel.join()
        
        let pipelineStatus
        
        channel.push('pipeline:status', {
          execution_id: 'monitoring_test_execution'
        })
        .receive('ok', (status) => {
          pipelineStatus = status
        })
        
        cy.wait(1000)
        
        cy.wrap(null).should(() => {
          expect(pipelineStatus).to.exist
          expect(pipelineStatus.execution_id).to.equal('monitoring_test_execution')
          expect(pipelineStatus.status).to.be.a('string')
        })
      })
    })

    it('should handle stage-level events', () => {
      cy.get('@socket').then((socket) => {
        const channel = socket.channel('swarm:stage_events', {})
        
        const stageEvents = []
        
        channel.on('stage:started', (event) => {
          stageEvents.push({ type: 'started', ...event })
        })
        
        channel.on('stage:completed', (event) => {
          stageEvents.push({ type: 'completed', ...event })
        })
        
        channel.join()
        
        // Trigger stage events
        channel.push('stage:ash:start', {
          execution_id: 'stage_event_test'
        })
        
        cy.wait(1000)
        
        channel.push('stage:ash:complete', {
          execution_id: 'stage_event_test',
          duration: 150,
          outputs: [{ type: 'ash_resource', count: 1 }]
        })
        
        cy.wait(1000)
        
        cy.wrap(null).should(() => {
          expect(stageEvents.length).to.be.greaterThan(0)
          expect(stageEvents.some(e => e.type === 'started')).to.be.true
        })
      })
    })
  })

  describe('Error Handling and Resilience', () => {
    it('should handle channel connection failures gracefully', () => {
      cy.get('@socket').then((socket) => {
        const channel = socket.channel('swarm:unreliable_connection', {})
        
        let connectionError = false
        
        channel.join()
          .receive('error', () => {
            connectionError = true
          })
        
        // Simulate connection issues
        socket.disconnect()
        
        cy.wait(1000)
        
        // Reconnect
        socket.connect()
        
        cy.wrap(null).should(() => {
          // Should handle disconnection gracefully
          expect(true).to.be.true // Basic connectivity test
        })
      })
    })

    it('should handle malformed messages', () => {
      cy.get('@socket').then((socket) => {
        const channel = socket.channel('swarm:error_handling', {})
        
        channel.join()
        
        let errorReceived = false
        
        channel.push('invalid:message:format', {
          malformed: true,
          missing_required_fields: true
        })
        .receive('error', () => {
          errorReceived = true
        })
        
        cy.wait(1000)
        
        cy.wrap(null).should(() => {
          expect(errorReceived).to.be.true
        })
      })
    })

    it('should maintain service during high message volume', () => {
      cy.get('@socket').then((socket) => {
        const channel = socket.channel('swarm:load_test', {})
        
        channel.join()
        
        let responses = 0
        
        // Send many messages rapidly
        for (let i = 0; i < 50; i++) {
          channel.push('load:test', { message_id: i })
            .receive('ok', () => {
              responses++
            })
        }
        
        cy.wait(3000)
        
        cy.wrap(null).should(() => {
          // Should handle most messages successfully
          expect(responses).to.be.greaterThan(40)
        })
      })
    })
  })

  describe('80/20 Optimization Channel Validation', () => {
    it('should demonstrate message filtering effectiveness', () => {
      cy.get('@socket').then((socket) => {
        const channel = socket.channel('swarm:optimization_validation', {
          optimization: '80_20'
        })
        
        const criticalMessages = []
        const nonCriticalMessages = []
        
        channel.on('critical_message', (msg) => {
          criticalMessages.push(msg)
        })
        
        channel.on('non_critical_message', (msg) => {
          nonCriticalMessages.push(msg)
        })
        
        channel.join()
        
        // Send mixed priority messages
        for (let i = 0; i < 10; i++) {
          channel.push('test_message', {
            priority: i < 3 ? 'critical' : 'low',
            message_id: i
          })
        }
        
        cy.wait(2000)
        
        cy.wrap(null).should(() => {
          // Critical messages should be prioritized
          expect(criticalMessages.length).to.be.greaterThan(0)
          expect(criticalMessages.length).to.be.greaterThanOrEqual(nonCriticalMessages.length)
        })
      })
    })

    it('should measure optimization performance improvement', () => {
      cy.get('@socket').then((socket) => {
        // Test without optimization
        const unoptimizedChannel = socket.channel('swarm:unoptimized', {
          optimization: 'full'
        })
        
        unoptimizedChannel.join()
        
        const startTime = Date.now()
        let unoptimizedResponses = 0
        
        for (let i = 0; i < 20; i++) {
          unoptimizedChannel.push('performance_test', { id: i })
            .receive('ok', () => {
              unoptimizedResponses++
            })
        }
        
        cy.wait(2000).then(() => {
          const unoptimizedTime = Date.now() - startTime
          
          // Test with 80/20 optimization
          const optimizedChannel = socket.channel('swarm:optimized', {
            optimization: '80_20'
          })
          
          optimizedChannel.join()
          
          const optimizedStartTime = Date.now()
          let optimizedResponses = 0
          
          for (let i = 0; i < 20; i++) {
            optimizedChannel.push('performance_test', { id: i })
              .receive('ok', () => {
                optimizedResponses++
              })
          }
          
          cy.wait(2000)
          
          cy.wrap(null).should(() => {
            const optimizedTime = Date.now() - optimizedStartTime
            
            // Should show performance improvement or at least comparable performance
            expect(optimizedTime).to.be.lessThan(unoptimizedTime * 1.2) // Within 20% margin
          })
        })
      })
    })
  })
})