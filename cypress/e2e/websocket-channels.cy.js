// WebSocket Channels Integration Tests
// Tests Phoenix Channels connection to Nuxt UI components

describe('WebSocket Channels Integration', () => {
  beforeEach(() => {
    cy.task('initializeSwarm')
    cy.visit('/swarm-pipeline')
  })

  afterEach(() => {
    cy.disconnectSwarmChannel()
  })

  describe('🔌 Phoenix Channel Connection', () => {
    it('should establish WebSocket connection to SwarmChannel', () => {
      cy.connectToSwarmChannel()
      
      cy.window().should((win) => {
        expect(win.swarmSocket).to.exist
        expect(win.swarmSocket.connectionState()).to.eq('open')
        expect(win.swarmChannel).to.exist
        expect(win.swarmChannel.state).to.eq('joined')
      })

      cy.getByDataCy('websocket-status')
        .should('contain', 'Connected')
        .and('have.class', 'status-connected')
    })

    it('should receive initial state on channel join', () => {
      cy.connectToSwarmChannel()

      cy.waitForTelemetryUpdate(5000).then((data) => {
        expect(data).to.have.property('strategies')
        expect(data.strategies).to.be.an('array')
        expect(data.strategies.length).to.be.greaterThan(0)
        
        expect(data).to.have.property('stages')
        expect(data.stages).to.include.members(['typer', 'turtle', 'ash', 'k8s'])
        
        expect(data.connected).to.be.true
      })
    })

    it('should handle connection errors gracefully', () => {
      // Simulate connection failure
      cy.window().then((win) => {
        const badSocket = new win.Phoenix.Socket('ws://invalid:9999/socket')
        badSocket.connect()
        
        const channel = badSocket.channel('swarm:pipeline', {})
        channel.join()
          .receive('error', (error) => {
            expect(error).to.exist
          })
      })

      cy.getByDataCy('connection-error')
        .should('be.visible')
        .and('contain', 'connection failed')

      cy.getByDataCy('retry-connection')
        .should('be.visible')
        .click()

      // Should attempt reconnection
      cy.connectToSwarmChannel()
      cy.getByDataCy('websocket-status')
        .should('contain', 'Connected')
    })
  })

  describe('⚡ Pipeline Execution Messages', () => {
    beforeEach(() => {
      cy.connectToSwarmChannel()
      cy.waitForWebSocketConnection()
    })

    it('should send execute_pipeline message and receive response', () => {
      const testInput = {
        strategy: 'skip_optimization',
        domain: 'healthcare',
        entities: ['patient_data', 'medical_records'],
        complexity: 0.3,
        priority: 'high'
      }

      cy.executeSwarmStrategy('skip_optimization', testInput)

      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('execution_result', (result) => {
            expect(result).to.have.property('success', true)
            expect(result).to.have.property('result')
            expect(result).to.have.property('duration')
            expect(result.duration).to.be.lessThan(300) // 80/20 optimization target
            resolve(result)
          })
        })
      })
    })

    it('should handle permutation strategy execution', () => {
      cy.executeSwarmStrategy('permutation', {
        domain: 'cybersecurity',
        complexity: 0.7,
        priority: 'critical'
      })

      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('execution_result', (result) => {
            expect(result.result).to.have.property('permutation_strategy')
            expect(result.result.permutation_strategy).to.eq('adaptive')
            resolve(result)
          })
        })
      })
    })

    it('should process strategy analysis messages', () => {
      cy.connectToSwarmChannel()

      cy.sendWebSocketMessage('select_strategy', { strategy: 'emergence_guided' })

      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('strategy_analysis', (analysis) => {
            expect(analysis).to.have.property('strategy', 'emergence_guided')
            expect(analysis).to.have.property('characteristics')
            expect(analysis.characteristics).to.include('AI-driven')
            expect(analysis).to.have.property('performance_profile')
            resolve(analysis)
          })
        })
      })

      cy.getByDataCy('strategy-analysis')
        .should('be.visible')
        .and('contain', 'AI-driven')
    })
  })

  describe('📡 Real-time Telemetry Streaming', () => {
    beforeEach(() => {
      cy.connectToSwarmChannel()
      cy.sendWebSocketMessage('subscribe_telemetry', {})
    })

    it('should stream telemetry updates during execution', () => {
      cy.executeSwarmStrategy('linear', {
        domain: 'cybersecurity',
        complexity: 0.5
      })

      let telemetryCount = 0
      
      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('telemetry_update', (data) => {
            telemetryCount++
            
            expect(data).to.have.property('type', 'metrics')
            expect(data.data).to.have.property('stage')
            expect(data.data).to.have.property('duration')
            expect(data.data).to.have.property('correlation_id')
            expect(data.data).to.have.property('timestamp')

            if (telemetryCount >= 3) { // Received multiple updates
              resolve(data)
            }
          })
        })
      })

      cy.getByDataCy('telemetry-stream')
        .should('be.visible')
        .children()
        .should('have.length.greaterThan', 2)
    })

    it('should handle telemetry subscription toggle', () => {
      cy.getByDataCy('telemetry-toggle')
        .should('be.checked')

      cy.getByDataCy('telemetry-toggle')
        .uncheck()

      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'healthcare',
        complexity: 0.2
      })

      // Should not receive telemetry updates when disabled
      cy.getByDataCy('telemetry-stream')
        .children()
        .should('have.length', 0)

      cy.getByDataCy('telemetry-toggle')
        .check()

      // Should resume receiving updates
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'healthcare',
        complexity: 0.2
      })

      cy.getByDataCy('telemetry-stream')
        .children()
        .should('have.length.greaterThan', 0)
    })
  })

  describe('🔢 Permutation Matrix Communication', () => {
    beforeEach(() => {
      cy.connectToSwarmChannel()
    })

    it('should request and receive permutation data', () => {
      const params = {
        complexity: 0.6,
        domain: 'finance',
        maxPermutations: 10
      }

      cy.sendWebSocketMessage('get_permutations', { params })

      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('permutation_update', (data) => {
            expect(data).to.have.property('permutations')
            expect(data.permutations).to.be.an('array')
            expect(data.permutations.length).to.be.greaterThan(0)
            
            expect(data).to.have.property('optimal')
            expect(data.optimal).to.be.a('string')

            // Verify permutation structure
            const firstPerm = data.permutations[0]
            expect(firstPerm).to.have.property('id')
            expect(firstPerm).to.have.property('strategy')
            expect(firstPerm).to.have.property('efficiency')
            expect(firstPerm).to.have.property('stage_count')

            resolve(data)
          })
        })
      })
    })

    it('should generate domain-specific permutations', () => {
      const domains = ['cybersecurity', 'healthcare', 'finance']

      domains.forEach(domain => {
        cy.sendWebSocketMessage('get_permutations', {
          params: { domain, complexity: 0.5 }
        })

        cy.window().then((win) => {
          return new Promise((resolve) => {
            win.swarmChannel.on('permutation_update', (data) => {
              const domainPerms = data.permutations.filter(p => p.domain === domain)
              expect(domainPerms.length).to.be.greaterThan(0)
              
              // Verify domain-specific characteristics
              if (domain === 'healthcare') {
                const healthcarePerm = domainPerms.find(p => p.strategy.includes('healthcare'))
                expect(healthcarePerm.efficiency).to.be.greaterThan(0.8) // Healthcare favors efficiency
              }
              
              resolve(data)
            })
          })
        })
      })
    })
  })

  describe('🧠 Swarm Intelligence Messages', () => {
    beforeEach(() => {
      cy.connectToSwarmChannel()
    })

    it('should receive emergence pattern updates', () => {
      cy.executeSwarmStrategy('emergence_guided', {
        domain: 'cybersecurity',
        complexity: 0.8
      })

      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('emergence_pattern', (data) => {
            expect(data).to.have.property('pattern_type')
            expect(data).to.have.property('confidence')
            expect(data).to.have.property('recommendations')
            expect(data.confidence).to.be.greaterThan(0.7)
            resolve(data)
          })
        })
      })
    })

    it('should handle swarm coordination messages', () => {
      cy.executeSwarmStrategy('parallel_merge', {
        domain: 'finance',
        complexity: 0.6
      })

      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('swarm_coordination', (data) => {
            expect(data).to.have.property('coordination_type')
            expect(data).to.have.property('agent_assignments')
            expect(data).to.have.property('load_balance')
            resolve(data)
          })
        })
      })
    })
  })

  describe('🔧 Error Handling and Recovery', () => {
    it('should handle channel disconnection and reconnection', () => {
      cy.connectToSwarmChannel()
      
      cy.getByDataCy('websocket-status')
        .should('contain', 'Connected')

      // Simulate disconnection
      cy.window().then((win) => {
        win.swarmSocket.disconnect()
      })

      cy.getByDataCy('websocket-status')
        .should('contain', 'Disconnected')
        .and('have.class', 'status-disconnected')

      // Should attempt automatic reconnection
      cy.wait(3000)

      cy.connectToSwarmChannel()
      
      cy.getByDataCy('websocket-status')
        .should('contain', 'Connected')
        .and('have.class', 'status-connected')
    })

    it('should handle malformed message errors', () => {
      cy.connectToSwarmChannel()

      cy.window().then((win) => {
        // Send malformed message
        win.swarmChannel.push('execute_pipeline', {
          invalid_data: 'bad_format'
        })
      })

      cy.getByDataCy('message-error')
        .should('be.visible')
        .and('contain', 'invalid message format')

      // Should still be able to send valid messages
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'healthcare',
        complexity: 0.3
      })

      cy.getByDataCy('execution-status')
        .should('contain', 'completed')
    })

    it('should handle server-side execution errors', () => {
      cy.connectToSwarmChannel()

      // Simulate server error response
      cy.window().then((win) => {
        win.swarmChannel.push('execute_pipeline', {
          data: { strategy: 'nonexistent_strategy' }
        })
      })

      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('execution_error', (error) => {
            expect(error).to.have.property('message')
            expect(error).to.have.property('code')
            resolve(error)
          })
        })
      })

      cy.getByDataCy('execution-error')
        .should('be.visible')
        .and('contain', 'strategy not found')
    })
  })

  describe('📈 Performance and Latency', () => {
    beforeEach(() => {
      cy.connectToSwarmChannel()
    })

    it('should maintain low WebSocket latency', () => {
      const startTime = Date.now()

      cy.sendWebSocketMessage('ping', { timestamp: startTime })

      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('pong', (data) => {
            const latency = Date.now() - data.timestamp
            expect(latency).to.be.lessThan(50) // Sub-50ms latency requirement
            resolve(latency)
          })
        })
      })
    })

    it('should handle high-frequency telemetry updates', () => {
      cy.executeSwarmStrategy('linear', {
        domain: 'cybersecurity',
        complexity: 0.8 // High complexity for more telemetry
      })

      let updateCount = 0
      const startTime = Date.now()

      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('telemetry_update', (data) => {
            updateCount++
            
            if (updateCount >= 10) {
              const duration = Date.now() - startTime
              const updatesPerSecond = (updateCount / duration) * 1000
              
              expect(updatesPerSecond).to.be.greaterThan(5) // At least 5 updates/sec
              resolve(updatesPerSecond)
            }
          })
        })
      })
    })

    it('should handle concurrent channel operations', () => {
      const promises = []

      // Execute multiple strategies concurrently
      const strategies = ['skip_optimization', 'parallel_merge', 'emergence_guided']
      
      strategies.forEach(strategy => {
        const promise = cy.executeSwarmStrategy(strategy, {
          domain: 'cybersecurity',
          complexity: 0.5
        })
        promises.push(promise)
      })

      // All should complete successfully
      Promise.all(promises).then(() => {
        cy.getByDataCy('concurrent-executions')
          .should('contain', '3 completed')
      })
    })
  })
})