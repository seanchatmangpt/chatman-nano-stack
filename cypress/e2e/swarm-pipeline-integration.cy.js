// Comprehensive Swarm Pipeline Integration Tests
// Tests all components connecting to Ash.Reactor and Phoenix Channels

describe('Swarm Pipeline Integration Tests', () => {
  beforeEach(() => {
    cy.visit('/swarm-pipeline')
    cy.connectToSwarmChannel()
    cy.waitForWebSocketConnection()
  })

  afterEach(() => {
    cy.disconnectSwarmChannel()
  })

  describe('🔌 WebSocket Channel Integration', () => {
    it('should establish WebSocket connection to Phoenix Channels', () => {
      cy.window().should((win) => {
        expect(win.swarmSocket).to.exist
        expect(win.swarmChannel).to.exist
      })
      
      cy.getByDataCy('websocket-status')
        .should('contain', 'Connected')
        .and('have.class', 'connected')
    })

    it('should receive initial state from SwarmChannel', () => {
      cy.waitForTelemetryUpdate().then((data) => {
        expect(data).to.have.property('strategies')
        expect(data).to.have.property('stages')
        expect(data.connected).to.be.true
      })
    })

    it('should handle WebSocket disconnection gracefully', () => {
      cy.disconnectSwarmChannel()
      
      cy.getByDataCy('websocket-status')
        .should('contain', 'Disconnected')
        .and('have.class', 'disconnected')
      
      // Should attempt reconnection
      cy.connectToSwarmChannel()
      cy.getByDataCy('websocket-status')
        .should('contain', 'Connected')
    })
  })

  describe('⚡ Ash.Reactor Pipeline Execution', () => {
    it('should execute linear sequential strategy', () => {
      cy.executeSwarmStrategy('linear', {
        domain: 'cybersecurity',
        complexity: 0.5,
        priority: 'medium'
      })

      cy.verifyAshReactorExecution([
        'typer', 'turtle', 'ttl2dspy', 'bitactor', 
        'erlang', 'ash', 'reactor', 'k8s'
      ])

      cy.getByDataCy('execution-status')
        .should('contain', 'completed')
    })

    it('should execute 80/20 skip optimization strategy', () => {
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'healthcare',
        complexity: 0.3,
        priority: 'high'
      })

      cy.verifyAshReactorExecution(['typer', 'turtle', 'ash', 'k8s'])

      cy.verifyOptimizationMetrics({
        stageReduction: '50%',
        efficiency: '90%',
        executionTime: '<250ms'
      })
    })

    it('should execute parallel merge strategy', () => {
      cy.executeSwarmStrategy('parallel_merge', {
        domain: 'finance',
        complexity: 0.7,
        priority: 'high'
      })

      // Verify parallel branches are created
      cy.getByDataCy('parallel-branches')
        .children()
        .should('have.length.greaterThan', 1)

      cy.verifyAshReactorExecution(['typer', 'turtle', 'ash', 'reactor', 'k8s'])
    })

    it('should execute emergence guided strategy with AI optimization', () => {
      cy.executeSwarmStrategy('emergence_guided', {
        domain: 'cybersecurity',
        complexity: 0.8,
        priority: 'critical'
      })

      cy.getByDataCy('emergence-factor')
        .should('contain', '0.8')
        .and('have.class', 'high-emergence')

      cy.getByDataCy('ai-optimizations')
        .should('be.visible')
        .and('contain', 'pattern_recognition')
    })
  })

  describe('🧠 Swarm Intelligence Coordination', () => {
    it('should adapt strategy based on input complexity', () => {
      // Low complexity should trigger skip optimization
      cy.executeSwarmStrategy('adaptive_branch', {
        complexity: 0.2,
        domain: 'healthcare'
      })

      cy.getByDataCy('adaptive-decision')
        .should('contain', 'skip_optimization')

      // High complexity should trigger full pipeline
      cy.executeSwarmStrategy('adaptive_branch', {
        complexity: 0.9,
        domain: 'cybersecurity'
      })

      cy.getByDataCy('adaptive-decision')
        .should('contain', 'linear')
    })

    it('should generate AI recommendations based on execution patterns', () => {
      // Execute multiple strategies to build pattern data
      const strategies = ['linear', 'skip_optimization', 'parallel_merge']
      
      strategies.forEach(strategy => {
        cy.executeSwarmStrategy(strategy, {
          domain: 'cybersecurity',
          complexity: 0.5
        })
        cy.wait(1000)
      })

      cy.getByDataCy('ai-recommendations')
        .should('be.visible')
        .children()
        .should('have.length.greaterThan', 0)

      cy.getByDataCy('recommendation-item')
        .first()
        .should('contain', 'optimization')
    })

    it('should detect emergence patterns in real-time', () => {
      cy.executeSwarmStrategy('emergence_guided', {
        domain: 'cybersecurity',
        complexity: 0.7
      })

      cy.getByDataCy('emergence-patterns')
        .should('be.visible')

      cy.getByDataCy('pattern-detection')
        .should('contain', 'convergent')
        .or('contain', 'divergent')
        .or('contain', 'lateral')
    })
  })

  describe('📊 Real-time Telemetry Streaming', () => {
    it('should stream telemetry data during pipeline execution', () => {
      cy.executeSwarmStrategy('linear', {
        domain: 'cybersecurity',
        complexity: 0.5
      })

      cy.waitForTelemetryUpdate().then((telemetryData) => {
        expect(telemetryData).to.have.property('stage')
        expect(telemetryData).to.have.property('duration')
        expect(telemetryData).to.have.property('correlation_id')
      })

      cy.getByDataCy('telemetry-stream')
        .should('be.visible')
        .children()
        .should('have.length.greaterThan', 0)
    })

    it('should update metrics charts in real-time', () => {
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'healthcare',
        complexity: 0.3
      })

      cy.getByDataCy('metrics-chart')
        .should('be.visible')

      // Verify chart updates during execution
      cy.getByDataCy('chart-data-points')
        .children()
        .should('have.length.greaterThan', 3)
    })

    it('should maintain telemetry history', () => {
      // Execute multiple strategies
      cy.executeSwarmStrategy('linear', { complexity: 0.5 })
      cy.wait(2000)
      cy.executeSwarmStrategy('skip_optimization', { complexity: 0.3 })

      cy.getByDataCy('telemetry-history')
        .should('be.visible')
        .children()
        .should('have.length.greaterThan', 1)

      cy.getByDataCy('history-item')
        .first()
        .should('contain', 'correlation_id')
    })
  })

  describe('🎯 80/20 Optimization Validation', () => {
    it('should achieve 80/20 optimization targets', () => {
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'healthcare',
        complexity: 0.2
      })

      cy.verifyOptimizationMetrics({
        stageReduction: '50%',
        efficiency: '90%',
        executionTime: '<200ms'
      })

      cy.getByDataCy('pareto-score')
        .should('contain', '90')
        .and('have.class', 'excellent')
    })

    it('should identify critical stages correctly', () => {
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'finance',
        complexity: 0.4
      })

      const criticalStages = ['typer', 'turtle', 'ash', 'k8s']
      
      criticalStages.forEach(stage => {
        cy.getByDataCy(`critical-stage-${stage}`)
          .should('be.visible')
          .and('have.class', 'critical')
      })
    })

    it('should provide optimization recommendations', () => {
      cy.executeSwarmStrategy('linear', {
        domain: 'healthcare',
        complexity: 0.8
      })

      cy.getByDataCy('optimization-suggestions')
        .should('be.visible')
        .and('contain', '80/20')

      cy.getByDataCy('apply-optimization')
        .click()

      cy.getByDataCy('optimization-applied')
        .should('contain', 'skip_optimization')
    })
  })

  describe('🔧 Error Handling and Recovery', () => {
    it('should handle reactor execution failures gracefully', () => {
      // Simulate reactor failure
      cy.window().then((win) => {
        win.swarmChannel.push('execute_pipeline', {
          data: { strategy: 'invalid_strategy' }
        })
      })

      cy.getByDataCy('error-message')
        .should('be.visible')
        .and('contain', 'execution failed')

      cy.getByDataCy('retry-execution')
        .should('be.visible')
        .click()

      cy.getByDataCy('execution-status')
        .should('not.contain', 'failed')
    })

    it('should recover from WebSocket disconnections', () => {
      cy.disconnectSwarmChannel()
      
      cy.getByDataCy('connection-error')
        .should('be.visible')

      cy.connectToSwarmChannel()
      
      cy.getByDataCy('connection-restored')
        .should('be.visible')

      // Should be able to execute strategies again
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'healthcare',
        complexity: 0.3
      })

      cy.getByDataCy('execution-status')
        .should('contain', 'completed')
    })
  })

  describe('📈 Performance Validation', () => {
    it('should meet performance benchmarks', () => {
      cy.measureRenderTime('[data-cy="swarm-pipeline-app"]')
      cy.measureMemoryUsage()

      // Test WebSocket latency
      const startTime = Date.now()
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'healthcare',
        complexity: 0.2
      })

      cy.waitForTelemetryUpdate().then(() => {
        const latency = Date.now() - startTime
        expect(latency).to.be.lessThan(100) // Sub-100ms latency
      })
    })

    it('should maintain 60fps during animations', () => {
      cy.executeSwarmStrategy('parallel_merge', {
        domain: 'cybersecurity',
        complexity: 0.6
      })

      cy.window().then((win) => {
        let frameCount = 0
        const startTime = win.performance.now()

        const countFrames = () => {
          frameCount++
          if (win.performance.now() - startTime < 1000) {
            win.requestAnimationFrame(countFrames)
          } else {
            expect(frameCount).to.be.greaterThan(55) // Allow for some variance
          }
        }

        win.requestAnimationFrame(countFrames)
      })
    })
  })
})