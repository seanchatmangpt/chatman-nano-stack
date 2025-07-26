// Ash.Reactor Integration Tests
// Tests Nuxt UI to Ash.Reactor pipeline execution

describe('Ash.Reactor Integration Tests', () => {
  beforeEach(() => {
    cy.task('initializeSwarm')
    cy.visit('/swarm-pipeline')
    cy.connectToSwarmChannel()
    cy.resetSwarmState()
  })

  afterEach(() => {
    cy.disconnectSwarmChannel()
  })

  describe('🔥 Ash Framework Integration', () => {
    it('should connect to Ash domain and resources', () => {
      cy.request({
        method: 'GET',
        url: `${Cypress.env('ASH_ENDPOINT')}/status`,
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(200)
        expect(response.body).to.have.property('domain', 'cns_forge')
        expect(response.body).to.have.property('resources')
        expect(response.body.resources).to.include('telemetry_swarm_reactor')
      })
    })

    it('should validate Ash domain configuration', () => {
      cy.request({
        method: 'GET',
        url: `${Cypress.env('ASH_ENDPOINT')}/domain/config`,
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(200)
        expect(response.body).to.have.property('short_name', 'cns_forge')
        expect(response.body).to.have.property('resources')
        
        const resources = response.body.resources
        expect(resources).to.be.an('array')
        expect(resources.some(r => r.name === 'telemetry_swarm_reactor')).to.be.true
      })
    })

    it('should execute Ash actions through WebSocket', () => {
      cy.executeSwarmStrategy('linear', {
        domain: 'cybersecurity',
        entities: ['threat_data', 'network_logs'],
        complexity: 0.6,
        priority: 'high'
      })

      cy.verifyAshReactorExecution([
        'typer', 'turtle', 'ttl2dspy', 'bitactor', 
        'erlang', 'ash', 'reactor', 'k8s'
      ])

      cy.request({
        method: 'GET',
        url: `${Cypress.env('ASH_ENDPOINT')}/actions/recent`,
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(200)
        expect(response.body).to.be.an('array')
        expect(response.body.length).to.be.greaterThan(0)
        
        const recentAction = response.body[0]
        expect(recentAction).to.have.property('type', 'reactor_execution')
        expect(recentAction).to.have.property('status', 'completed')
      })
    })
  })

  describe('⚡ Reactor Workflow Execution', () => {
    it('should execute linear workflow through reactor', () => {
      cy.executeSwarmStrategy('linear', {
        domain: 'cybersecurity',
        complexity: 0.5
      })

      cy.request({
        method: 'GET',
        url: `${Cypress.env('REACTOR_ENDPOINT')}/status`,
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(200)
        expect(response.body).to.have.property('reactor_running', true)
        expect(response.body).to.have.property('current_workflow')
        expect(response.body.current_workflow).to.have.property('strategy', 'linear')
      })

      cy.getByDataCy('reactor-status')
        .should('contain', 'executing')
        .and('have.class', 'status-active')
    })

    it('should execute 80/20 optimized workflow', () => {
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'healthcare',
        complexity: 0.3,
        priority: 'critical'
      })

      cy.request({
        method: 'GET',
        url: `${Cypress.env('REACTOR_ENDPOINT')}/workflow/current`,
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(200)
        expect(response.body).to.have.property('steps')
        expect(response.body.steps).to.have.length(4) // Only critical stages
        
        const stepNames = response.body.steps.map(s => s.name)
        expect(stepNames).to.include.members(['typer', 'turtle', 'ash', 'k8s'])
        
        // Should not include non-critical stages
        expect(stepNames).to.not.include('ttl2dspy')
        expect(stepNames).to.not.include('bitactor')
        expect(stepNames).to.not.include('erlang')
      })

      cy.verifyOptimizationMetrics({
        stageReduction: '50%',
        efficiency: '90%',
        executionTime: '<200ms'
      })
    })

    it('should execute parallel merge workflow', () => {
      cy.executeSwarmStrategy('parallel_merge', {
        domain: 'finance',
        complexity: 0.7
      })

      cy.request({
        method: 'GET',
        url: `${Cypress.env('REACTOR_ENDPOINT')}/workflow/branches`,
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(200)
        expect(response.body).to.have.property('branches')
        expect(response.body.branches).to.be.an('array')
        expect(response.body.branches.length).to.be.greaterThan(1)
        
        // Verify parallel execution structure
        const branch1 = response.body.branches[0]
        const branch2 = response.body.branches[1]
        
        expect(branch1).to.have.property('steps')
        expect(branch2).to.have.property('steps')
        expect(branch1.steps).to.not.deep.equal(branch2.steps) // Different steps
      })

      cy.getByDataCy('parallel-execution')
        .should('be.visible')
        .and('contain', 'branches active')
    })

    it('should execute emergence guided workflow with AI patterns', () => {
      cy.executeSwarmStrategy('emergence_guided', {
        domain: 'cybersecurity',
        complexity: 0.8,
        priority: 'critical'
      })

      cy.request({
        method: 'GET',
        url: `${Cypress.env('REACTOR_ENDPOINT')}/ai/patterns`,
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(200)
        expect(response.body).to.have.property('emergence_factor')
        expect(response.body.emergence_factor).to.be.greaterThan(0.8)
        
        expect(response.body).to.have.property('patterns')
        expect(response.body.patterns).to.be.an('array')
        expect(response.body.patterns.length).to.be.greaterThan(0)
        
        const patterns = response.body.patterns
        expect(patterns.some(p => p.type === 'convergent')).to.be.true
      })

      cy.getByDataCy('emergence-patterns')
        .should('be.visible')
        .and('contain', 'convergent')
    })
  })

  describe('🔄 Reactor Step Execution', () => {
    it('should execute individual reactor steps correctly', () => {
      cy.executeSwarmStrategy('linear', {
        domain: 'cybersecurity',
        complexity: 0.5
      })

      const expectedSteps = [
        'correlate_event',
        'detect_patterns', 
        'calculate_emergence',
        'optimize_path',
        'execute_stages',
        'collect_results'
      ]

      expectedSteps.forEach((step, index) => {
        cy.request({
          method: 'GET',
          url: `${Cypress.env('REACTOR_ENDPOINT')}/step/${step}/status`,
          failOnStatusCode: false
        }).then((response) => {
          expect(response.status).to.eq(200)
          expect(response.body).to.have.property('step_name', step)
          expect(response.body).to.have.property('status')
          expect(['pending', 'running', 'completed']).to.include(response.body.status)
        })
      })
    })

    it('should handle step failures and retries', () => {
      // Simulate step failure by sending invalid input
      cy.executeSwarmStrategy('linear', {
        domain: 'invalid_domain',
        complexity: 1.5, // Invalid complexity > 1
        priority: 'invalid_priority'
      })

      cy.request({
        method: 'GET',
        url: `${Cypress.env('REACTOR_ENDPOINT')}/errors/recent`,
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(200)
        expect(response.body).to.be.an('array')
        
        if (response.body.length > 0) {
          const error = response.body[0]
          expect(error).to.have.property('step')
          expect(error).to.have.property('error_type')
          expect(error).to.have.property('retry_count')
        }
      })

      cy.getByDataCy('step-error')
        .should('be.visible')
        .and('contain', 'validation failed')

      cy.getByDataCy('retry-step')
        .should('be.visible')
        .click()

      // Should retry with corrected input
      cy.getByDataCy('step-status')
        .should('contain', 'retrying')
    })

    it('should track step execution metrics', () => {
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'healthcare',
        complexity: 0.3
      })

      cy.request({
        method: 'GET',
        url: `${Cypress.env('REACTOR_ENDPOINT')}/metrics/steps`,
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(200)
        expect(response.body).to.have.property('step_metrics')
        
        const metrics = response.body.step_metrics
        expect(metrics).to.be.an('array')
        
        metrics.forEach(metric => {
          expect(metric).to.have.property('step_name')
          expect(metric).to.have.property('duration')
          expect(metric).to.have.property('memory_usage')
          expect(metric).to.have.property('cpu_usage')
          expect(metric.duration).to.be.greaterThan(0)
        })
      })
    })
  })

  describe('🧠 Swarm Intelligence Integration', () => {
    it('should integrate swarm telemetry with reactor execution', () => {
      cy.executeSwarmStrategy('emergence_guided', {
        domain: 'cybersecurity',
        complexity: 0.7
      })

      cy.waitForTelemetryUpdate().then((telemetryData) => {
        expect(telemetryData.data).to.have.property('correlation_id')
        
        // Verify reactor can access telemetry correlation
        cy.request({
          method: 'GET',
          url: `${Cypress.env('REACTOR_ENDPOINT')}/telemetry/${telemetryData.data.correlation_id}`,
          failOnStatusCode: false
        }).then((response) => {
          expect(response.status).to.eq(200)
          expect(response.body).to.have.property('correlation_id', telemetryData.data.correlation_id)
          expect(response.body).to.have.property('reactor_steps')
          expect(response.body.reactor_steps).to.be.an('array')
        })
      })
    })

    it('should adapt reactor execution based on swarm patterns', () => {
      // Execute multiple strategies to build pattern data
      const strategies = ['linear', 'skip_optimization', 'parallel_merge']
      
      for (const strategy of strategies) {
        cy.executeSwarmStrategy(strategy, {
          domain: 'cybersecurity',
          complexity: 0.5
        })
        cy.wait(1000)
      }

      // Next execution should adapt based on patterns
      cy.executeSwarmStrategy('emergence_guided', {
        domain: 'cybersecurity',
        complexity: 0.5
      })

      cy.request({
        method: 'GET',
        url: `${Cypress.env('REACTOR_ENDPOINT')}/adaptation/history`,
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(200)
        expect(response.body).to.have.property('adaptations')
        expect(response.body.adaptations).to.be.an('array')
        expect(response.body.adaptations.length).to.be.greaterThan(0)
        
        const adaptation = response.body.adaptations[0]
        expect(adaptation).to.have.property('pattern_detected')
        expect(adaptation).to.have.property('optimization_applied')
      })
    })

    it('should coordinate between multiple reactor instances', () => {
      // Simulate multiple reactor instances
      cy.executeSwarmStrategy('parallel_merge', {
        domain: 'finance',
        complexity: 0.8
      })

      cy.request({
        method: 'GET',
        url: `${Cypress.env('REACTOR_ENDPOINT')}/coordination/status`,
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(200)
        expect(response.body).to.have.property('coordinator_active', true)
        expect(response.body).to.have.property('reactor_instances')
        expect(response.body.reactor_instances).to.be.greaterThan(0)
        
        expect(response.body).to.have.property('load_balance')
        expect(response.body.load_balance).to.have.property('distribution')
      })
    })
  })

  describe('📊 Performance and Optimization', () => {
    it('should meet 80/20 optimization performance targets', () => {
      const startTime = Date.now()
      
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'healthcare',
        complexity: 0.2,
        priority: 'critical'
      })

      cy.request({
        method: 'GET',
        url: `${Cypress.env('REACTOR_ENDPOINT')}/performance/latest`,
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(200)
        
        const performance = response.body
        expect(performance).to.have.property('execution_time')
        expect(performance.execution_time).to.be.lessThan(200) // <200ms target
        
        expect(performance).to.have.property('stages_executed', 4) // Only critical stages
        expect(performance).to.have.property('optimization_score')
        expect(performance.optimization_score).to.be.greaterThan(0.9) // >90% efficiency
        
        expect(performance).to.have.property('resource_usage')
        expect(performance.resource_usage.memory).to.be.lessThan(50) // <50MB
        expect(performance.resource_usage.cpu).to.be.lessThan(30) // <30% CPU
      })

      const totalTime = Date.now() - startTime
      expect(totalTime).to.be.lessThan(300) // End-to-end under 300ms
    })

    it('should scale reactor execution based on complexity', () => {
      const complexities = [0.2, 0.5, 0.8]
      const results = []

      for (const complexity of complexities) {
        cy.executeSwarmStrategy('adaptive_branch', {
          domain: 'cybersecurity',
          complexity: complexity
        })

        cy.request({
          method: 'GET',
          url: `${Cypress.env('REACTOR_ENDPOINT')}/performance/latest`,
          failOnStatusCode: false
        }).then((response) => {
          results.push({
            complexity,
            execution_time: response.body.execution_time,
            stages_executed: response.body.stages_executed
          })
        })
      }

      // Verify scaling behavior
      cy.then(() => {
        expect(results.length).to.eq(3)
        
        // Higher complexity should use more stages
        expect(results[2].stages_executed).to.be.greaterThan(results[0].stages_executed)
        
        // But execution time should scale reasonably (not exponentially)
        const timeRatio = results[2].execution_time / results[0].execution_time
        expect(timeRatio).to.be.lessThan(3) // No more than 3x slower for 4x complexity
      })
    })

    it('should maintain reactor memory efficiency', () => {
      // Execute multiple strategies sequentially
      const strategies = ['linear', 'skip_optimization', 'parallel_merge', 'emergence_guided']
      
      for (const strategy of strategies) {
        cy.executeSwarmStrategy(strategy, {
          domain: 'cybersecurity',
          complexity: 0.6
        })

        cy.request({
          method: 'GET',
          url: `${Cypress.env('REACTOR_ENDPOINT')}/memory/usage`,
          failOnStatusCode: false
        }).then((response) => {
          expect(response.status).to.eq(200)
          expect(response.body).to.have.property('current_usage')
          expect(response.body.current_usage).to.be.lessThan(100) // <100MB
          
          expect(response.body).to.have.property('peak_usage')
          expect(response.body.peak_usage).to.be.lessThan(150) // <150MB peak
          
          // Memory should not leak between executions
          expect(response.body).to.have.property('baseline_usage')
          const memoryGrowth = response.body.current_usage - response.body.baseline_usage
          expect(memoryGrowth).to.be.lessThan(10) // <10MB growth per execution
        })
      }
    })
  })

  describe('🔧 Error Recovery and Resilience', () => {
    it('should recover from reactor crashes gracefully', () => {
      cy.executeSwarmStrategy('linear', {
        domain: 'cybersecurity',
        complexity: 0.5
      })

      // Simulate reactor crash
      cy.request({
        method: 'POST',
        url: `${Cypress.env('REACTOR_ENDPOINT')}/test/crash`,
        failOnStatusCode: false
      })

      cy.getByDataCy('reactor-error')
        .should('be.visible')
        .and('contain', 'reactor unavailable')

      // Should auto-restart reactor
      cy.wait(5000)

      cy.request({
        method: 'GET',
        url: `${Cypress.env('REACTOR_ENDPOINT')}/status`,
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(200)
        expect(response.body).to.have.property('reactor_running', true)
        expect(response.body).to.have.property('restart_count')
        expect(response.body.restart_count).to.be.greaterThan(0)
      })

      // Should be able to execute strategies again
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'healthcare',
        complexity: 0.3
      })

      cy.getByDataCy('execution-status')
        .should('contain', 'completed')
    })

    it('should handle partial workflow failures', () => {
      // Execute workflow that will fail on specific step
      cy.executeSwarmStrategy('linear', {
        domain: 'cybersecurity',
        complexity: 0.5,
        force_step_failure: 'detect_patterns' // Simulate step failure
      })

      cy.request({
        method: 'GET',
        url: `${Cypress.env('REACTOR_ENDPOINT')}/workflow/failed_steps`,
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(200)
        expect(response.body).to.be.an('array')
        expect(response.body.length).to.be.greaterThan(0)
        
        const failedStep = response.body.find(s => s.name === 'detect_patterns')
        expect(failedStep).to.exist
        expect(failedStep).to.have.property('error')
        expect(failedStep).to.have.property('retry_strategy')
      })

      cy.getByDataCy('partial-failure')
        .should('be.visible')
        .and('contain', 'detect_patterns failed')

      cy.getByDataCy('resume-workflow')
        .should('be.visible')
        .click()

      // Should resume from failed step
      cy.getByDataCy('workflow-resumed')
        .should('be.visible')
    })
  })
})