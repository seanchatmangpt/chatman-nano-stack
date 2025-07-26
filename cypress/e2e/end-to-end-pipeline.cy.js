// 🎯 ULTRATHINK SWARM 80/20: End-to-End Pipeline Tests
// Complete pipeline validation: Nuxt UI → Channels → Reactor/Ash → K8s

describe('🚀 Complete End-to-End Pipeline Validation', () => {
  beforeEach(() => {
    cy.visit('/')
    cy.resetSwarmState()
    cy.connectToSwarmChannel()
  })
  
  afterEach(() => {
    cy.disconnectSwarmChannel()
  })
  
  describe('🌊 80/20 Critical Path Testing', () => {
    it('should execute the most critical 20% of pipeline flows', () => {
      // Test the critical path: TTL → Ash → Reactor → K8s
      const criticalOntology = `
        @prefix critical: <http://critical.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        
        critical:CriticalAsset a owl:Class .
        critical:SecurityControl a owl:Class .
        
        critical:name a owl:DatatypeProperty ;
          owl:domain critical:CriticalAsset ;
          owl:range xsd:string .
          
        critical:protects a owl:ObjectProperty ;
          owl:domain critical:SecurityControl ;
          owl:range critical:CriticalAsset .
      `
      
      // Phase 1: Transform to Ash
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(criticalOntology)
      cy.get('[data-cy="output-format"]').select('ash')
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Verify Ash resources generated
      cy.get('[data-cy="ash-output"]').should('contain', 'CriticalAsset')
      cy.get('[data-cy="ash-output"]').should('contain', 'SecurityControl')
      
      // Phase 2: Execute in Reactor
      cy.get('[data-cy="output-format"]').select('reactor')
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Verify Reactor workflow generated
      cy.get('[data-cy="reactor-output"]').should('contain', 'use Reactor')
      cy.get('[data-cy="reactor-output"]').should('contain', 'step :')
      
      // Phase 3: Deploy to K8s
      cy.get('[data-cy="output-format"]').select('k8s')
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Verify K8s manifests generated
      cy.get('[data-cy="k8s-output"]').should('contain', 'apiVersion')
      cy.get('[data-cy="k8s-output"]').should('contain', 'kind: Deployment')
      
      // Validate complete pipeline success
      cy.get('[data-cy="pipeline-status"]').should('contain', 'Critical path validated')
    })
    
    it('should handle ultra-fast bypass transformations', () => {
      const bypassOntology = `
        @prefix bypass: <http://bypass.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        
        bypass:SimpleEntity a owl:Class .
      `
      
      // Execute ultra bypass mode
      cy.get('[data-cy="tab-executor"]').click()
      cy.get('[data-cy="execution-mode"]').select('bypass')
      cy.get('[data-cy="speed-control"]').invoke('val', 3).trigger('input') // Max speed
      
      // Load ontology
      cy.get('[data-cy="ontology-input-executor"]').clear().type(bypassOntology)
      
      const startTime = Date.now()
      cy.get('[data-cy="start-pipeline"]').click()
      
      // Should complete very quickly
      cy.get('[data-cy="execution-status"]', { timeout: 10000 })
        .should('contain', 'Completed')
      
      cy.then(() => {
        const endTime = Date.now()
        const duration = endTime - startTime
        expect(duration).to.be.lessThan(5000) // Ultra-fast completion
      })
      
      // Verify bypass stages were used
      cy.get('[data-cy="execution-summary"]').should('contain', 'bypass')
      cy.get('[data-cy="stages-skipped"]').should('exist')
    })
    
    it('should validate all permutation combinations work end-to-end', () => {
      cy.task('generatePermutations', {
        components: ['transformer', 'executor', 'performance'],
        flows: ['ash-integration', 'reactor-integration', 'k8s-deployment'],
        modes: ['sequential', 'parallel', 'bypass']
      }).then((permutations) => {
        // Test top 20% of permutations (80/20 rule)
        const criticalPermutations = permutations.slice(0, Math.ceil(permutations.length * 0.2))
        
        criticalPermutations.forEach((permutation, index) => {
          cy.log(`Testing critical permutation ${index + 1}: ${permutation.id}`)
          
          // Execute permutation end-to-end
          cy.executePermutationE2E(permutation)
          
          // Verify successful completion
          cy.get('[data-cy="permutation-result"]').should('contain', 'success')
          
          // Log results to swarm
          cy.task('logSwarmResults', {
            permutation: permutation.id,
            status: 'success',
            timestamp: new Date().toISOString()
          })
        })
      })
    })
  })
  
  describe('🔄 Complete Pipeline Flow Testing', () => {
    it('should execute typer → turtle → ttl2dspy → ash → reactor → k8s', () => {
      const fullPipelineOntology = `
        @prefix full: <http://fullpipeline.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        
        full:Application a owl:Class .
        full:Database a owl:Class .
        full:LoadBalancer a owl:Class .
        
        full:name a owl:DatatypeProperty ;
          owl:domain full:Application ;
          owl:range xsd:string .
          
        full:connects a owl:ObjectProperty ;
          owl:domain full:Application ;
          owl:range full:Database .
          
        full:balances a owl:ObjectProperty ;
          owl:domain full:LoadBalancer ;
          owl:range full:Application .
      `
      
      // Start complete pipeline
      cy.get('[data-cy="tab-executor"]').click()
      cy.get('[data-cy="execution-mode"]').select('sequential')
      cy.get('[data-cy="ontology-input-executor"]').clear().type(fullPipelineOntology)
      cy.get('[data-cy="start-pipeline"]').click()
      
      // Monitor each stage
      const expectedStages = ['typer', 'turtle', 'ttl2dspy', 'ash', 'reactor', 'k8s']
      
      expectedStages.forEach((stage, index) => {
        cy.get(`[data-cy="stage-${stage}"]`).should('be.visible')
        
        // Wait for stage to start
        cy.get(`[data-cy="stage-${stage}"]`).should('have.class', 'running')
        
        // Monitor stage progress via WebSocket
        cy.window().then((win) => {
          return new Promise((resolve) => {
            win.swarmChannel.on(`stage:${stage}:completed`, (data) => {
              expect(data).to.have.property('stage', stage)
              expect(data).to.have.property('output')
              expect(data).to.have.property('duration')
              resolve(data)
            })
          })
        })
        
        // Wait for stage completion
        cy.get(`[data-cy="stage-${stage}"]`, { timeout: 30000 }).should('have.class', 'completed')
      })
      
      // Verify final output
      cy.get('[data-cy="execution-status"]').should('contain', 'Completed')
      cy.get('[data-cy="final-output"]').should('contain', 'K8s deployment ready')
      
      // Validate outputs from each stage
      cy.get('[data-cy="stage-outputs"]').within(() => {
        cy.get('[data-cy="typer-output"]').should('exist')
        cy.get('[data-cy="turtle-output"]').should('contain', '@prefix')
        cy.get('[data-cy="ttl2dspy-output"]').should('contain', 'dspy')
        cy.get('[data-cy="ash-output"]').should('contain', 'use Ash.Resource')
        cy.get('[data-cy="reactor-output"]').should('contain', 'use Reactor')
        cy.get('[data-cy="k8s-output"]').should('contain', 'apiVersion')
      })
    })
    
    it('should handle parallel pipeline execution', () => {
      const parallelOntology = `
        @prefix parallel: <http://parallel.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        
        parallel:ServiceA a owl:Class .
        parallel:ServiceB a owl:Class .
        parallel:ServiceC a owl:Class .
      `
      
      cy.get('[data-cy="tab-executor"]').click()
      cy.get('[data-cy="execution-mode"]').select('parallel')
      cy.get('[data-cy="ontology-input-executor"]').clear().type(parallelOntology)
      cy.get('[data-cy="start-pipeline"]').click()
      
      // Verify parallel execution indicators
      cy.get('[data-cy="parallel-execution-indicator"]').should('be.visible')
      
      // Monitor concurrent stages
      cy.window().then((win) => {
        return new Promise((resolve) => {
          let runningStages = []
          
          win.swarmChannel.on('parallel:stage_started', (data) => {
            runningStages.push(data.stage)
            
            // Should have multiple stages running simultaneously
            if (runningStages.length >= 2) {
              resolve(runningStages)
            }
          })
        })
      }).then((stages) => {
        expect(stages.length).to.be.greaterThan(1)
      })
      
      // Verify completion
      cy.get('[data-cy="execution-status"]', { timeout: 45000 }).should('contain', 'Completed')
      cy.get('[data-cy="parallel-execution-summary"]').should('contain', 'stages ran in parallel')
    })
    
    it('should validate adaptive routing and optimization', () => {
      const adaptiveOntology = `
        @prefix adaptive: <http://adaptive.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        
        adaptive:ComplexEntity a owl:Class .
        
        adaptive:complexity a owl:DatatypeProperty ;
          owl:domain adaptive:ComplexEntity ;
          owl:range xsd:float .
      `
      
      cy.get('[data-cy="tab-executor"]').click()
      cy.get('[data-cy="execution-mode"]').select('adaptive')
      cy.get('[data-cy="ontology-input-executor"]').clear().type(adaptiveOntology)
      cy.get('[data-cy="start-pipeline"]').click()
      
      // Monitor adaptive routing decisions
      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('adaptive:routing_decision', (data) => {
            expect(data).to.have.property('decision_type')
            expect(data).to.have.property('optimized_path')
            expect(data).to.have.property('estimated_savings')
            resolve(data)
          })
        })
      })
      
      // Verify adaptive optimizations
      cy.get('[data-cy="adaptive-optimizations"]').should('be.visible')
      cy.get('[data-cy="optimization-applied"]').should('contain', 'route optimized')
      
      cy.get('[data-cy="execution-status"]', { timeout: 30000 }).should('contain', 'Completed')
    })
  })
  
  describe('🎨 UI Component Integration Testing', () => {
    it('should validate all UI components work together seamlessly', () => {
      // Test component switching and state preservation
      const testOntology = `
        @prefix ui: <http://uitest.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        
        ui:UIEntity a owl:Class .
      `
      
      // Start in transformer
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(testOntology)
      cy.get('[data-cy="output-format"]').select('ash')
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Switch to executor - should maintain context
      cy.get('[data-cy="tab-executor"]').click()
      cy.get('[data-cy="current-ontology"]').should('contain', 'UIEntity')
      
      // Execute in real-time
      cy.get('[data-cy="start-pipeline"]').click()
      cy.get('[data-cy="execution-status"]', { timeout: 30000 }).should('contain', 'Completed')
      
      // Switch to performance dashboard
      cy.get('[data-cy="tab-performance"]').click()
      cy.get('[data-cy="recent-activity"]').should('contain', 'transformation')
      cy.get('[data-cy="execution-metrics"]').should('be.visible')
      
      // Switch to permutations
      cy.get('[data-cy="tab-permutations"]').click()
      cy.get('[data-cy="permutation-matrix"]').should('be.visible')
      
      // Generate and execute permutation
      cy.get('[data-cy="generate-permutation"]').click()
      cy.get('[data-cy="permutation-list"]').first().click()
      cy.get('[data-cy="execute-permutation"]').click()
      
      cy.get('[data-cy="permutation-result"]', { timeout: 30000 }).should('exist')
      
      // Return to control center overview
      cy.get('[data-cy="tab-overview"]').click()
      cy.get('[data-cy="system-health"]').should('be.visible')
      cy.get('[data-cy="pipeline-summary"]').should('contain', 'completed')
    })
    
    it('should handle real-time updates across all components', () => {
      // Start monitoring all components
      cy.get('[data-cy="tab-performance"]').click()
      cy.get('[data-cy="enable-realtime"]').check()
      
      // Execute transformation while monitoring
      cy.get('[data-cy="tab-transformer"]').click()
      
      const realtimeOntology = `
        @prefix realtime: <http://realtime.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        
        realtime:RealtimeEntity a owl:Class .
      `
      
      cy.get('[data-cy="ontology-input"]').clear().type(realtimeOntology)
      cy.get('[data-cy="output-format"]').select('reactor')
      cy.get('[data-cy="transform-button"]').click()
      
      // Monitor real-time updates in performance tab
      cy.get('[data-cy="tab-performance"]').click()
      
      // Should see metrics updating
      cy.get('[data-cy="cpu-usage"]').should('not.contain', '0%')
      cy.get('[data-cy="memory-usage"]').should('not.contain', '0MB')
      
      // Should see activity updates
      cy.get('[data-cy="recent-activity"]').should('contain', 'transformation started')
      
      // Wait for completion
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Verify final metrics update
      cy.get('[data-cy="tab-performance"]').click()
      cy.get('[data-cy="recent-activity"]').should('contain', 'transformation completed')
    })
    
    it('should validate WebSocket connectivity across all components', () => {
      // Test WebSocket communication for each component
      const components = ['transformer', 'executor', 'permutations', 'performance']
      
      components.forEach(component => {
        cy.get(`[data-cy="tab-${component}"]`).click()
        
        // Verify WebSocket status indicator
        cy.get('[data-cy="websocket-status"]').should('contain', 'Connected')
        
        // Test WebSocket communication
        cy.window().then((win) => {
          return new Promise((resolve) => {
            win.swarmChannel.push('ping', { component })
              .receive('ok', (response) => {
                expect(response).to.have.property('pong', true)
                expect(response).to.have.property('component', component)
                resolve(response)
              })
          })
        })
      })
      
      // Verify all components maintain connection
      cy.get('[data-cy="connection-health"]').should('contain', 'All components connected')
    })
  })
  
  describe('🚨 Error Handling and Recovery', () => {
    it('should handle pipeline failures gracefully', () => {
      const errorOntology = `
        @prefix error: <http://error.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        
        error:InvalidEntity a owl:Class .
        // Intentional syntax error to trigger failure
        invalid syntax here
      `
      
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(errorOntology)
      cy.get('[data-cy="transform-button"]').click()
      
      // Should handle error gracefully
      cy.get('[data-cy="transformation-error"]', { timeout: 10000 }).should('be.visible')
      cy.get('[data-cy="error-details"]').should('contain', 'syntax error')
      
      // Should offer recovery options
      cy.get('[data-cy="error-recovery"]').should('be.visible')
      cy.get('[data-cy="retry-transformation"]').should('be.visible')
      cy.get('[data-cy="fix-syntax"]').should('be.visible')
      
      // Test recovery
      cy.get('[data-cy="fix-syntax"]').click()
      cy.get('[data-cy="ontology-input"]').should('not.contain', 'invalid syntax')
      
      cy.get('[data-cy="retry-transformation"]').click()
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
    })
    
    it('should handle WebSocket disconnections', () => {
      // Start with connected state
      cy.get('[data-cy="websocket-status"]').should('contain', 'Connected')
      
      // Simulate disconnection
      cy.window().then((win) => {
        win.swarmSocket.disconnect()
      })
      
      // Should detect disconnection
      cy.get('[data-cy="websocket-status"]').should('contain', 'Disconnected')
      cy.get('[data-cy="connection-warning"]').should('be.visible')
      
      // Should attempt reconnection
      cy.get('[data-cy="reconnect-button"]').click()
      
      // Should reconnect successfully
      cy.connectToSwarmChannel()
      cy.get('[data-cy="websocket-status"]').should('contain', 'Connected')
      cy.get('[data-cy="connection-restored"]').should('be.visible')
    })
    
    it('should validate system recovery after errors', () => {
      // Create error condition
      const invalidOntology = 'invalid ttl content'
      
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(invalidOntology)
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-error"]', { timeout: 10000 }).should('be.visible')
      
      // Reset system
      cy.get('[data-cy="system-reset"]').click()
      cy.get('[data-cy="confirm-reset"]').click()
      
      // Verify system is reset and functional
      cy.get('[data-cy="ontology-input"]').should('have.value', '')
      cy.get('[data-cy="system-status"]').should('contain', 'Ready')
      
      // Test that system works after reset
      const recoveryOntology = `
        @prefix recovery: <http://recovery.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        
        recovery:RecoveryEntity a owl:Class .
      `
      
      cy.get('[data-cy="ontology-input"]').type(recoveryOntology)
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      cy.get('[data-cy="system-recovery-success"]').should('be.visible')
    })
  })
  
  describe('📊 Performance and Scalability Validation', () => {
    it('should handle large ontologies efficiently', () => {
      // Generate large ontology for stress testing
      const largeOntology = Array.from({ length: 200 }, (_, i) => 
        `large:Entity${i} a owl:Class .`
      ).join('\n')
      
      const fullLargeOntology = `
        @prefix large: <http://large.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        ${largeOntology}
      `
      
      const startTime = Date.now()
      
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(fullLargeOntology)
      cy.get('[data-cy="output-format"]').select('ash')
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 120000 })
        .should('contain', '100%')
      
      cy.then(() => {
        const endTime = Date.now()
        const duration = endTime - startTime
        expect(duration).to.be.lessThan(60000) // Should complete within 1 minute
      })
      
      // Verify all entities were processed
      cy.get('[data-cy="ash-output"]').should('contain', 'Entity0')
      cy.get('[data-cy="ash-output"]').should('contain', 'Entity199')
      
      // Check memory usage didn't spike excessively
      cy.get('[data-cy="tab-performance"]').click()
      cy.get('[data-cy="memory-usage"]').should('not.contain', '100%')
    })
    
    it('should validate concurrent user simulation', () => {
      // Simulate multiple concurrent operations
      const concurrentOntologies = Array.from({ length: 5 }, (_, i) => 
        `@prefix concurrent${i}: <http://concurrent${i}.org/> . concurrent${i}:Entity a owl:Class .`
      )
      
      // Execute multiple transformations concurrently
      concurrentOntologies.forEach((ontology, index) => {
        cy.window().then((win) => {
          win.swarmChannel.push('transform:request', {
            ontology,
            format: 'ash',
            requestId: `concurrent_${index}`
          })
        })
      })
      
      // Wait for all to complete
      cy.window().then((win) => {
        return new Promise((resolve) => {
          let completedCount = 0
          
          win.swarmChannel.on('transform:completed', (data) => {
            if (data.requestId && data.requestId.startsWith('concurrent_')) {
              completedCount++
              
              if (completedCount >= 5) {
                resolve(completedCount)
              }
            }
          })
        })
      }).then((count) => {
        expect(count).to.equal(5)
      })
      
      // Verify system remained stable
      cy.get('[data-cy="system-health"]').should('contain', 'Stable')
      cy.get('[data-cy="concurrent-operations"]').should('contain', '5 completed')
    })
  })
})

// Custom commands for E2E testing
Cypress.Commands.add('executePermutationE2E', (permutation) => {
  // Navigate to appropriate component
  cy.get(`[data-cy="tab-${permutation.component}"]`).click()
  
  // Set up permutation parameters
  switch (permutation.flow) {
    case 'ash-integration':
      cy.get('[data-cy="output-format"]').select('ash')
      break
    case 'reactor-integration':
      cy.get('[data-cy="output-format"]').select('reactor')
      break
    case 'k8s-deployment':
      cy.get('[data-cy="output-format"]').select('k8s')
      break
  }
  
  // Set execution mode
  if (permutation.component === 'executor') {
    cy.get('[data-cy="execution-mode"]').select(permutation.mode)
  }
  
  // Execute the permutation
  const testOntology = `
    @prefix perm: <http://permutation.org/> .
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    
    perm:PermutationEntity a owl:Class .
  `
  
  if (permutation.component === 'transformer') {
    cy.get('[data-cy="ontology-input"]').clear().type(testOntology)
    cy.get('[data-cy="transform-button"]').click()
    
    cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
      .should('contain', '100%')
  } else if (permutation.component === 'executor') {
    cy.get('[data-cy="ontology-input-executor"]').clear().type(testOntology)
    cy.get('[data-cy="start-pipeline"]').click()
    
    cy.get('[data-cy="execution-status"]', { timeout: 30000 })
      .should('contain', 'Completed')
  }
})