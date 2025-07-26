// ⚛️ ULTRATHINK SWARM 80/20: Reactor Integration Tests
// Testing Nuxt UI → WebSocket → Elixir Reactor pipeline flow

describe('⚛️ Nuxt to Reactor Pipeline Integration', () => {
  beforeEach(() => {
    cy.visit('/')
    cy.resetSwarmState()
    cy.connectToSwarmChannel()
  })
  
  afterEach(() => {
    cy.disconnectSwarmChannel()
  })
  
  describe('🔄 Reactor Workflow Generation', () => {
    it('should generate valid Reactor workflow from TTL ontology', () => {
      const testOntology = `
        @prefix cyber: <http://cybersecurity.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        
        cyber:WebAsset a owl:Class .
        cyber:SecurityThreat a owl:Class .
        cyber:protects a owl:ObjectProperty ;
          owl:domain cyber:WebAsset ;
          owl:range cyber:SecurityThreat .
      `
      
      // Navigate to transformer
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(testOntology)
      cy.get('[data-cy="output-format"]').select('reactor')
      cy.get('[data-cy="transform-button"]').click()
      
      // Wait for transformation to complete
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Verify Reactor workflow structure
      cy.get('[data-cy="reactor-output"]').should('be.visible')
      cy.get('[data-cy="reactor-output"]').should('contain', 'defmodule')
      cy.get('[data-cy="reactor-output"]').should('contain', 'use Reactor')
      cy.get('[data-cy="reactor-output"]').should('contain', 'input')
      cy.get('[data-cy="reactor-output"]').should('contain', 'step')
    })
    
    it('should validate Reactor workflow syntax', () => {
      const complexOntology = `
        @prefix cyber: <http://cybersecurity.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        
        cyber:Asset a owl:Class .
        cyber:Threat a owl:Class .
        cyber:Control a owl:Class .
        
        cyber:threatens a owl:ObjectProperty ;
          owl:domain cyber:Threat ;
          owl:range cyber:Asset .
          
        cyber:mitigates a owl:ObjectProperty ;
          owl:domain cyber:Control ;
          owl:range cyber:Threat .
      `
      
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(complexOntology)
      cy.get('[data-cy="output-format"]').select('reactor')
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Validate Elixir syntax
      cy.get('[data-cy="validate-syntax"]').click()
      cy.get('[data-cy="syntax-validation"]').should('contain', 'Valid Elixir syntax')
      
      // Check for required Reactor components
      cy.get('[data-cy="reactor-output"]').then(($output) => {
        const content = $output.text()
        expect(content).to.include('defmodule')
        expect(content).to.include('use Reactor')
        expect(content).to.include('input :ontology')
        expect(content).to.include('step :')
        expect(content).to.include('run')
      })
    })
    
    it('should handle large ontologies efficiently', () => {
      // Generate large ontology for performance testing
      const largeOntology = Array.from({ length: 50 }, (_, i) => 
        `cyber:Entity${i} a owl:Class .`
      ).join('\n')
      
      const fullOntology = `
        @prefix cyber: <http://cybersecurity.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        ${largeOntology}
      `
      
      const startTime = Date.now()
      
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(fullOntology)
      cy.get('[data-cy="output-format"]').select('reactor')
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 60000 })
        .should('contain', '100%')
      
      cy.then(() => {
        const endTime = Date.now()
        const duration = endTime - startTime
        expect(duration).to.be.lessThan(30000) // Should complete in under 30 seconds
      })
      
      // Verify all entities are processed
      cy.get('[data-cy="reactor-output"]').should('contain', 'Entity0')
      cy.get('[data-cy="reactor-output"]').should('contain', 'Entity49')
    })
  })
  
  describe('🔗 Reactor Execution Testing', () => {
    beforeEach(() => {
      // Set up test ontology
      const testOntology = `
        @prefix test: <http://test.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        
        test:Asset a owl:Class .
        test:owns a owl:ObjectProperty ;
          owl:domain test:Asset ;
          owl:range test:Asset .
      `
      
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(testOntology)
      cy.get('[data-cy="output-format"]').select('reactor')
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
    })
    
    it('should execute Reactor workflow via WebSocket', () => {
      cy.get('[data-cy="execute-reactor"]').click()
      
      // Monitor execution via WebSocket
      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('reactor:execution_started', (data) => {
            expect(data).to.have.property('workflow_id')
            expect(data).to.have.property('status', 'running')
            resolve(data)
          })
        })
      })
      
      // Wait for completion
      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('reactor:execution_completed', (data) => {
            expect(data).to.have.property('workflow_id')
            expect(data).to.have.property('status', 'completed')
            expect(data).to.have.property('result')
            resolve(data)
          })
        })
      })
      
      cy.get('[data-cy="reactor-execution-status"]').should('contain', 'Completed')
    })
    
    it('should handle Reactor execution errors gracefully', () => {
      // Introduce syntax error in generated workflow
      cy.get('[data-cy="reactor-output-editor"]').clear().type('invalid elixir syntax')
      cy.get('[data-cy="execute-reactor"]').click()
      
      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('reactor:execution_error', (error) => {
            expect(error).to.have.property('type', 'syntax_error')
            expect(error).to.have.property('message')
            expect(error).to.have.property('line')
            resolve(error)
          })
        })
      })
      
      cy.get('[data-cy="reactor-error"]').should('be.visible')
      cy.get('[data-cy="reactor-error"]').should('contain', 'syntax error')
    })
    
    it('should provide real-time execution feedback', () => {
      cy.get('[data-cy="execute-reactor"]').click()
      
      let stepCount = 0
      
      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('reactor:step_completed', (data) => {
            stepCount++
            expect(data).to.have.property('step_name')
            expect(data).to.have.property('duration')
            expect(data).to.have.property('output')
            
            if (stepCount >= 2) {
              resolve(stepCount)
            }
          })
        })
      })
      
      // Verify steps are displayed in UI
      cy.get('[data-cy="reactor-steps"]').should('be.visible')
      cy.get('[data-cy="step-status"]').should('have.length.greaterThan', 1)
    })
  })
  
  describe('🎯 Reactor Performance Optimization', () => {
    it('should optimize Reactor workflow for parallel execution', () => {
      const parallelOntology = `
        @prefix parallel: <http://parallel.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        
        parallel:TaskA a owl:Class .
        parallel:TaskB a owl:Class .
        parallel:TaskC a owl:Class .
        
        parallel:dependsOn a owl:ObjectProperty ;
          owl:domain parallel:TaskC ;
          owl:range parallel:TaskA .
      `
      
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(parallelOntology)
      cy.get('[data-cy="output-format"]').select('reactor')
      cy.get('[data-cy="optimization-mode"]').select('parallel')
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Verify parallel execution hints in generated workflow
      cy.get('[data-cy="reactor-output"]').should('contain', 'async: true')
      cy.get('[data-cy="reactor-output"]').should('contain', 'max_concurrency')
      
      // Check optimization metrics
      cy.get('[data-cy="optimization-metrics"]').should('be.visible')
      cy.get('[data-cy="parallel-stages"]').should('contain.text', '2') // TaskA and TaskB can run in parallel
    })
    
    it('should measure and compare execution performance', () => {
      const testOntology = `
        @prefix perf: <http://performance.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        
        perf:Entity a owl:Class .
        perf:relates a owl:ObjectProperty .
      `
      
      // Execute sequential version
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(testOntology)
      cy.get('[data-cy="output-format"]').select('reactor')
      cy.get('[data-cy="optimization-mode"]').select('sequential')
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      const sequentialStartTime = Date.now()
      cy.get('[data-cy="execute-reactor"]').click()
      
      cy.get('[data-cy="reactor-execution-status"]', { timeout: 30000 })
        .should('contain', 'Completed')
      
      cy.then(() => {
        const sequentialDuration = Date.now() - sequentialStartTime
        cy.wrap(sequentialDuration).as('sequentialTime')
      })
      
      // Execute parallel version
      cy.get('[data-cy="optimization-mode"]').select('parallel')
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      const parallelStartTime = Date.now()
      cy.get('[data-cy="execute-reactor"]').click()
      
      cy.get('[data-cy="reactor-execution-status"]', { timeout: 30000 })
        .should('contain', 'Completed')
      
      cy.then(() => {
        const parallelDuration = Date.now() - parallelStartTime
        
        cy.get('@sequentialTime').then((sequential) => {
          // Parallel should be faster or at least comparable
          expect(parallelDuration).to.be.lessThan(sequential * 1.2)
        })
      })
    })
    
    it('should provide performance recommendations', () => {
      const complexOntology = `
        @prefix complex: <http://complex.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        
        ${Array.from({ length: 20 }, (_, i) => 
          `complex:Entity${i} a owl:Class .`
        ).join('\n')}
        
        ${Array.from({ length: 10 }, (_, i) => 
          `complex:relation${i} a owl:ObjectProperty .`
        ).join('\n')}
      `
      
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(complexOntology)
      cy.get('[data-cy="output-format"]').select('reactor')
      cy.get('[data-cy="analyze-performance"]').check()
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Check performance recommendations
      cy.get('[data-cy="performance-recommendations"]').should('be.visible')
      cy.get('[data-cy="recommendation-list"]').should('have.length.greaterThan', 0)
      
      // Verify specific recommendations
      cy.get('[data-cy="recommendation-list"]').should('contain', 'parallel')
      cy.get('[data-cy="performance-score"]').should('exist')
      cy.get('[data-cy="bottleneck-analysis"]').should('exist')
    })
  })
  
  describe('🔍 Reactor Debugging and Monitoring', () => {
    it('should provide step-by-step execution debugging', () => {
      const debugOntology = `
        @prefix debug: <http://debug.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        
        debug:DebugEntity a owl:Class .
        debug:hasProperty a owl:DatatypeProperty ;
          owl:domain debug:DebugEntity ;
          owl:range xsd:string .
      `
      
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(debugOntology)
      cy.get('[data-cy="output-format"]').select('reactor')
      cy.get('[data-cy="debug-mode"]').check()
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      cy.get('[data-cy="execute-reactor"]').click()
      
      // Monitor debug information
      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('reactor:debug_info', (data) => {
            expect(data).to.have.property('step_name')
            expect(data).to.have.property('input_data')
            expect(data).to.have.property('output_data')
            expect(data).to.have.property('execution_time')
            resolve(data)
          })
        })
      })
      
      // Verify debug panel
      cy.get('[data-cy="debug-panel"]').should('be.visible')
      cy.get('[data-cy="step-inputs"]').should('be.visible')
      cy.get('[data-cy="step-outputs"]').should('be.visible')
      cy.get('[data-cy="execution-timeline"]').should('be.visible')
    })
    
    it('should track memory and resource usage', () => {
      cy.get('[data-cy="tab-transformer"]').click()
      
      const resourceOntology = `
        @prefix resource: <http://resource.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        
        ${Array.from({ length: 100 }, (_, i) => 
          `resource:Entity${i} a owl:Class .`
        ).join('\n')}
      `
      
      cy.get('[data-cy="ontology-input"]').clear().type(resourceOntology)
      cy.get('[data-cy="output-format"]').select('reactor')
      cy.get('[data-cy="monitor-resources"]').check()
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 60000 })
        .should('contain', '100%')
      
      cy.get('[data-cy="execute-reactor"]').click()
      
      // Monitor resource usage
      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('reactor:resource_update', (data) => {
            expect(data).to.have.property('memory_usage')
            expect(data).to.have.property('cpu_usage')
            expect(data).to.have.property('step_count')
            expect(data.memory_usage).to.be.a('number')
            resolve(data)
          })
        })
      })
      
      // Verify resource monitoring UI
      cy.get('[data-cy="resource-monitor"]').should('be.visible')
      cy.get('[data-cy="memory-usage"]').should('not.contain', '0')
      cy.get('[data-cy="cpu-usage"]').should('not.contain', '0')
    })
    
    it('should generate execution reports', () => {
      const reportOntology = `
        @prefix report: <http://report.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        
        report:ReportEntity a owl:Class .
        report:hasMetric a owl:DatatypeProperty .
      `
      
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(reportOntology)
      cy.get('[data-cy="output-format"]').select('reactor')
      cy.get('[data-cy="generate-reports"]').check()
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      cy.get('[data-cy="execute-reactor"]').click()
      
      cy.get('[data-cy="reactor-execution-status"]', { timeout: 30000 })
        .should('contain', 'Completed')
      
      // Verify report generation
      cy.get('[data-cy="execution-report"]').should('be.visible')
      cy.get('[data-cy="export-report"]').click()
      
      // Verify report content
      cy.get('[data-cy="report-content"]').should('contain', 'Execution Summary')
      cy.get('[data-cy="report-content"]').should('contain', 'Step Details')
      cy.get('[data-cy="report-content"]').should('contain', 'Performance Metrics')
      cy.get('[data-cy="report-content"]').should('contain', 'Resource Usage')
    })
  })
  
  describe('🔄 Reactor Integration with Other Components', () => {
    it('should integrate with Ash resources', () => {
      const integrationOntology = `
        @prefix integration: <http://integration.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        
        integration:IntegrationEntity a owl:Class .
        integration:integrates a owl:ObjectProperty .
      `
      
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(integrationOntology)
      cy.get('[data-cy="output-format"]').select('reactor')
      cy.get('[data-cy="integrate-ash"]').check()
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Verify Ash integration in generated code
      cy.get('[data-cy="reactor-output"]').should('contain', 'Ash.')
      cy.get('[data-cy="reactor-output"]').should('contain', 'create')
      cy.get('[data-cy="reactor-output"]').should('contain', 'read')
      
      // Test execution with Ash backend
      cy.get('[data-cy="execute-reactor"]').click()
      
      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('reactor:ash_integration', (data) => {
            expect(data).to.have.property('ash_resources_created')
            expect(data).to.have.property('integration_status', 'success')
            resolve(data)
          })
        })
      })
    })
    
    it('should coordinate with pipeline permutations', () => {
      // Navigate to permutations tab first
      cy.get('[data-cy="tab-permutations"]').click()
      cy.get('[data-cy="generate-permutation"]').click()
      
      // Get permutation that includes reactor
      cy.get('[data-cy="permutation-list"]').contains('reactor').click()
      
      // Execute permutation with reactor integration
      cy.get('[data-cy="execute-permutation"]').click()
      
      // Verify reactor is part of the permutation
      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('permutation:reactor_step', (data) => {
            expect(data).to.have.property('step_type', 'reactor')
            expect(data).to.have.property('workflow_definition')
            expect(data).to.have.property('integration_points')
            resolve(data)
          })
        })
      })
      
      cy.get('[data-cy="permutation-reactor-step"]').should('be.visible')
      cy.get('[data-cy="reactor-permutation-status"]').should('contain', 'integrated')
    })
    
    it('should validate end-to-end pipeline with reactor output', () => {
      const e2eOntology = `
        @prefix e2e: <http://e2e.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        
        e2e:PipelineEntity a owl:Class .
        e2e:flowsTo a owl:ObjectProperty ;
          owl:domain e2e:PipelineEntity ;
          owl:range e2e:PipelineEntity .
      `
      
      // Start from transformer
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(e2eOntology)
      cy.get('[data-cy="output-format"]').select('reactor')
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Execute in real-time executor
      cy.get('[data-cy="tab-executor"]').click()
      cy.get('[data-cy="execution-mode"]').select('sequential')
      cy.get('[data-cy="start-pipeline"]').click()
      
      // Verify reactor stage execution
      cy.get('[data-cy="stage-reactor"]').should('have.class', 'running')
      cy.get('[data-cy="stage-reactor"]', { timeout: 30000 }).should('have.class', 'completed')
      
      // Check final output
      cy.get('[data-cy="pipeline-output"]').should('contain', 'reactor workflow executed')
      cy.get('[data-cy="execution-status"]').should('contain', 'Completed')
    })
  })
})