// 🔥 ULTRATHINK SWARM 80/20: Ash Integration Tests
// Testing Nuxt UI → WebSocket → Elixir Ash framework pipeline flow

describe('🔥 Nuxt to Ash Framework Integration', () => {
  beforeEach(() => {
    cy.visit('/')
    cy.resetSwarmState()
    cy.connectToSwarmChannel()
  })
  
  afterEach(() => {
    cy.disconnectSwarmChannel()
  })
  
  describe('📝 Ash Resource Generation', () => {
    it('should generate valid Ash resources from TTL ontology', () => {
      const testOntology = `
        @prefix cyber: <http://cybersecurity.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        
        cyber:Asset a owl:Class .
        cyber:Threat a owl:Class .
        
        cyber:name a owl:DatatypeProperty ;
          owl:domain cyber:Asset ;
          owl:range xsd:string .
          
        cyber:severity a owl:DatatypeProperty ;
          owl:domain cyber:Threat ;
          owl:range xsd:integer .
          
        cyber:threatens a owl:ObjectProperty ;
          owl:domain cyber:Threat ;
          owl:range cyber:Asset .
      `
      
      // Navigate to transformer
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(testOntology)
      cy.get('[data-cy="output-format"]').select('ash')
      cy.get('[data-cy="transform-button"]').click()
      
      // Wait for transformation to complete
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Verify Ash resource structure
      cy.get('[data-cy="ash-output"]').should('be.visible')
      cy.get('[data-cy="ash-output"]').should('contain', 'defmodule')
      cy.get('[data-cy="ash-output"]').should('contain', 'use Ash.Resource')
      cy.get('[data-cy="ash-output"]').should('contain', 'attributes do')
      cy.get('[data-cy="ash-output"]').should('contain', 'relationships do')
    })
    
    it('should generate proper Ash attribute types', () => {
      const typedOntology = `
        @prefix test: <http://test.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        
        test:Entity a owl:Class .
        
        test:stringProp a owl:DatatypeProperty ;
          owl:domain test:Entity ;
          owl:range xsd:string .
          
        test:intProp a owl:DatatypeProperty ;
          owl:domain test:Entity ;
          owl:range xsd:integer .
          
        test:boolProp a owl:DatatypeProperty ;
          owl:domain test:Entity ;
          owl:range xsd:boolean .
          
        test:dateProp a owl:DatatypeProperty ;
          owl:domain test:Entity ;
          owl:range xsd:dateTime .
      `
      
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(typedOntology)
      cy.get('[data-cy="output-format"]').select('ash')
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Verify correct Ash type mappings
      cy.get('[data-cy="ash-output"]').should('contain', 'attribute :string_prop, :string')
      cy.get('[data-cy="ash-output"]').should('contain', 'attribute :int_prop, :integer')
      cy.get('[data-cy="ash-output"]').should('contain', 'attribute :bool_prop, :boolean')
      cy.get('[data-cy="ash-output"]').should('contain', 'attribute :date_prop, :utc_datetime')
    })
    
    it('should generate Ash relationships correctly', () => {
      const relationshipOntology = `
        @prefix rel: <http://relationship.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        
        rel:User a owl:Class .
        rel:Post a owl:Class .
        rel:Comment a owl:Class .
        
        rel:authoredBy a owl:ObjectProperty ;
          owl:domain rel:Post ;
          owl:range rel:User .
          
        rel:hasComment a owl:ObjectProperty ;
          owl:domain rel:Post ;
          owl:range rel:Comment .
      `
      
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(relationshipOntology)
      cy.get('[data-cy="output-format"]').select('ash')
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Verify relationship generation
      cy.get('[data-cy="ash-output"]').should('contain', 'belongs_to :user')
      cy.get('[data-cy="ash-output"]').should('contain', 'has_many :comments')
      cy.get('[data-cy="ash-output"]').should('contain', 'relationships do')
    })
    
    it('should validate generated Ash syntax', () => {
      const validationOntology = `
        @prefix valid: <http://validation.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        
        valid:Product a owl:Class .
        
        valid:name a owl:DatatypeProperty ;
          owl:domain valid:Product ;
          owl:range xsd:string .
          
        valid:price a owl:DatatypeProperty ;
          owl:domain valid:Product ;
          owl:range xsd:decimal .
      `
      
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(validationOntology)
      cy.get('[data-cy="output-format"]').select('ash')
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Validate Elixir syntax
      cy.get('[data-cy="validate-syntax"]').click()
      cy.get('[data-cy="syntax-validation"]').should('contain', 'Valid Elixir syntax')
      
      // Check for required Ash components
      cy.get('[data-cy="ash-output"]').then(($output) => {
        const content = $output.text()
        expect(content).to.include('defmodule')
        expect(content).to.include('use Ash.Resource')
        expect(content).to.include('attributes do')
        expect(content).to.include('actions do')
      })
    })
  })
  
  describe('🔧 Ash Domain Configuration', () => {
    it('should generate Ash domain with multiple resources', () => {
      const domainOntology = `
        @prefix domain: <http://domain.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        
        domain:User a owl:Class .
        domain:Role a owl:Class .
        domain:Permission a owl:Class .
        
        domain:hasRole a owl:ObjectProperty ;
          owl:domain domain:User ;
          owl:range domain:Role .
          
        domain:hasPermission a owl:ObjectProperty ;
          owl:domain domain:Role ;
          owl:range domain:Permission .
      `
      
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(domainOntology)
      cy.get('[data-cy="output-format"]').select('ash')
      cy.get('[data-cy="generate-domain"]').check()
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Verify domain generation
      cy.get('[data-cy="ash-domain-output"]').should('be.visible')
      cy.get('[data-cy="ash-domain-output"]').should('contain', 'use Ash.Domain')
      cy.get('[data-cy="ash-domain-output"]').should('contain', 'resources do')
      cy.get('[data-cy="ash-domain-output"]').should('contain', 'resource User')
      cy.get('[data-cy="ash-domain-output"]').should('contain', 'resource Role')
      cy.get('[data-cy="ash-domain-output"]').should('contain', 'resource Permission')
    })
    
    it('should configure Ash actions based on ontology patterns', () => {
      const actionOntology = `
        @prefix action: <http://action.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        
        action:Document a owl:Class .
        
        action:title a owl:DatatypeProperty ;
          owl:domain action:Document ;
          owl:range xsd:string .
          
        action:status a owl:DatatypeProperty ;
          owl:domain action:Document ;
          owl:range xsd:string .
      `
      
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(actionOntology)
      cy.get('[data-cy="output-format"]').select('ash')
      cy.get('[data-cy="generate-actions"]').check()
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Verify CRUD actions generation
      cy.get('[data-cy="ash-output"]').should('contain', 'actions do')
      cy.get('[data-cy="ash-output"]').should('contain', 'create :create')
      cy.get('[data-cy="ash-output"]').should('contain', 'read :read')
      cy.get('[data-cy="ash-output"]').should('contain', 'update :update')
      cy.get('[data-cy="ash-output"]').should('contain', 'destroy :destroy')
    })
    
    it('should generate Ash policies for security', () => {
      const securityOntology = `
        @prefix sec: <http://security.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        
        sec:SecureResource a owl:Class .
        
        sec:owner a owl:ObjectProperty ;
          owl:domain sec:SecureResource ;
          owl:range sec:User .
          
        sec:visibility a owl:DatatypeProperty ;
          owl:domain sec:SecureResource ;
          owl:range xsd:string .
      `
      
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(securityOntology)
      cy.get('[data-cy="output-format"]').select('ash')
      cy.get('[data-cy="generate-policies"]').check()
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Verify policy generation
      cy.get('[data-cy="ash-output"]').should('contain', 'policies do')
      cy.get('[data-cy="ash-output"]').should('contain', 'policy action_type')
      cy.get('[data-cy="ash-output"]').should('contain', 'authorize_if')
    })
  })
  
  describe('🚀 Ash Execution Testing', () => {
    beforeEach(() => {
      // Set up test ontology for execution
      const execOntology = `
        @prefix exec: <http://execution.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        
        exec:TestEntity a owl:Class .
        
        exec:name a owl:DatatypeProperty ;
          owl:domain exec:TestEntity ;
          owl:range xsd:string .
      `
      
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(execOntology)
      cy.get('[data-cy="output-format"]').select('ash')
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
    })
    
    it('should test Ash resource CRUD operations via WebSocket', () => {
      cy.get('[data-cy="test-ash-crud"]').click()
      
      // Monitor execution via WebSocket
      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('ash:crud_test_started', (data) => {
            expect(data).to.have.property('resource_name')
            expect(data).to.have.property('operations', ['create', 'read', 'update', 'destroy'])
            resolve(data)
          })
        })
      })
      
      // Wait for CRUD test completion
      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('ash:crud_test_completed', (data) => {
            expect(data).to.have.property('results')
            expect(data.results).to.have.property('create', 'success')
            expect(data.results).to.have.property('read', 'success')
            expect(data.results).to.have.property('update', 'success')
            expect(data.results).to.have.property('destroy', 'success')
            resolve(data)
          })
        })
      })
      
      cy.get('[data-cy="ash-crud-status"]').should('contain', 'All CRUD operations successful')
    })
    
    it('should handle Ash validation errors', () => {
      // Generate resource with validation rules
      const validationOntology = `
        @prefix valid: <http://validation.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        
        valid:ValidatedEntity a owl:Class .
        
        valid:email a owl:DatatypeProperty ;
          owl:domain valid:ValidatedEntity ;
          owl:range xsd:string .
      `
      
      cy.get('[data-cy="ontology-input"]').clear().type(validationOntology)
      cy.get('[data-cy="generate-validations"]').check()
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Test with invalid data
      cy.get('[data-cy="test-ash-validation"]').click()
      cy.get('[data-cy="test-data-input"]').type('{"email": "invalid-email"}')
      cy.get('[data-cy="submit-test-data"]').click()
      
      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('ash:validation_error', (error) => {
            expect(error).to.have.property('field', 'email')
            expect(error).to.have.property('message')
            expect(error.message).to.include('invalid format')
            resolve(error)
          })
        })
      })
      
      cy.get('[data-cy="ash-validation-errors"]').should('be.visible')
      cy.get('[data-cy="ash-validation-errors"]').should('contain', 'email')
    })
    
    it('should test Ash queries and filters', () => {
      const queryOntology = `
        @prefix query: <http://query.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        
        query:Product a owl:Class .
        
        query:name a owl:DatatypeProperty ;
          owl:domain query:Product ;
          owl:range xsd:string .
          
        query:price a owl:DatatypeProperty ;
          owl:domain query:Product ;
          owl:range xsd:decimal .
          
        query:category a owl:DatatypeProperty ;
          owl:domain query:Product ;
          owl:range xsd:string .
      `
      
      cy.get('[data-cy="ontology-input"]').clear().type(queryOntology)
      cy.get('[data-cy="generate-filters"]').check()
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Test query execution
      cy.get('[data-cy="test-ash-queries"]').click()
      
      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('ash:query_results', (data) => {
            expect(data).to.have.property('query_type')
            expect(data).to.have.property('results')
            expect(data).to.have.property('count')
            resolve(data)
          })
        })
      })
      
      // Verify filter generation in code
      cy.get('[data-cy="ash-output"]').should('contain', 'filter name')
      cy.get('[data-cy="ash-output"]').should('contain', 'filter price')
      cy.get('[data-cy="ash-output"]').should('contain', 'filter category')
    })
  })
  
  describe('📊 Ash Performance and Optimization', () => {
    it('should optimize Ash resource loading', () => {
      const perfOntology = `
        @prefix perf: <http://performance.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        
        perf:User a owl:Class .
        perf:Post a owl:Class .
        perf:Comment a owl:Class .
        
        perf:author a owl:ObjectProperty ;
          owl:domain perf:Post ;
          owl:range perf:User .
          
        perf:hasComment a owl:ObjectProperty ;
          owl:domain perf:Post ;
          owl:range perf:Comment .
      `
      
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(perfOntology)
      cy.get('[data-cy="output-format"]').select('ash')
      cy.get('[data-cy="optimize-loading"]').check()
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Verify optimization features in generated code
      cy.get('[data-cy="ash-output"]').should('contain', 'preparations do')
      cy.get('[data-cy="ash-output"]').should('contain', 'prepare build')
      
      // Test performance with optimizations
      cy.get('[data-cy="test-performance"]').click()
      
      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('ash:performance_result', (data) => {
            expect(data).to.have.property('query_time')
            expect(data).to.have.property('n_plus_one_queries', 0)
            expect(data.query_time).to.be.lessThan(1000) // Less than 1 second
            resolve(data)
          })
        })
      })
    })
    
    it('should generate efficient Ash aggregates', () => {
      const aggregateOntology = `
        @prefix agg: <http://aggregate.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        
        agg:Order a owl:Class .
        agg:OrderItem a owl:Class .
        
        agg:belongsTo a owl:ObjectProperty ;
          owl:domain agg:OrderItem ;
          owl:range agg:Order .
          
        agg:quantity a owl:DatatypeProperty ;
          owl:domain agg:OrderItem ;
          owl:range xsd:integer .
          
        agg:price a owl:DatatypeProperty ;
          owl:domain agg:OrderItem ;
          owl:range xsd:decimal .
      `
      
      cy.get('[data-cy="ontology-input"]').clear().type(aggregateOntology)
      cy.get('[data-cy="generate-aggregates"]').check()
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Verify aggregate generation
      cy.get('[data-cy="ash-output"]').should('contain', 'aggregates do')
      cy.get('[data-cy="ash-output"]').should('contain', 'count :order_items_count')
      cy.get('[data-cy="ash-output"]').should('contain', 'sum :total_amount')
      
      // Test aggregate performance
      cy.get('[data-cy="test-aggregates"]').click()
      
      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('ash:aggregate_test', (data) => {
            expect(data).to.have.property('aggregates_computed')
            expect(data).to.have.property('execution_time')
            expect(data.execution_time).to.be.lessThan(500) // Less than 500ms
            resolve(data)
          })
        })
      })
    })
    
    it('should benchmark Ash vs traditional approaches', () => {
      const benchmarkOntology = `
        @prefix bench: <http://benchmark.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        
        bench:BenchEntity a owl:Class .
        
        bench:name a owl:DatatypeProperty ;
          owl:domain bench:BenchEntity ;
          owl:range xsd:string .
      `
      
      cy.get('[data-cy="ontology-input"]').clear().type(benchmarkOntology)
      cy.get('[data-cy="run-benchmark"]').check()
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      cy.get('[data-cy="start-benchmark"]').click()
      
      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('ash:benchmark_results', (data) => {
            expect(data).to.have.property('ash_time')
            expect(data).to.have.property('traditional_time')
            expect(data).to.have.property('improvement_percentage')
            expect(data.improvement_percentage).to.be.greaterThan(0)
            resolve(data)
          })
        })
      })
      
      cy.get('[data-cy="benchmark-results"]').should('be.visible')
      cy.get('[data-cy="performance-improvement"]').should('contain', '%')
    })
  })
  
  describe('🔗 Ash Integration with Pipeline', () => {
    it('should integrate Ash with Reactor workflows', () => {
      const integrationOntology = `
        @prefix int: <http://integration.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        
        int:WorkflowEntity a owl:Class .
        
        int:status a owl:DatatypeProperty ;
          owl:domain int:WorkflowEntity ;
          owl:range xsd:string .
      `
      
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(integrationOntology)
      cy.get('[data-cy="output-format"]').select('ash')
      cy.get('[data-cy="integrate-reactor"]').check()
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Verify Reactor integration in Ash code
      cy.get('[data-cy="ash-output"]').should('contain', 'use Reactor')
      cy.get('[data-cy="ash-output"]').should('contain', 'step :')
      
      // Test integrated execution
      cy.get('[data-cy="test-ash-reactor"]').click()
      
      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('ash:reactor_integration', (data) => {
            expect(data).to.have.property('ash_operations')
            expect(data).to.have.property('reactor_steps')
            expect(data).to.have.property('integration_status', 'success')
            resolve(data)
          })
        })
      })
    })
    
    it('should coordinate with permutation matrix', () => {
      // Navigate to permutations tab
      cy.get('[data-cy="tab-permutations"]').click()
      cy.get('[data-cy="generate-permutation"]').click()
      
      // Select permutation with Ash integration
      cy.get('[data-cy="permutation-list"]').contains('ash').click()
      cy.get('[data-cy="execute-permutation"]').click()
      
      // Verify Ash is part of the permutation
      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('permutation:ash_step', (data) => {
            expect(data).to.have.property('step_type', 'ash')
            expect(data).to.have.property('resources_generated')
            expect(data).to.have.property('domain_configured')
            resolve(data)
          })
        })
      })
      
      cy.get('[data-cy="permutation-ash-step"]').should('be.visible')
      cy.get('[data-cy="ash-permutation-status"]').should('contain', 'resources created')
    })
    
    it('should validate complete pipeline with Ash backend', () => {
      const pipelineOntology = `
        @prefix pipeline: <http://pipeline.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        
        pipeline:PipelineData a owl:Class .
        
        pipeline:processed a owl:DatatypeProperty ;
          owl:domain pipeline:PipelineData ;
          owl:range xsd:boolean .
      `
      
      // Execute full pipeline with Ash as target
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(pipelineOntology)
      cy.get('[data-cy="output-format"]').select('ash')
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Execute in real-time executor
      cy.get('[data-cy="tab-executor"]').click()
      cy.get('[data-cy="execution-mode"]').select('sequential')
      cy.get('[data-cy="start-pipeline"]').click()
      
      // Verify Ash stage execution
      cy.get('[data-cy="stage-ash"]').should('have.class', 'running')
      cy.get('[data-cy="stage-ash"]', { timeout: 30000 }).should('have.class', 'completed')
      
      // Verify Ash resources were created
      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('ash:pipeline_complete', (data) => {
            expect(data).to.have.property('resources_created')
            expect(data).to.have.property('domain_configured')
            expect(data).to.have.property('migrations_generated')
            resolve(data)
          })
        })
      })
      
      cy.get('[data-cy="pipeline-output"]').should('contain', 'Ash resources created')
      cy.get('[data-cy="execution-status"]').should('contain', 'Completed')
    })
  })
  
  describe('🛠️ Ash Development Tools Integration', () => {
    it('should generate Ash migrations', () => {
      const migrationOntology = `
        @prefix mig: <http://migration.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        
        mig:MigrationTest a owl:Class .
        
        mig:field1 a owl:DatatypeProperty ;
          owl:domain mig:MigrationTest ;
          owl:range xsd:string .
      `
      
      cy.get('[data-cy="tab-transformer"]').click()
      cy.get('[data-cy="ontology-input"]').clear().type(migrationOntology)
      cy.get('[data-cy="output-format"]').select('ash')
      cy.get('[data-cy="generate-migrations"]').check()
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Verify migration generation
      cy.get('[data-cy="ash-migrations-output"]').should('be.visible')
      cy.get('[data-cy="ash-migrations-output"]').should('contain', 'create table')
      cy.get('[data-cy="ash-migrations-output"]').should('contain', 'add_column')
    })
    
    it('should generate Ash GraphQL schema', () => {
      const graphqlOntology = `
        @prefix gql: <http://graphql.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        
        gql:GraphQLEntity a owl:Class .
        
        gql:name a owl:DatatypeProperty ;
          owl:domain gql:GraphQLEntity ;
          owl:range xsd:string .
      `
      
      cy.get('[data-cy="ontology-input"]').clear().type(graphqlOntology)
      cy.get('[data-cy="generate-graphql"]').check()
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Verify GraphQL schema generation
      cy.get('[data-cy="ash-graphql-output"]').should('be.visible')
      cy.get('[data-cy="ash-graphql-output"]').should('contain', 'use AshGraphql.Domain')
      cy.get('[data-cy="ash-graphql-output"]').should('contain', 'graphql do')
    })
    
    it('should validate generated code with Ash formatter', () => {
      const formatOntology = `
        @prefix fmt: <http://format.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        
        fmt:FormatTest a owl:Class .
      `
      
      cy.get('[data-cy="ontology-input"]').clear().type(formatOntology)
      cy.get('[data-cy="transform-button"]').click()
      
      cy.get('[data-cy="transformation-progress"]', { timeout: 30000 })
        .should('contain', '100%')
      
      // Format generated code
      cy.get('[data-cy="format-ash-code"]').click()
      
      cy.window().then((win) => {
        return new Promise((resolve) => {
          win.swarmChannel.on('ash:code_formatted', (data) => {
            expect(data).to.have.property('formatted_code')
            expect(data).to.have.property('format_status', 'success')
            resolve(data)
          })
        })
      })
      
      cy.get('[data-cy="format-status"]').should('contain', 'Code formatted successfully')
    })
  })
})