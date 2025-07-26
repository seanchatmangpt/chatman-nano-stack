describe('Complete Pipeline Integration Tests', () => {
  const applications = [
    { name: 'Command Center', port: 3020, id: 'command-center' },
    { name: 'Workflow Studio', port: 3021, id: 'workflow-studio' },
    { name: 'Data Explorer', port: 3022, id: 'data-explorer' },
    { name: 'Performance Hub', port: 3030, id: 'performance-hub' },
    { name: 'Semantic Playground', port: 3040, id: 'semantic-playground' }
  ]

  beforeEach(() => {
    // Setup global interceptors for all pipeline services
    cy.intercept('GET', '**/api/health', { statusCode: 200, body: { status: 'healthy' } }).as('healthCheck')
    cy.intercept('POST', '**/api/pipeline/execute', { statusCode: 200, body: { success: true, executionId: 'test-123' } }).as('pipelineExecution')
    cy.intercept('GET', '**/api/channels/status', { statusCode: 200, body: { connected: true, channels: ['pipeline', 'metrics'] } }).as('channelStatus')
  })

  describe('Cross-Application Pipeline Flow', () => {
    it('should execute end-to-end pipeline across all applications', () => {
      applications.forEach(app => {
        cy.visit(`http://localhost:${app.port}`)
        cy.get('[data-cy=pipeline-trigger]').click()
        cy.wait('@pipelineExecution')
        cy.get('[data-cy=execution-status]').should('contain', 'success')
      })
    })

    it('should synchronize data across applications via channels', () => {
      // Start monitoring on Command Center
      cy.visit('http://localhost:3020')
      cy.get('[data-cy=start-monitoring]').click()
      
      // Trigger workflow in Workflow Studio
      cy.visit('http://localhost:3021')
      cy.get('[data-cy=deploy-workflow]').click()
      
      // Verify update appears in Performance Hub
      cy.visit('http://localhost:3030')
      cy.get('[data-cy=recent-deployments]').should('contain', 'New workflow deployed')
    })
  })

  describe('Reactor Workflow Integration', () => {
    it('should create and execute Reactor workflows', () => {
      cy.visit('http://localhost:3021') // Workflow Studio
      
      // Create workflow
      cy.get('[data-cy=new-workflow]').click()
      cy.get('[data-cy=workflow-name]').type('Test Pipeline Workflow')
      
      // Add pipeline stages
      cy.get('[data-cy=add-stage]').click()
      cy.get('[data-cy=stage-type]').select('typer')
      cy.get('[data-cy=save-stage]').click()
      
      // Deploy to Reactor
      cy.get('[data-cy=deploy-reactor]').click()
      cy.wait('@pipelineExecution')
      
      // Verify deployment
      cy.get('[data-cy=deployment-status]').should('contain', 'deployed')
    })
  })

  describe('Ash Resource Management', () => {
    it('should manage Ash resources across applications', () => {
      cy.visit('http://localhost:3022') // Data Explorer
      
      // Browse Ash resources
      cy.get('[data-cy=resource-browser]').should('be.visible')
      cy.get('[data-cy=resource-list]').should('contain', 'User')
      
      // Create new resource
      cy.get('[data-cy=new-resource]').click()
      cy.get('[data-cy=resource-name]').type('TestResource')
      cy.get('[data-cy=save-resource]').click()
      
      // Verify resource appears in other apps
      cy.visit('http://localhost:3040') // Semantic Playground
      cy.get('[data-cy=ash-resources]').should('contain', 'TestResource')
    })
  })

  describe('Elixir Channels Communication', () => {
    it('should establish and maintain channel connections', () => {
      applications.forEach(app => {
        cy.visit(`http://localhost:${app.port}`)
        
        // Verify channel connection
        cy.window().then((win) => {
          expect(win.phoenixSocket).to.exist
          expect(win.phoenixSocket.isConnected()).to.be.true
        })
        
        // Test channel subscription
        cy.get('[data-cy=channel-status]').should('contain', 'connected')
      })
    })

    it('should broadcast and receive real-time messages', () => {
      // Setup listeners on multiple apps
      cy.visit('http://localhost:3020') // Command Center
      cy.get('[data-cy=subscribe-updates]').click()
      
      cy.visit('http://localhost:3030') // Performance Hub  
      cy.get('[data-cy=subscribe-metrics]').click()
      
      // Trigger broadcast from Semantic Playground
      cy.visit('http://localhost:3040')
      cy.get('[data-cy=broadcast-update]').click()
      
      // Verify other apps received update
      cy.visit('http://localhost:3020')
      cy.get('[data-cy=latest-broadcast]').should('not.be.empty')
    })
  })

  describe('Performance and Load Testing', () => {
    it('should handle concurrent operations across applications', () => {
      // Simulate load across all applications
      const promises = applications.map(app => {
        return new Promise(resolve => {
          cy.visit(`http://localhost:${app.port}`)
          cy.get('[data-cy=stress-test]').click()
          cy.get('[data-cy=stress-complete]').should('be.visible')
          resolve()
        })
      })
      
      // All applications should complete stress test
      cy.wrap(Promise.all(promises)).should('be.fulfilled')
    })
  })
})