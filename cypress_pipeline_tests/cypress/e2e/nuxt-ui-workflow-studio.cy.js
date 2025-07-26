describe('Visual Workflow Studio E2E Tests', () => {
  beforeEach(() => {
    // Visit the application
    cy.visit('http://localhost:3021')
    
    // Wait for application to load
    cy.get('body').should('be.visible')
    
    // Setup API interceptors
        cy.intercept('GET', '**/api/ash/**', { fixture: 'ash_response.json' }).as('ashRequest')
    cy.intercept('POST', '**/api/ash/**', { statusCode: 200, body: { success: true } }).as('ashPost')
    cy.intercept('GET', '**/api/reactor/**', { fixture: 'reactor_response.json' }).as('reactorRequest')
    cy.intercept('POST', '**/api/reactor/**', { statusCode: 200, body: { success: true } }).as('reactorPost')
    cy.intercept('GET', '**/api/k8s/**', { fixture: 'k8s_response.json' }).as('k8sRequest')
    cy.intercept('POST', '**/api/k8s/**', { statusCode: 200, body: { success: true } }).as('k8sPost')
  })

  describe('UI Component Tests', () => {
    
    it('should support drag and drop workflow creation', () => {
      cy.get('[data-cy=component-palette]').should('be.visible')
      cy.get('[data-cy=workflow-canvas]').should('be.visible')
      
      // Drag component to canvas
      cy.get('[data-cy=component-typer]').trigger('dragstart')
      cy.get('[data-cy=workflow-canvas]').trigger('drop')
      
      // Verify component was added
      cy.get('[data-cy=canvas-node]').should('exist')
    })
  })

  describe('Pipeline Integration Tests', () => {
    
    it('should connect to ash API endpoint', () => {
      cy.request('GET', Cypress.env('PIPELINE_API_URL') + '/ash/health')
        .its('status')
        .should('eq', 200)
        
      // Test data flow
      cy.get('[data-cy=ash-status]').should('contain', 'active')
    })

    it('should connect to reactor API endpoint', () => {
      cy.request('GET', Cypress.env('PIPELINE_API_URL') + '/reactor/health')
        .its('status')
        .should('eq', 200)
        
      // Test data flow
      cy.get('[data-cy=reactor-status]').should('contain', 'active')
    })

    it('should connect to k8s API endpoint', () => {
      cy.request('GET', Cypress.env('PIPELINE_API_URL') + '/k8s/health')
        .its('status')
        .should('eq', 200)
        
      // Test data flow
      cy.get('[data-cy=k8s-status]').should('contain', 'active')
    })

    it('should execute complete pipeline flow', () => {
      const endpoints = ["ash", "reactor", "k8s"]
      
      // Start pipeline execution
      cy.get('[data-cy=execute-pipeline]').click()
      
      // Verify each stage completes
      endpoints.forEach(endpoint => {
        cy.get(`[data-cy=${endpoint}-stage]`).should('have.class', 'completed')
      })
      
      // Verify final output
      cy.get('[data-cy=pipeline-output]').should('be.visible')
    })
  })

  describe('Real-time Data Flow Tests', () => {
    
    it('should establish WebSocket connection', () => {
      cy.window().its('WebSocket').should('exist')
      
      // Mock WebSocket messages
      cy.window().then((win) => {
        const mockMessage = {
          type: 'pipeline_update',
          data: { status: 'processing', stage: 'typer' }
        }
        
        // Simulate WebSocket message
        win.postMessage(mockMessage, '*')
        cy.get('[data-cy=pipeline-status]').should('contain', 'processing')
      })
    })
    
    it('should handle real-time updates', () => {
      // Start real-time monitoring
      cy.get('[data-cy=enable-realtime]').click()
      
      // Verify updates are received
      cy.get('[data-cy=last-update-time]').should('not.be.empty')
      
      // Test auto-refresh functionality
      cy.get('[data-cy=auto-refresh-toggle]').should('be.checked')
    })
  })

  describe('Performance Tests', () => {
    
    it('should load within performance thresholds', () => {
      // Measure page load time
      cy.window().its('performance').invoke('mark', 'start-test')
      
      cy.get('[data-cy=main-content]').should('be.visible')
      
      cy.window().its('performance').invoke('mark', 'end-test')
      cy.window().its('performance').invoke('measure', 'page-load', 'start-test', 'end-test')
      
      cy.window().its('performance')
        .invoke('getEntriesByName', 'page-load')
        .its('0.duration')
        .should('be.lessThan', 3000) // 3 seconds max
    })
    
    it('should handle concurrent users simulation', () => {
      // Simulate multiple rapid interactions
      for (let i = 0; i < 10; i++) {
        cy.get('[data-cy=refresh-data]').click()
      }
      
      // Verify application remains responsive
      cy.get('[data-cy=loading-indicator]').should('not.exist')
      cy.get('[data-cy=error-message]').should('not.exist')
    })
  })
})