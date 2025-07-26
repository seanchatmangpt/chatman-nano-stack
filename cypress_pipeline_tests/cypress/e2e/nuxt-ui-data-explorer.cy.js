describe('Interactive Data Explorer E2E Tests', () => {
  beforeEach(() => {
    // Visit the application
    cy.visit('http://localhost:3022')
    
    // Wait for application to load
    cy.get('body').should('be.visible')
    
    // Setup API interceptors
        cy.intercept('GET', '**/api/typer/**', { fixture: 'typer_response.json' }).as('typerRequest')
    cy.intercept('POST', '**/api/typer/**', { statusCode: 200, body: { success: true } }).as('typerPost')
    cy.intercept('GET', '**/api/turtle/**', { fixture: 'turtle_response.json' }).as('turtleRequest')
    cy.intercept('POST', '**/api/turtle/**', { statusCode: 200, body: { success: true } }).as('turtlePost')
    cy.intercept('GET', '**/api/ttl2dspy/**', { fixture: 'ttl2dspy_response.json' }).as('ttl2dspyRequest')
    cy.intercept('POST', '**/api/ttl2dspy/**', { statusCode: 200, body: { success: true } }).as('ttl2dspyPost')
    cy.intercept('GET', '**/api/ash/**', { fixture: 'ash_response.json' }).as('ashRequest')
    cy.intercept('POST', '**/api/ash/**', { statusCode: 200, body: { success: true } }).as('ashPost')
  })

  describe('UI Component Tests', () => {
    
    it('should handle virtual scrolling for large datasets', () => {
      cy.get('[data-cy=data-table]').should('be.visible')
      cy.get('[data-cy=virtual-scroller]').should('exist')
      
      // Scroll through large dataset
      cy.get('[data-cy=virtual-scroller]').scrollTo('bottom')
      cy.get('[data-cy=table-row]').should('be.visible')
    })
  })

  describe('Pipeline Integration Tests', () => {
    
    it('should connect to typer API endpoint', () => {
      cy.request('GET', Cypress.env('PIPELINE_API_URL') + '/typer/health')
        .its('status')
        .should('eq', 200)
        
      // Test data flow
      cy.get('[data-cy=typer-status]').should('contain', 'active')
    })

    it('should connect to turtle API endpoint', () => {
      cy.request('GET', Cypress.env('PIPELINE_API_URL') + '/turtle/health')
        .its('status')
        .should('eq', 200)
        
      // Test data flow
      cy.get('[data-cy=turtle-status]').should('contain', 'active')
    })

    it('should connect to ttl2dspy API endpoint', () => {
      cy.request('GET', Cypress.env('PIPELINE_API_URL') + '/ttl2dspy/health')
        .its('status')
        .should('eq', 200)
        
      // Test data flow
      cy.get('[data-cy=ttl2dspy-status]').should('contain', 'active')
    })

    it('should connect to ash API endpoint', () => {
      cy.request('GET', Cypress.env('PIPELINE_API_URL') + '/ash/health')
        .its('status')
        .should('eq', 200)
        
      // Test data flow
      cy.get('[data-cy=ash-status]').should('contain', 'active')
    })

    it('should execute complete pipeline flow', () => {
      const endpoints = ["typer", "turtle", "ttl2dspy", "ash"]
      
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