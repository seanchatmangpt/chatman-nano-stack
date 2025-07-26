describe('Command Center Dashboard E2E Tests', () => {
  beforeEach(() => {
    // Visit the application
    cy.visit('http://localhost:3020')
    
    // Wait for application to load
    cy.get('body').should('be.visible')
    
    // Setup API interceptors
        cy.intercept('GET', '**/api/typer/**', { fixture: 'typer_response.json' }).as('typerRequest')
    cy.intercept('POST', '**/api/typer/**', { statusCode: 200, body: { success: true } }).as('typerPost')
    cy.intercept('GET', '**/api/turtle/**', { fixture: 'turtle_response.json' }).as('turtleRequest')
    cy.intercept('POST', '**/api/turtle/**', { statusCode: 200, body: { success: true } }).as('turtlePost')
    cy.intercept('GET', '**/api/ttl2dspy/**', { fixture: 'ttl2dspy_response.json' }).as('ttl2dspyRequest')
    cy.intercept('POST', '**/api/ttl2dspy/**', { statusCode: 200, body: { success: true } }).as('ttl2dspyPost')
    cy.intercept('GET', '**/api/bitactor/**', { fixture: 'bitactor_response.json' }).as('bitactorRequest')
    cy.intercept('POST', '**/api/bitactor/**', { statusCode: 200, body: { success: true } }).as('bitactorPost')
    cy.intercept('GET', '**/api/reactor/**', { fixture: 'reactor_response.json' }).as('reactorRequest')
    cy.intercept('POST', '**/api/reactor/**', { statusCode: 200, body: { success: true } }).as('reactorPost')
    cy.intercept('GET', '**/api/k8s/**', { fixture: 'k8s_response.json' }).as('k8sRequest')
    cy.intercept('POST', '**/api/k8s/**', { statusCode: 200, body: { success: true } }).as('k8sPost')
  })

  describe('UI Component Tests', () => {
    
    it('should open command palette with ⌘K', () => {
      cy.get('body').type('{cmd}k')
      cy.get('[data-cy=command-palette]').should('be.visible')
      cy.get('[data-cy=command-search]').type('restart')
      cy.get('[data-cy=command-option]').first().click()
    })

    it('should display real-time metrics', () => {
      cy.get('[data-cy=metric-card]').should('have.length.at.least', 3)
      cy.get('[data-cy=metric-value]').should('contain.text', '/')
      cy.get('[data-cy=metric-trend]').should('be.visible')
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

    it('should connect to bitactor API endpoint', () => {
      cy.request('GET', Cypress.env('PIPELINE_API_URL') + '/bitactor/health')
        .its('status')
        .should('eq', 200)
        
      // Test data flow
      cy.get('[data-cy=bitactor-status]').should('contain', 'active')
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
      const endpoints = ["typer", "turtle", "ttl2dspy", "bitactor", "reactor", "k8s"]
      
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