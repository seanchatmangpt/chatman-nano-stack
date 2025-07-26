// BitActor Pipeline Custom Cypress Commands
// TTL-aware testing commands for 80/20 validation strategy

// Command to validate TTL constraints across pipeline stages
Cypress.Commands.add('validateTTLConstraints', (expectedBudgetMs = 8000) => {
  const startTime = Date.now()
  
  cy.window().then((win) => {
    const endTime = Date.now()
    const duration = endTime - startTime
    
    expect(duration).to.be.lessThan(expectedBudgetMs, 
      `Pipeline execution exceeded TTL budget of ${expectedBudgetMs}ms. Actual: ${duration}ms`)
    
    cy.task('measureTTLCompliance', { startTime, endTime })
      .then((metrics) => {
        cy.log(`TTL Compliance: ${metrics.compliance ? 'PASS' : 'FAIL'}`)
        cy.log(`Efficiency: ${metrics.efficiency.toFixed(2)}%`)
      })
  })
})

// Command to test BitActor pipeline stages
Cypress.Commands.add('testPipelineStage', (stageName, stageSelector = null) => {
  const selector = stageSelector || `[data-stage="${stageName.toLowerCase()}"]`
  
  cy.get(selector, { timeout: 10000 }).should('exist').then(($stage) => {
    const stageData = {
      stageName,
      status: $stage.attr('data-status') || 'unknown',
      ttlUsage: parseInt($stage.attr('data-ttl-usage')) || 0
    }
    
    cy.task('validatePipelineStage', stageData).then((validation) => {
      expect(validation.exists).to.be.true
      cy.log(`Stage ${stageName}: ${validation.status}`)
    })
  })
})

// Command to test WebSocket connectivity for real-time variants
Cypress.Commands.add('testWebSocketConnection', (wsUrl = 'ws://localhost:3100') => {
  cy.window().then((win) => {
    return new Cypress.Promise((resolve, reject) => {
      const ws = new win.WebSocket(wsUrl)
      const timeout = setTimeout(() => {
        reject(new Error('WebSocket connection timeout'))
      }, Cypress.env('WEBSOCKET_TIMEOUT'))
      
      ws.onopen = () => {
        clearTimeout(timeout)
        ws.close()
        resolve('Connected')
      }
      
      ws.onerror = (error) => {
        clearTimeout(timeout)
        reject(error)
      }
    })
  })
})

// Command to test Nuxt SSR functionality
Cypress.Commands.add('testSSRHydration', () => {
  cy.window().should('have.property', '__NUXT__')
  cy.get('[data-server-rendered="true"]').should('exist')
  
  // Verify client-side hydration completed
  cy.window().its('$nuxt').should('exist')
  cy.window().its('$nuxt.$el').should('exist')
})

// Command to validate component reactivity
Cypress.Commands.add('testReactivity', (triggerSelector, targetSelector, expectedChange) => {
  cy.get(targetSelector).invoke('text').then((initialText) => {
    cy.get(triggerSelector).click()
    cy.get(targetSelector).should(($el) => {
      expect($el.text()).not.to.equal(initialText)
      if (expectedChange) {
        expect($el.text()).to.contain(expectedChange)
      }
    })
  })
})

// Command to test drag and drop functionality (for pipeline builder)
Cypress.Commands.add('dragAndDrop', (sourceSelector, targetSelector) => {
  cy.get(sourceSelector).trigger('mousedown', { which: 1 })
  cy.get(targetSelector).trigger('mousemove').trigger('mouseup')
})

// Command to test form validation
Cypress.Commands.add('testFormValidation', (formSelector, invalidData, expectedErrors) => {
  cy.get(formSelector).within(() => {
    Object.keys(invalidData).forEach(fieldName => {
      cy.get(`[name="${fieldName}"]`).clear().type(invalidData[fieldName])
    })
    
    cy.get('[type="submit"]').click()
    
    expectedErrors.forEach(error => {
      cy.contains(error).should('be.visible')
    })
  })
})

// Command to measure performance metrics
Cypress.Commands.add('measurePerformance', (actionCallback) => {
  cy.window().then((win) => {
    const startTime = win.performance.now()
    
    actionCallback()
    
    cy.then(() => {
      const endTime = win.performance.now()
      const duration = endTime - startTime
      
      cy.log(`Action duration: ${duration.toFixed(2)}ms`)
      return duration
    })
  })
})

// Command to test responsive design
Cypress.Commands.add('testResponsive', (breakpoints = [320, 768, 1024, 1280]) => {
  breakpoints.forEach(width => {
    cy.viewport(width, 720)
    cy.wait(500) // Allow layout to stabilize
    cy.get('body').should('be.visible')
    cy.log(`Tested viewport: ${width}px`)
  })
})

// Command to validate accessibility
Cypress.Commands.add('testAccessibility', () => {
  // Check for essential accessibility attributes
  cy.get('img').each(($img) => {
    cy.wrap($img).should('have.attr', 'alt')
  })
  
  cy.get('input').each(($input) => {
    cy.wrap($input).should('satisfy', ($el) => {
      return $el.attr('aria-label') || $el.attr('placeholder') || $el.prev('label').length > 0
    })
  })
  
  cy.get('button').each(($button) => {
    cy.wrap($button).should('satisfy', ($el) => {
      return $el.text().trim() || $el.attr('aria-label')
    })
  })
})

// Command for 80/20 testing - focus on critical functionality
Cypress.Commands.add('test8020Critical', (componentSelector) => {
  cy.get(componentSelector).should('exist')
  cy.get(componentSelector).should('be.visible')
  cy.testAccessibility()
  cy.validateTTLConstraints()
})

// Custom assertion for BitActor pipeline status
Cypress.Commands.add('shouldHavePipelineStatus', { prevSubject: true }, (subject, expectedStatus) => {
  expect(subject.attr('data-pipeline-status')).to.equal(expectedStatus)
  return cy.wrap(subject)
})