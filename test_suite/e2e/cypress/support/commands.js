// Custom commands for Ultrathink Swarm 80/20 testing

// Pipeline-specific commands
Cypress.Commands.add('selectOptimizationStrategy', (strategy) => {
  cy.get('[data-testid="optimization-selector"]')
    .select(strategy)
    .should('have.value', strategy)
})

Cypress.Commands.add('selectDomain', (domain) => {
  cy.get('[data-testid="domain-selector"]')
    .select(domain)
    .should('have.value', domain)
})

Cypress.Commands.add('executeOptimizedPipeline', (options = {}) => {
  const {
    strategy = 'skip_non_critical',
    domain = 'cybersecurity',
    complexity = 5,
    waitForCompletion = true
  } = options

  // Set configuration
  if (strategy) cy.selectOptimizationStrategy(strategy)
  if (domain) cy.selectDomain(domain)
  
  if (complexity) {
    cy.get('[data-testid="complexity-slider"]')
      .invoke('val', complexity)
      .trigger('input')
  }

  // Execute pipeline
  cy.get('[data-testid="execute-pipeline-btn"]')
    .should('be.enabled')
    .click()

  if (waitForCompletion) {
    cy.get('[data-testid="execution-status"]', { timeout: 15000 })
      .should('contain', 'completed')
  }
})

Cypress.Commands.add('verifyStageExecution', (stageId, shouldExecute = true) => {
  const assertion = shouldExecute ? 'have.class' : 'not.have.class'
  
  cy.get(`[data-testid="stage-${stageId}"]`)
    .should(assertion, 'executed')
})

Cypress.Commands.add('validateExecutionMetrics', (expectedMetrics = {}) => {
  const {
    minEfficiency = 80,
    maxDuration = 10000,
    minTimeSaved = 100
  } = expectedMetrics

  // Check efficiency
  cy.get('[data-testid="efficiency-score"]')
    .invoke('text')
    .then(text => {
      const efficiency = parseInt(text.replace(/\D/g, ''))
      expect(efficiency).to.be.greaterThan(minEfficiency)
    })

  // Check execution duration
  cy.get('[data-testid="execution-duration"]')
    .invoke('text')
    .then(text => {
      const duration = parseInt(text.replace(/\D/g, ''))
      expect(duration).to.be.lessThan(maxDuration)
    })

  // Check time saved
  cy.get('[data-testid="time-saved"]')
    .invoke('text')
    .then(text => {
      const timeSaved = parseInt(text.replace(/\D/g, ''))
      expect(timeSaved).to.be.greaterThan(minTimeSaved)
    })
})

// Permutation testing commands
Cypress.Commands.add('generatePermutations', (config = {}) => {
  if (config.domain) cy.selectDomain(config.domain)
  if (config.complexity) {
    cy.get('[data-testid="complexity-slider"]')
      .invoke('val', config.complexity)
      .trigger('input')
  }

  cy.get('[data-testid="generate-permutations-btn"]').click()
  
  cy.get('[data-testid="permutation-result"]')
    .should('have.length.greaterThan', 0)
})

Cypress.Commands.add('selectOptimalPermutation', () => {
  cy.get('[data-testid="optimal-permutation"]')
    .find('[data-testid="select-permutation-btn"]')
    .click()
  
  cy.get('[data-testid="selected-permutation"]')
    .should('be.visible')
})

Cypress.Commands.add('validatePermutationEfficiency', (permutationIndex = 0) => {
  cy.get('[data-testid="permutation-result"]')
    .eq(permutationIndex)
    .within(() => {
      cy.get('[data-testid="efficiency-score"]')
        .should('be.visible')
        .invoke('text')
        .then(text => {
          const efficiency = parseInt(text.replace(/\D/g, ''))
          expect(efficiency).to.be.greaterThan(0)
        })
      
      cy.get('[data-testid="execution-path"]')
        .should('be.visible')
      
      cy.get('[data-testid="estimated-duration"]')
        .should('be.visible')
    })
})

// Notification testing commands
Cypress.Commands.add('subscribeToChannels', (channels) => {
  cy.get('[data-testid="channel-subscription"]').click()
  
  channels.forEach(channel => {
    cy.get(`[data-testid="channel-${channel}"]`).check()
  })
  
  cy.get('[data-testid="subscribe-btn"]').click()
})

Cypress.Commands.add('waitForNotifications', (count = 1, timeout = 5000) => {
  cy.get('[data-testid="notification-item"]', { timeout })
    .should('have.length.greaterThanOrEqual', count)
})

Cypress.Commands.add('validateNotificationPriority', () => {
  cy.get('[data-testid="notification-item"]').then($notifications => {
    const notifications = Array.from($notifications).map(el => ({
      priority: el.dataset.priority,
      level: el.dataset.level,
      channel: el.dataset.channel
    }))
    
    // Critical notifications should appear first
    const criticalCount = notifications.filter(n => n.priority === 'critical').length
    const totalCount = notifications.length
    
    if (totalCount > 0) {
      expect(criticalCount / totalCount).to.be.greaterThan(0.3) // At least 30% critical
    }
  })
})

Cypress.Commands.add('clearNotifications', () => {
  cy.get('[data-testid="clear-notifications-btn"]')
    .should('be.visible')
    .click()
  
  cy.get('[data-testid="notification-item"]')
    .should('have.length', 0)
})

// WebSocket testing commands
Cypress.Commands.add('establishWebSocketConnection', () => {
  cy.window().then(win => {
    if (!win.WebSocket) {
      throw new Error('WebSocket not available')
    }
    
    // Check if connection indicator shows connected status
    cy.get('[data-testid="connection-status"]', { timeout: 10000 })
      .should('contain', 'connected')
  })
})

Cypress.Commands.add('simulateWebSocketMessage', (messageType, payload) => {
  cy.window().then(win => {
    const mockMessage = new MessageEvent('message', {
      data: JSON.stringify({
        type: messageType,
        payload: payload
      })
    })
    
    // Trigger message event if WebSocket exists
    if (win.WebSocket && win.WebSocket.prototype.onmessage) {
      win.WebSocket.prototype.onmessage(mockMessage)
    }
  })
})

// Swarm intelligence commands
Cypress.Commands.add('triggerSwarmOptimization', (options = {}) => {
  const {
    target = 'pipeline',
    strategy = '80_20',
    constraints = {}
  } = options

  cy.get('[data-testid="swarm-optimize-btn"]').click()
  
  cy.get('[data-testid="optimization-modal"]')
    .should('be.visible')
    .within(() => {
      cy.get('[data-testid="optimization-target"]').select(target)
      cy.get('[data-testid="optimization-strategy"]').select(strategy)
      
      if (constraints.maxDuration) {
        cy.get('[data-testid="max-duration-input"]')
          .clear()
          .type(constraints.maxDuration.toString())
      }
      
      cy.get('[data-testid="apply-optimization-btn"]').click()
    })
  
  cy.get('[data-testid="optimization-applied"]')
    .should('be.visible')
})

Cypress.Commands.add('validateSwarmLearning', (expectedPatterns = 1) => {
  cy.get('[data-testid="swarm-learn-btn"]').click()
  
  cy.get('[data-testid="learning-result"]')
    .should('be.visible')
    .within(() => {
      cy.get('[data-testid="patterns-learned"]')
        .invoke('text')
        .then(text => {
          const patterns = parseInt(text)
          expect(patterns).to.be.greaterThanOrEqual(expectedPatterns)
        })
      
      cy.get('[data-testid="confidence-score"]')
        .invoke('text')
        .then(text => {
          const confidence = parseFloat(text)
          expect(confidence).to.be.greaterThan(0.0)
          expect(confidence).to.be.lessThanOrEqual(1.0)
        })
    })
})

Cypress.Commands.add('getContextualRecommendations', (context = {}) => {
  if (context.complexity) {
    cy.get('[data-testid="complexity-slider"]')
      .invoke('val', context.complexity)
      .trigger('input')
  }
  
  if (context.timeConstraint) {
    cy.get('[data-testid="time-constraint-selector"]')
      .select(context.timeConstraint)
  }

  cy.get('[data-testid="get-recommendations-btn"]').click()
  
  cy.get('[data-testid="recommendations-list"]')
    .should('be.visible')
    .find('[data-testid="recommendation-item"]')
    .should('have.length.greaterThan', 0)
})

// Performance testing commands
Cypress.Commands.add('measureExecutionTime', (testFunction) => {
  const startTime = performance.now()
  
  cy.wrap(testFunction()).then(() => {
    const endTime = performance.now()
    const duration = endTime - startTime
    return cy.wrap(duration)
  })
})

Cypress.Commands.add('compareOptimizationPerformance', () => {
  let baselineDuration, optimizedDuration

  // Measure baseline (full pipeline)
  cy.executeOptimizedPipeline({ strategy: 'adaptive_routing' })
  cy.get('[data-testid="execution-duration"]')
    .invoke('text')
    .then(text => {
      baselineDuration = parseInt(text.replace(/\D/g, ''))
    })

  // Measure optimized (80/20)
  cy.executeOptimizedPipeline({ strategy: 'skip_non_critical' })
  cy.get('[data-testid="execution-duration"]')
    .invoke('text')
    .then(text => {
      optimizedDuration = parseInt(text.replace(/\D/g, ''))
      
      const improvement = (baselineDuration - optimizedDuration) / baselineDuration
      expect(improvement).to.be.greaterThan(0.15) // At least 15% improvement
      
      return cy.wrap({
        baseline: baselineDuration,
        optimized: optimizedDuration,
        improvement: Math.round(improvement * 100)
      })
    })
})

// Error handling commands
Cypress.Commands.add('handleExecutionErrors', () => {
  // Check for error notifications
  cy.get('body').then($body => {
    if ($body.find('[data-testid="error-notification"]').length > 0) {
      cy.get('[data-testid="error-notification"]')
        .should('be.visible')
        .find('[data-testid="dismiss-error-btn"]')
        .click()
    }
  })
})

Cypress.Commands.add('validateErrorRecovery', () => {
  // Ensure interface remains functional after error
  cy.get('[data-testid="execute-pipeline-btn"]')
    .should('not.be.disabled')
  
  cy.get('[data-testid="optimization-selector"]')
    .should('be.enabled')
})

// Debug and development commands
Cypress.Commands.add('enableDebugMode', () => {
  cy.get('[data-testid="debug-toggle"]').check()
  cy.get('[data-testid="debug-panel"]').should('be.visible')
})

Cypress.Commands.add('captureSwarmMetrics', () => {
  cy.get('[data-testid="swarm-metrics"]')
    .should('be.visible')
    .within(() => {
      cy.get('[data-testid="optimizations-applied"]')
        .invoke('text')
        .as('optimizationsApplied')
      
      cy.get('[data-testid="average-improvement"]')
        .invoke('text')
        .as('averageImprovement')
      
      cy.get('[data-testid="success-rate"]')
        .invoke('text')
        .as('successRate')
    })
})

// Accessibility commands
Cypress.Commands.add('validateKeyboardNavigation', () => {
  // Test tab navigation
  cy.get('body').tab()
  cy.focused().should('be.visible')
  
  // Test interactive elements
  cy.get('[data-testid="execute-pipeline-btn"]')
    .focus()
    .should('have.focus')
    .type('{enter}')
})

Cypress.Commands.add('validateAriaLabels', () => {
  // Check critical interactive elements have proper ARIA labels
  cy.get('[data-testid="execute-pipeline-btn"]')
    .should('have.attr', 'aria-label')
  
  cy.get('[data-testid="optimization-selector"]')
    .should('have.attr', 'aria-label')
  
  cy.get('[data-testid="domain-selector"]')
    .should('have.attr', 'aria-label')
})

// Data validation commands
Cypress.Commands.add('validateDataIntegrity', () => {
  // Verify pipeline stages are correctly loaded
  cy.get('[data-testid="pipeline-stage"]')
    .should('have.length', 8)
  
  // Verify critical stages are marked
  cy.get('[data-testid="pipeline-stage"].critical')
    .should('have.length.greaterThan', 4)
  
  // Verify optimization strategies are available
  cy.get('[data-testid="optimization-selector"] option')
    .should('have.length.greaterThan', 3)
})

Cypress.Commands.add('validateOutputGeneration', () => {
  cy.get('[data-testid="pipeline-outputs"]')
    .should('be.visible')
    .within(() => {
      cy.get('[data-testid="output-item"]')
        .should('have.length.greaterThan', 0)
      
      // Verify essential outputs are present
      cy.get('[data-testid="output-item"]')
        .should('contain', 'processed_data')
    })
})