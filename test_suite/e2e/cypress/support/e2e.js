// Import commands
import './commands'

// Global configuration
beforeEach(() => {
  // Set up test environment
  cy.window().then((win) => {
    // Mock Phoenix Socket if not available
    if (!win.Phoenix) {
      win.Phoenix = {
        Socket: class MockSocket {
          constructor(url, options) {
            this.url = url
            this.options = options
            this.channels = new Map()
            this.connected = false
          }
          
          connect() {
            this.connected = true
            return this
          }
          
          disconnect() {
            this.connected = false
            return this
          }
          
          channel(topic, params) {
            const channel = new MockChannel(topic, params)
            this.channels.set(topic, channel)
            return channel
          }
        }
      }
      
      class MockChannel {
        constructor(topic, params) {
          this.topic = topic
          this.params = params
          this.callbacks = new Map()
          this.joined = false
        }
        
        join() {
          this.joined = true
          setTimeout(() => {
            const callback = this.callbacks.get('ok')
            if (callback) callback({ status: 'joined' })
          }, 100)
          return this
        }
        
        push(event, payload) {
          const response = {
            receive: (status, callback) => {
              setTimeout(() => {
                if (status === 'ok') {
                  callback({ success: true, ...payload })
                }
              }, 50)
              return response
            }
          }
          return response
        }
        
        on(event, callback) {
          this.callbacks.set(event, callback)
        }
        
        receive(status, callback) {
          this.callbacks.set(status, callback)
          return this
        }
      }
    }
  })
})

// Global error handling
Cypress.on('uncaught:exception', (err, runnable) => {
  // Don't fail tests on unhandled promise rejections in WebSocket connections
  if (err.message.includes('WebSocket') || err.message.includes('connection')) {
    return false
  }
  
  // Don't fail on React hydration issues during testing
  if (err.message.includes('hydration') || err.message.includes('Hydration')) {
    return false
  }
  
  return true
})

// Performance measurement utilities
Cypress.Commands.add('measurePerformance', (testFn) => {
  const startTime = performance.now()
  
  cy.wrap(testFn()).then(() => {
    const endTime = performance.now()
    const duration = endTime - startTime
    
    return cy.wrap(duration)
  })
})

// 80/20 optimization validation
Cypress.Commands.add('validate80_20Optimization', (selector) => {
  cy.get(selector).then($elements => {
    const totalStages = $elements.length
    const criticalStages = $elements.filter('.critical').length
    const criticalRatio = criticalStages / totalStages
    
    cy.task('validate80_20Optimization', {
      stages: Array.from($elements).map(el => el.dataset.stage),
      criticalStages: Array.from($elements.filter('.critical')).map(el => el.dataset.stage)
    }).then(result => {
      expect(result.meets_pareto_principle).to.be.true
      return cy.wrap(result)
    })
  })
})

// WebSocket connection helpers
Cypress.Commands.add('connectWebSocket', (url, options = {}) => {
  cy.window().then((win) => {
    if (win.WebSocket) {
      const ws = new win.WebSocket(url)
      const connectionPromise = new Promise((resolve, reject) => {
        ws.onopen = () => resolve(ws)
        ws.onerror = reject
        setTimeout(() => reject(new Error('WebSocket connection timeout')), 5000)
      })
      
      return cy.wrap(connectionPromise)
    } else {
      return cy.wrap(null)
    }
  })
})

// Phoenix channel helpers
Cypress.Commands.add('joinChannel', (socket, topic, params = {}) => {
  cy.wrap(socket).then(socketInstance => {
    const channel = socketInstance.channel(topic, params)
    
    const joinPromise = new Promise((resolve, reject) => {
      channel.join()
        .receive('ok', resolve)
        .receive('error', reject)
      
      setTimeout(() => reject(new Error('Channel join timeout')), 5000)
    })
    
    return cy.wrap({ channel, joinPromise })
  })
})

// Swarm metrics validation
Cypress.Commands.add('validateSwarmMetrics', (metricsSelector) => {
  cy.get(metricsSelector).then($metrics => {
    const metricsData = {
      patterns_learned: parseInt($metrics.find('[data-testid="patterns-learned"]').text() || '0'),
      confidence_score: parseFloat($metrics.find('[data-testid="confidence-score"]').text() || '0'),
      recommendations_applied: parseInt($metrics.find('[data-testid="recommendations-applied"]').text() || '0')
    }
    
    cy.task('validateSwarmMetrics', metricsData).then(result => {
      expect(result.learning_effectiveness).to.be.true
      expect(result.confidence_acceptable).to.be.true
      return cy.wrap(result)
    })
  })
})

// Performance comparison utilities
Cypress.Commands.add('comparePerformance', (baselineSelector, optimizedSelector) => {
  let baselineDuration, optimizedDuration
  
  cy.get(baselineSelector).invoke('text').then(text => {
    baselineDuration = parseInt(text.replace(/\D/g, ''))
  })
  
  cy.get(optimizedSelector).invoke('text').then(text => {
    optimizedDuration = parseInt(text.replace(/\D/g, ''))
    
    cy.task('measurePerformance', {
      baseline: baselineDuration,
      optimized: optimizedDuration
    }).then(result => {
      expect(result.meets_80_20_criteria).to.be.true
      return cy.wrap(result)
    })
  })
})

// Notification testing utilities
Cypress.Commands.add('waitForNotification', (type, timeout = 5000) => {
  cy.get(`[data-testid="notification-item"][data-type="${type}"]`, { timeout })
    .should('be.visible')
})

Cypress.Commands.add('validateNotificationFiltering', () => {
  cy.get('[data-testid="notification-item"]').then($notifications => {
    const critical = $notifications.filter('[data-priority="critical"]').length
    const total = $notifications.length
    
    // In 80/20 mode, critical notifications should dominate
    expect(critical / total).to.be.greaterThan(0.6) // At least 60% critical
  })
})

// Pipeline execution helpers
Cypress.Commands.add('executePipeline', (strategy = 'skip_non_critical') => {
  cy.get('[data-testid="optimization-selector"]').select(strategy)
  cy.get('[data-testid="execute-pipeline-btn"]').click()
  
  // Wait for execution to complete
  cy.get('[data-testid="execution-status"]', { timeout: 15000 })
    .should('contain', 'completed')
})

Cypress.Commands.add('validatePipelineOptimization', () => {
  cy.get('[data-testid="optimization-metrics"]').should('be.visible')
  cy.get('[data-testid="time-saved"]').should('contain', 'ms')
  cy.get('[data-testid="efficiency-gain"]').invoke('text').then(text => {
    const efficiency = parseInt(text.replace(/\D/g, ''))
    expect(efficiency).to.be.greaterThan(20) // At least 20% improvement
  })
})

// Error injection for testing resilience
Cypress.Commands.add('injectError', (errorType) => {
  cy.window().then(win => {
    switch (errorType) {
      case 'websocket_disconnect':
        if (win.WebSocket) {
          win.WebSocket.prototype.close.call()
        }
        break
      case 'network_error':
        cy.intercept('**/api/**', { forceNetworkError: true })
        break
      case 'timeout':
        cy.intercept('**/api/**', { delay: 10000 })
        break
      default:
        throw new Error(`Unknown error type: ${errorType}`)
    }
  })
})

// Accessibility testing
Cypress.Commands.add('checkA11y', (selector = null) => {
  const target = selector || 'body'
  
  cy.get(target).should($el => {
    // Check for ARIA labels on interactive elements
    $el.find('button, input, select').each((_, element) => {
      const $element = Cypress.$(element)
      const hasAriaLabel = $element.attr('aria-label') || $element.attr('aria-labelledby')
      expect(hasAriaLabel).to.exist
    })
  })
})

// Data attribute utilities
Cypress.Commands.add('getByTestId', (testId) => {
  return cy.get(`[data-testid="${testId}"]`)
})

// Custom assertions for 80/20 optimization
Cypress.Commands.add('shouldMeet80_20Criteria', { prevSubject: true }, (subject) => {
  cy.wrap(subject).should($element => {
    const efficiency = $element.data('efficiency') || parseInt($element.text().replace(/\D/g, ''))
    expect(efficiency).to.be.greaterThan(80)
  })
})

// Test data management
Cypress.Commands.add('generateTestData', (type) => {
  const testData = {
    pipeline: {
      stages: ['typer', 'turtle', 'ttl2dspy', 'bitactor', 'erlang', 'ash', 'reactor', 'k8s'],
      critical_stages: ['typer', 'turtle', 'ash', 'reactor', 'k8s'],
      domain: 'cybersecurity',
      optimization_strategy: 'skip_non_critical'
    },
    notifications: {
      critical: { level: 'critical', channel: 'error_alerts', priority: 'high' },
      normal: { level: 'info', channel: 'pipeline_events', priority: 'medium' },
      low: { level: 'debug', channel: 'debug_logs', priority: 'low' }
    },
    workflow: {
      name: 'test_workflow',
      steps: [
        { name: 'parse_input', type: 'ash_action', critical: true },
        { name: 'generate_resources', type: 'ash_create', critical: true },
        { name: 'validate_output', type: 'ash_validate', critical: false }
      ]
    }
  }
  
  return cy.wrap(testData[type] || testData)
})