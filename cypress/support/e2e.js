// Cypress E2E Support Commands for Swarm Pipeline Testing

import './commands'

// Global test configuration
Cypress.on('uncaught:exception', (err, runnable) => {
  // Don't fail tests on uncaught exceptions from app
  if (err.message.includes('ResizeObserver loop limit exceeded')) {
    return false
  }
  return true
})

// Custom commands for Swarm Pipeline testing
Cypress.Commands.add('connectToSwarmChannel', () => {
  cy.window().then((win) => {
    // Connect to Phoenix WebSocket channel
    const socket = new win.Phoenix.Socket(Cypress.env('phoenixUrl'))
    socket.connect()
    
    const channel = socket.channel('swarm:pipeline', {})
    
    return new Promise((resolve) => {
      channel.join()
        .receive('ok', () => {
          win.swarmChannel = channel
          win.swarmSocket = socket
          resolve(channel)
        })
        .receive('error', (error) => {
          throw new Error(`Failed to connect to swarm channel: ${error}`)
        })
    })
  })
})

Cypress.Commands.add('disconnectSwarmChannel', () => {
  cy.window().then((win) => {
    if (win.swarmChannel) {
      win.swarmChannel.leave()
    }
    if (win.swarmSocket) {
      win.swarmSocket.disconnect()
    }
  })
})

Cypress.Commands.add('executeSwarmStrategy', (strategy, input) => {
  cy.window().then((win) => {
    if (!win.swarmChannel) {
      throw new Error('Swarm channel not connected')
    }
    
    return new Promise((resolve, reject) => {
      win.swarmChannel.push('execute_pipeline', {
        data: {
          strategy: strategy,
          domain: input.domain || 'cybersecurity',
          entities: input.entities || ['test_entity'],
          complexity: input.complexity || 0.5,
          priority: input.priority || 'medium'
        }
      })
      .receive('ok', resolve)
      .receive('error', reject)
      .receive('timeout', () => reject(new Error('Strategy execution timeout')))
    })
  })
})

Cypress.Commands.add('waitForTelemetryUpdate', (timeout = 5000) => {
  cy.window().then((win) => {
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => reject(new Error('Telemetry timeout')), timeout)
      
      if (win.swarmChannel) {
        win.swarmChannel.on('telemetry_update', (data) => {
          clearTimeout(timer)
          resolve(data)
        })
      } else {
        reject(new Error('No swarm channel available'))
      }
    })
  })
})

Cypress.Commands.add('verifyAshReactorExecution', (expectedStages) => {
  cy.request({
    method: 'GET',
    url: `${Cypress.env('apiUrl')}/reactor/status`,
    failOnStatusCode: false
  }).then((response) => {
    expect(response.status).to.eq(200)
    expect(response.body).to.have.property('reactor_running', true)
    
    if (expectedStages) {
      expect(response.body.executed_stages).to.include.members(expectedStages)
    }
  })
})

Cypress.Commands.add('resetSwarmState', () => {
  cy.request({
    method: 'POST',
    url: `${Cypress.env('apiUrl')}/swarm/reset`,
    failOnStatusCode: false
  })
})

// Component testing helpers
Cypress.Commands.add('mountSwarmComponent', (component, props = {}) => {
  cy.mount(component, {
    props: {
      ...props,
      mockWebSocket: true // Use mock WebSocket for component tests
    }
  })
})

Cypress.Commands.add('triggerSwarmVisualization', (strategy) => {
  cy.get('[data-cy="strategy-selector"]').select(strategy)
  cy.get('[data-cy="execute-pipeline"]').click()
  cy.get('[data-cy="pipeline-animation"]').should('be.visible')
})

Cypress.Commands.add('verifyOptimizationMetrics', (expectedMetrics) => {
  cy.get('[data-cy="optimization-metrics"]').within(() => {
    if (expectedMetrics.stageReduction) {
      cy.get('[data-cy="stage-reduction"]').should('contain', expectedMetrics.stageReduction)
    }
    if (expectedMetrics.efficiency) {
      cy.get('[data-cy="efficiency-score"]').should('contain', expectedMetrics.efficiency)
    }
    if (expectedMetrics.executionTime) {
      cy.get('[data-cy="execution-time"]').should('contain', expectedMetrics.executionTime)
    }
  })
})

// Before each test setup
beforeEach(() => {
  // Set up test environment
  cy.resetSwarmState()
  cy.visit('/')
})

// After each test cleanup
afterEach(() => {
  // Clean up connections
  cy.disconnectSwarmChannel()
})