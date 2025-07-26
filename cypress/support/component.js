// Component testing support for Nuxt UI Swarm components

import { mount } from 'cypress/vue'
import './commands'

// Mock Phoenix WebSocket for component tests
const mockPhoenixSocket = {
  connect: cy.stub(),
  disconnect: cy.stub(),
  channel: cy.stub().returns({
    join: cy.stub().returns({
      receive: cy.stub().returns({ receive: cy.stub() })
    }),
    leave: cy.stub(),
    push: cy.stub(),
    on: cy.stub(),
    off: cy.stub()
  })
}

// Mock WebSocket globally for component tests
beforeEach(() => {
  cy.window().then((win) => {
    win.Phoenix = { Socket: () => mockPhoenixSocket }
  })
})

// Component mounting with common props
Cypress.Commands.add('mountSwarmComponent', (component, options = {}) => {
  const defaultProps = {
    mockMode: true,
    websocketUrl: 'ws://localhost:4000/socket',
    ...options.props
  }
  
  return mount(component, {
    props: defaultProps,
    ...options
  })
})

// Component-specific testing utilities
Cypress.Commands.add('testComponentPerformance', (componentName) => {
  cy.measureRenderTime(`[data-cy="${componentName}"]`)
  cy.measureMemoryUsage()
})

Cypress.Commands.add('testComponentInteractivity', (interactions) => {
  interactions.forEach(interaction => {
    cy.getByDataCy(interaction.element)
      .should('be.visible')
      .and('not.be.disabled')
    
    if (interaction.click) {
      cy.getByDataCy(interaction.element).click()
    }
    
    if (interaction.type) {
      cy.getByDataCy(interaction.element).type(interaction.type)
    }
    
    if (interaction.select) {
      cy.getByDataCy(interaction.element).select(interaction.select)
    }
    
    if (interaction.expectedResult) {
      cy.getByDataCy(interaction.expectedResult.element)
        .should(interaction.expectedResult.assertion, interaction.expectedResult.value)
    }
  })
})

Cypress.Commands.add('testComponentAnimation', (animationSelector, duration = 1000) => {
  cy.get(animationSelector)
    .should('be.visible')
    .and('have.css', 'transition-duration')
    .wait(duration)
})

// Mock data generators for testing
const generateMockTelemetryData = () => ({
  stage: 'typer',
  duration: 50,
  ttl_remaining: 30000,
  correlation_id: 'test-correlation-123',
  timestamp: new Date().toISOString()
})

const generateMockStrategyData = () => ([
  { id: 'linear', name: 'Linear Sequential', efficiency: 0.6 },
  { id: 'skip_optimization', name: 'Skip Optimization', efficiency: 0.9 },
  { id: 'parallel_merge', name: 'Parallel Merge', efficiency: 0.8 }
])

const generateMockPermutationData = () => ([
  {
    id: 'perm-1',
    strategy: 'Linear',
    path: ['typer', 'turtle', 'ash', 'k8s'],
    efficiency: 0.7,
    stage_count: 4
  },
  {
    id: 'perm-2', 
    strategy: 'Skip Optimization',
    path: ['typer', 'ash', 'k8s'],
    efficiency: 0.9,
    stage_count: 3
  }
])

// Export mock data for use in tests
Cypress.env('mockData', {
  telemetry: generateMockTelemetryData,
  strategies: generateMockStrategyData,
  permutations: generateMockPermutationData
})