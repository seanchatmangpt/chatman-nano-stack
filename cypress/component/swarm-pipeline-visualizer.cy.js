// SwarmPipelineVisualizer Component Tests

import SwarmPipelineVisualizer from '../../nuxt_ui/components/SwarmPipelineVisualizer.vue'

describe('SwarmPipelineVisualizer Component', () => {
  beforeEach(() => {
    cy.mountSwarmComponent(SwarmPipelineVisualizer, {
      strategies: Cypress.env('mockData').strategies(),
      connected: true
    })
  })

  describe('🎨 Component Rendering', () => {
    it('should render the main visualizer interface', () => {
      cy.getByDataCy('swarm-pipeline-visualizer')
        .should('be.visible')

      cy.contains('🚀 Swarm Pipeline Visualizer')
        .should('be.visible')

      cy.testComponentPerformance('swarm-pipeline-visualizer')
    })

    it('should render strategy selection controls', () => {
      cy.getByDataCy('strategy-selection')
        .should('be.visible')

      cy.getByDataCy('strategy-card')
        .should('have.length.greaterThan', 2)

      cy.getByDataCy('strategy-card')
        .first()
        .should('contain', 'Linear')
        .and('be.visible')
    })

    it('should render pipeline flow visualization', () => {
      cy.getByDataCy('pipeline-flow')
        .should('be.visible')

      cy.getByDataCy('pipeline-stage')
        .should('have.length', 8) // All 8 stages initially

      // Verify stage icons are rendered
      cy.getByDataCy('stage-typer').should('contain', '🎯')
      cy.getByDataCy('stage-turtle').should('contain', '🐢')
      cy.getByDataCy('stage-ash').should('contain', '🔥')
      cy.getByDataCy('stage-k8s').should('contain', '☸️')
    })
  })

  describe('🎮 Interactive Controls', () => {
    it('should allow strategy selection', () => {
      cy.getByDataCy('strategy-card')
        .first()
        .click()

      cy.getByDataCy('strategy-card')
        .first()
        .should('have.class', 'selected')

      cy.getByDataCy('selected-strategy')
        .should('contain', 'Linear')
    })

    it('should enable execution controls when strategy is selected', () => {
      cy.getByDataCy('execute-pipeline')
        .should('be.disabled')

      cy.getByDataCy('strategy-card')
        .contains('Skip Optimization')
        .click()

      cy.getByDataCy('execute-pipeline')
        .should('not.be.disabled')
        .and('contain', 'Execute Pipeline')
    })

    it('should handle complexity adjustment', () => {
      cy.getByDataCy('complexity-slider')
        .should('be.visible')
        .invoke('val', 0.8)
        .trigger('input')

      cy.getByDataCy('complexity-value')
        .should('contain', '0.8')

      cy.getByDataCy('complexity-indicator')
        .should('have.class', 'high-complexity')
    })

    it('should allow domain selection', () => {
      cy.getByDataCy('domain-selector')
        .select('cybersecurity')

      cy.getByDataCy('domain-selector')
        .should('have.value', 'cybersecurity')

      cy.getByDataCy('domain-icon')
        .should('contain', '🛡️')
    })
  })

  describe('⚡ Pipeline Execution Animation', () => {
    beforeEach(() => {
      cy.getByDataCy('strategy-card')
        .contains('Skip Optimization')
        .click()
    })

    it('should animate pipeline execution', () => {
      cy.getByDataCy('execute-pipeline').click()

      cy.getByDataCy('pipeline-animation')
        .should('be.visible')
        .and('have.class', 'executing')

      cy.verifyAnimation('[data-cy="stage-progress"]', 2000)

      cy.getByDataCy('execution-status')
        .should('contain', 'completed')
    })

    it('should show stage progression indicators', () => {
      cy.getByDataCy('execute-pipeline').click()

      // Verify stages light up in sequence
      cy.getByDataCy('stage-typer')
        .should('have.class', 'active')

      cy.wait(500)

      cy.getByDataCy('stage-turtle')
        .should('have.class', 'active')

      cy.getByDataCy('stage-typer')
        .should('have.class', 'completed')
    })

    it('should display execution metrics during animation', () => {
      cy.getByDataCy('execute-pipeline').click()

      cy.getByDataCy('execution-metrics')
        .should('be.visible')

      cy.getByDataCy('execution-time')
        .should('be.visible')
        .and('contain', 'ms')

      cy.getByDataCy('stages-completed')
        .should('be.visible')
    })

    it('should show 80/20 optimization in action', () => {
      cy.getByDataCy('execute-pipeline').click()

      cy.getByDataCy('optimization-indicator')
        .should('be.visible')
        .and('contain', '80/20')

      cy.getByDataCy('skipped-stages')
        .should('contain', '4 stages skipped')

      cy.getByDataCy('efficiency-gain')
        .should('contain', '50% faster')
    })
  })

  describe('📊 Real-time Telemetry Display', () => {
    it('should display telemetry event log', () => {
      cy.getByDataCy('telemetry-log')
        .should('be.visible')

      // Simulate telemetry events
      cy.window().then((win) => {
        win.postMessage({
          type: 'telemetry_update',
          data: {
            stage: 'typer',
            duration: 45,
            correlation_id: 'test-123'
          }
        }, '*')
      })

      cy.getByDataCy('telemetry-event')
        .should('be.visible')
        .and('contain', 'typer')
        .and('contain', '45ms')
    })

    it('should update connection status indicator', () => {
      cy.getByDataCy('connection-status')
        .should('be.visible')
        .and('have.class', 'connected')
        .and('contain', 'Connected')

      // Simulate disconnection
      cy.window().then((win) => {
        win.postMessage({
          type: 'websocket_disconnect'
        }, '*')
      })

      cy.getByDataCy('connection-status')
        .should('have.class', 'disconnected')
        .and('contain', 'Disconnected')
    })

    it('should display swarm intelligence insights', () => {
      cy.getByDataCy('strategy-card')
        .contains('Emergence Guided')
        .click()

      cy.getByDataCy('execute-pipeline').click()

      cy.getByDataCy('swarm-insights')
        .should('be.visible')

      cy.getByDataCy('emergence-factor')
        .should('be.visible')
        .and('contain', '0.8')

      cy.getByDataCy('pattern-detection')
        .should('contain', 'convergent')
    })
  })

  describe('🔧 Error Handling', () => {
    it('should handle execution errors gracefully', () => {
      cy.getByDataCy('strategy-card')
        .first()
        .click()

      // Simulate execution error
      cy.window().then((win) => {
        win.postMessage({
          type: 'execution_error',
          error: 'Reactor execution failed'
        }, '*')
      })

      cy.getByDataCy('error-message')
        .should('be.visible')
        .and('contain', 'execution failed')

      cy.getByDataCy('retry-button')
        .should('be.visible')
        .click()

      cy.getByDataCy('error-message')
        .should('not.exist')
    })

    it('should validate input parameters', () => {
      cy.getByDataCy('complexity-slider')
        .invoke('val', 1.5) // Invalid value > 1
        .trigger('input')

      cy.getByDataCy('validation-error')
        .should('be.visible')
        .and('contain', 'must be between 0 and 1')

      cy.getByDataCy('execute-pipeline')
        .should('be.disabled')
    })
  })

  describe('📱 Responsive Design', () => {
    it('should adapt to mobile viewport', () => {
      cy.viewport(375, 667) // iPhone dimensions

      cy.getByDataCy('swarm-pipeline-visualizer')
        .should('be.visible')

      cy.getByDataCy('strategy-cards')
        .should('have.css', 'flex-direction', 'column')

      cy.getByDataCy('pipeline-flow')
        .should('have.css', 'overflow-x', 'auto')
    })

    it('should work on tablet viewport', () => {
      cy.viewport(768, 1024) // iPad dimensions

      cy.getByDataCy('strategy-cards')
        .should('have.css', 'grid-template-columns')

      cy.getByDataCy('pipeline-flow')
        .should('be.visible')
        .and('not.have.css', 'overflow-x', 'auto')
    })
  })

  describe('🎭 Accessibility', () => {
    it('should have proper ARIA labels', () => {
      cy.getByDataCy('strategy-card')
        .first()
        .should('have.attr', 'role', 'button')
        .and('have.attr', 'aria-label')

      cy.getByDataCy('execute-pipeline')
        .should('have.attr', 'aria-label', 'Execute selected pipeline strategy')
    })

    it('should be keyboard navigable', () => {
      cy.getByDataCy('strategy-card')
        .first()
        .focus()
        .type('{enter}')

      cy.getByDataCy('strategy-card')
        .first()
        .should('have.class', 'selected')

      cy.getByDataCy('execute-pipeline')
        .focus()
        .type('{enter}')

      cy.getByDataCy('pipeline-animation')
        .should('be.visible')
    })

    it('should announce status changes to screen readers', () => {
      cy.getByDataCy('sr-status')
        .should('have.attr', 'aria-live', 'polite')

      cy.getByDataCy('strategy-card')
        .first()
        .click()

      cy.getByDataCy('sr-status')
        .should('contain', 'Linear strategy selected')
    })
  })
})