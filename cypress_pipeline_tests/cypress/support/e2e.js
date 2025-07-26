// Import commands
import './commands'

// Global configuration
Cypress.on('uncaught:exception', (err, runnable) => {
  // Prevent Cypress from failing on uncaught exceptions
  return false
})

// Custom assertions
chai.use((chai, utils) => {
  chai.Assertion.addMethod('pipelineStatus', function (expected) {
    const obj = this._obj
    this.assert(
      obj.status === expected,
      `expected pipeline status to be ${expected} but got ${obj.status}`,
      `expected pipeline status not to be ${expected}`,
      expected,
      obj.status
    )
  })
})

// Global before hook
beforeEach(() => {
  // Setup performance marks
  cy.window().then((win) => {
    win.performance.mark('test-start')
  })
})

afterEach(() => {
  // Collect performance metrics
  cy.window().then((win) => {
    win.performance.mark('test-end')
    win.performance.measure('test-duration', 'test-start', 'test-end')
  })
})