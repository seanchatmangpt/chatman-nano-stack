// Pipeline testing commands
Cypress.Commands.add('connectToPipeline', (endpoint) => {
  cy.request('GET', `${Cypress.env('PIPELINE_API_URL')}/${endpoint}/connect`)
    .its('status')
    .should('eq', 200)
})

Cypress.Commands.add('executePipelineStage', (stage, data = {}) => {
  cy.request('POST', `${Cypress.env('PIPELINE_API_URL')}/${stage}/execute`, data)
    .its('body')
    .should('have.property', 'success', true)
})

Cypress.Commands.add('waitForPipelineCompletion', (executionId) => {
  cy.request('GET', `${Cypress.env('PIPELINE_API_URL')}/execution/${executionId}/status`)
    .its('body.status')
    .should('eq', 'completed')
})

// Reactor workflow commands
Cypress.Commands.add('createReactorWorkflow', (workflowData) => {
  cy.request('POST', `${Cypress.env('REACTOR_API_URL')}/workflows`, workflowData)
    .its('body')
    .should('have.property', 'id')
})

Cypress.Commands.add('deployWorkflow', (workflowId) => {
  cy.request('POST', `${Cypress.env('REACTOR_API_URL')}/workflows/${workflowId}/deploy`)
    .its('status')
    .should('eq', 200)
})

// Ash resource commands
Cypress.Commands.add('createAshResource', (resourceData) => {
  cy.request('POST', `${Cypress.env('ASH_API_URL')}/resources`, resourceData)
    .its('body')
    .should('have.property', 'id')
})

Cypress.Commands.add('queryAshResource', (resourceType, query = {}) => {
  cy.request('GET', `${Cypress.env('ASH_API_URL')}/resources/${resourceType}`, { qs: query })
    .its('status')
    .should('eq', 200)
})

// Channel communication commands
Cypress.Commands.add('subscribeToChannel', (channelName) => {
  cy.window().then((win) => {
    if (win.phoenixSocket) {
      const channel = win.phoenixSocket.channel(channelName)
      channel.join()
      return cy.wrap(channel).as('activeChannel')
    }
  })
})

Cypress.Commands.add('broadcastToChannel', (message) => {
  cy.get('@activeChannel').then((channel) => {
    channel.push('broadcast', message)
  })
})

// Performance testing commands
Cypress.Commands.add('measurePerformance', (operation, callback) => {
  cy.window().then((win) => {
    win.performance.mark(`${operation}-start`)
    callback()
    win.performance.mark(`${operation}-end`)
    win.performance.measure(operation, `${operation}-start`, `${operation}-end`)
  })
})

Cypress.Commands.add('assertPerformance', (operation, maxDuration) => {
  cy.window().then((win) => {
    const measure = win.performance.getEntriesByName(operation)[0]
    expect(measure.duration).to.be.lessThan(maxDuration)
  })
})

// UI interaction helpers
Cypress.Commands.add('dragAndDrop', (sourceSelector, targetSelector) => {
  cy.get(sourceSelector).trigger('dragstart')
  cy.get(targetSelector).trigger('drop')
})

Cypress.Commands.add('waitForRealTimeUpdate', (selector, timeout = 5000) => {
  cy.get(selector, { timeout }).should('not.have.attr', 'data-stale')
})