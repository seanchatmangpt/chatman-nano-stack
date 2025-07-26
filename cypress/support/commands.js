// Custom Cypress commands for Swarm Pipeline testing

// Add common selectors
const SELECTORS = {
  // Pipeline Visualizer
  pipelineVisualizer: '[data-cy="swarm-pipeline-visualizer"]',
  strategySelector: '[data-cy="strategy-selector"]',
  executePipeline: '[data-cy="execute-pipeline"]',
  pipelineAnimation: '[data-cy="pipeline-animation"]',
  
  // Optimization Comparator
  optimizationComparator: '[data-cy="swarm-optimization-comparator"]',
  strategyCard: '[data-cy="strategy-card"]',
  runComparison: '[data-cy="run-comparison"]',
  comparisonResults: '[data-cy="comparison-results"]',
  
  // Telemetry Dashboard
  telemetryDashboard: '[data-cy="swarm-telemetry-dashboard"]',
  metricsChart: '[data-cy="metrics-chart"]',
  emergencePattern: '[data-cy="emergence-pattern"]',
  
  // Flow Editor
  flowEditor: '[data-cy="pipeline-flow-editor"]',
  flowCanvas: '[data-cy="flow-canvas"]',
  stageNode: '[data-cy="stage-node"]',
  
  // 3D Explorer
  explorer3D: '[data-cy="permutation-explorer-3d"]',
  canvas3D: '[data-cy="canvas-3d"]',
  viewMode: '[data-cy="view-mode"]',
  
  // Parallel Visualizer
  parallelVisualizer: '[data-cy="parallel-execution-visualizer"]',
  branch: '[data-cy="execution-branch"]',
  timeline: '[data-cy="execution-timeline"]',
  
  // Permutation Matrix
  permutationMatrix: '[data-cy="permutation-matrix"]',
  matrixGrid: '[data-cy="matrix-grid"]',
  complexitySlider: '[data-cy="complexity-slider"]'
}

// Store selectors globally
Cypress.Commands.add('getByDataCy', (selector) => {
  return cy.get(`[data-cy="${selector}"]`)
})

// WebSocket connection utilities
Cypress.Commands.add('waitForWebSocketConnection', (timeout = 10000) => {
  return cy.window({ timeout }).should((win) => {
    expect(win.swarmSocket).to.exist
    expect(win.swarmSocket.connectionState()).to.eq('open')
  })
})

Cypress.Commands.add('sendWebSocketMessage', (event, payload) => {
  cy.window().then((win) => {
    if (win.swarmChannel && win.swarmChannel.state === 'joined') {
      win.swarmChannel.push(event, payload)
    }
  })
})

// Component interaction helpers
Cypress.Commands.add('selectStrategy', (strategy) => {
  cy.getByDataCy('strategy-selector').select(strategy)
})

Cypress.Commands.add('executeStrategy', (strategy, options = {}) => {
  cy.selectStrategy(strategy)
  cy.getByDataCy('execute-pipeline').click()
  
  if (options.waitForCompletion) {
    cy.getByDataCy('execution-status').should('contain', 'completed')
  }
})

Cypress.Commands.add('verifyPipelineStages', (expectedStages) => {
  expectedStages.forEach((stage, index) => {
    cy.getByDataCy(`stage-${stage}`)
      .should('be.visible')
      .and('have.attr', 'data-stage-order', index.toString())
  })
})

Cypress.Commands.add('dragStageToPosition', (stageName, x, y) => {
  cy.getByDataCy(`stage-${stageName}`)
    .trigger('mousedown', { which: 1 })
    .trigger('dragstart')
    .trigger('drag', { clientX: x, clientY: y })
    .trigger('drop')
    .trigger('dragend')
})

// Performance testing helpers
Cypress.Commands.add('measureRenderTime', (componentSelector) => {
  cy.window().then((win) => {
    const startTime = win.performance.now()
    
    cy.get(componentSelector).should('be.visible').then(() => {
      const endTime = win.performance.now()
      const renderTime = endTime - startTime
      
      // Assert render time is under 100ms
      expect(renderTime).to.be.lessThan(100)
      
      // Log performance metric
      cy.log(`Render time: ${renderTime.toFixed(2)}ms`)
    })
  })
})

Cypress.Commands.add('measureMemoryUsage', () => {
  cy.window().then((win) => {
    if (win.performance && win.performance.memory) {
      const memory = win.performance.memory
      const usedMB = memory.usedJSHeapSize / 1024 / 1024
      
      // Assert memory usage is under 50MB
      expect(usedMB).to.be.lessThan(50)
      
      cy.log(`Memory usage: ${usedMB.toFixed(2)}MB`)
    }
  })
})

// Animation testing helpers
Cypress.Commands.add('verifyAnimation', (selector, duration = 3000) => {
  cy.get(selector)
    .should('have.class', 'animating')
    .wait(duration)
    .should('not.have.class', 'animating')
})

Cypress.Commands.add('verify3DRendering', () => {
  cy.getByDataCy('canvas-3d').should('be.visible').then(($canvas) => {
    const canvas = $canvas[0]
    const context = canvas.getContext('webgl') || canvas.getContext('experimental-webgl')
    expect(context).to.not.be.null
    
    // Verify WebGL is working
    expect(context.getParameter(context.VERSION)).to.exist
  })
})

// Validation helpers
Cypress.Commands.add('verifyOptimizationResults', (strategy, expectedResults) => {
  cy.getByDataCy('optimization-results').within(() => {
    if (expectedResults.stageCount) {
      cy.contains(`${expectedResults.stageCount} stages`)
    }
    if (expectedResults.efficiency) {
      cy.contains(`${expectedResults.efficiency}% efficient`)
    }
    if (expectedResults.executionTime) {
      cy.contains(`${expectedResults.executionTime}ms`)
    }
  })
})

Cypress.Commands.add('verifyTelemetryData', (expectedData) => {
  cy.getByDataCy('telemetry-data').should('contain.text', expectedData.stage)
  if (expectedData.duration) {
    cy.getByDataCy('stage-duration').should('contain', expectedData.duration)
  }
  if (expectedData.correlationId) {
    cy.getByDataCy('correlation-id').should('contain', expectedData.correlationId)
  }
})

// Error handling
Cypress.Commands.add('handleAsyncError', (callback) => {
  cy.window().then((win) => {
    win.addEventListener('error', (e) => {
      if (callback) callback(e)
      throw e
    })
    
    win.addEventListener('unhandledrejection', (e) => {
      if (callback) callback(e)
      throw e.reason
    })
  })
})

// Store selectors for reuse
Cypress.env('selectors', SELECTORS)