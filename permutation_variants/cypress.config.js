const { defineConfig } = require('cypress')

module.exports = defineConfig({
  e2e: {
    // No baseUrl needed for file-based testing
    // baseUrl: 'http://localhost:3000',
    viewportWidth: 1280,
    viewportHeight: 720,
    video: false,
    screenshotOnRunFailure: true,
    defaultCommandTimeout: 10000,
    requestTimeout: 10000,
    responseTimeout: 10000,
    pageLoadTimeout: 30000,
    
    // BitActor TTL-aware testing configuration
    env: {
      TTL_BUDGET_MS: 8000,
      PIPELINE_STAGES: ['typer', 'turtle', 'ttl2dspy', 'bitactor', 'erlang', 'ash', 'reactor', 'k8s'],
      SWARM_COORDINATION_TIMEOUT: 5000,
      WEBSOCKET_TIMEOUT: 3000
    },
    
    setupNodeEvents(on, config) {
      // BitActor pipeline performance monitoring
      on('task', {
        measureTTLCompliance(testData) {
          const startTime = testData.startTime
          const endTime = Date.now()
          const duration = endTime - startTime
          const ttlBudget = config.env.TTL_BUDGET_MS
          
          return {
            duration,
            ttlBudget,
            compliance: duration <= ttlBudget,
            efficiency: (duration / ttlBudget) * 100
          }
        },
        
        validatePipelineStage(stageData) {
          const requiredStages = config.env.PIPELINE_STAGES
          const stageExists = requiredStages.includes(stageData.stageName.toLowerCase())
          
          return {
            stageName: stageData.stageName,
            exists: stageExists,
            ttlUsage: stageData.ttlUsage || 0,
            status: stageData.status || 'unknown'
          }
        },
        
        logSwarmCoordination(coordinationData) {
          console.log(`[SWARM] ${coordinationData.timestamp}: ${coordinationData.message}`)
          return coordinationData
        }
      })
      
      // Custom commands for BitActor testing
      on('before:browser:launch', (browser = {}, launchOptions) => {
        if (browser.name === 'chrome') {
          launchOptions.args.push('--disable-web-security')
          launchOptions.args.push('--disable-features=VizDisplayCompositor')
        }
        return launchOptions
      })
      
      return config
    },
    
    specPattern: [
      'cypress/e2e/nuxt-variants/**/*.cy.js',
      'cypress/e2e/nuxt-ui-variants/**/*.cy.js',
      'cypress/e2e/bitactor-integration/**/*.cy.js'
    ]
  },
  
  component: {
    devServer: {
      framework: 'nuxt',
      bundler: 'webpack'
    },
    specPattern: 'cypress/component/**/*.cy.js'
  }
})