const { defineConfig } = require('cypress')

module.exports = defineConfig({
  e2e: {
    baseUrl: 'http://localhost:3000',
    supportFile: 'cypress/support/e2e.js',
    specPattern: 'cypress/e2e/**/*.cy.{js,jsx,ts,tsx}',
    videosFolder: 'cypress/videos',
    screenshotsFolder: 'cypress/screenshots',
    viewportWidth: 1280,
    viewportHeight: 720,
    video: true,
    videoCompression: 32,
    defaultCommandTimeout: 10000,
    requestTimeout: 10000,
    responseTimeout: 10000,
    pageLoadTimeout: 30000,
    
    // Enhanced environment variables for Ultrathink Swarm 80/20 testing
    env: {
      phoenixUrl: 'ws://localhost:4000/socket',
      apiUrl: 'http://localhost:4000/api',
      PIPELINE_API_URL: 'ws://localhost:4000/socket',
      MOCK_BACKEND: true,
      SWARM_MODE: true,
      TEST_TIMEOUT: 30000,
      
      // 80/20 Test Categories - Critical paths only
      CRITICAL_TESTS: [
        'pipeline-control-center',
        'websocket-channels', 
        'reactor-integration',
        'ash-integration'
      ],
      
      // Backend service endpoints
      ASH_ENDPOINT: 'http://localhost:4000/api/ash',
      REACTOR_ENDPOINT: 'http://localhost:4000/api/reactor',
      CHANNEL_ENDPOINT: 'ws://localhost:4000/socket/websocket'
    },
    
    setupNodeEvents(on, config) {
      // Ultrathink Swarm test orchestration
      on('task', {
        // Initialize swarm testing
        initializeSwarm() {
          console.log('🌊 Initializing Ultrathink Swarm Tests')
          return {
            timestamp: new Date().toISOString(),
            swarmId: `swarm_${Date.now()}`,
            status: 'initialized'
          }
        },
        
        // Mock backend services for testing
        startMockServices() {
          console.log('🎭 Starting mock backend services')
          return {
            ash: 'http://localhost:4001',
            reactor: 'http://localhost:4002', 
            websocket: 'ws://localhost:4003'
          }
        },
        
        // Generate test permutations using 80/20 approach
        generatePermutations(options) {
          const permutations = []
          const { components, flows, modes } = options
          
          components.forEach(component => {
            flows.forEach(flow => {
              modes.forEach(mode => {
                permutations.push({
                  id: `${component}_${flow}_${mode}`,
                  component,
                  flow,
                  mode,
                  priority: getPriority(component, flow)
                })
              })
            })
          })
          
          return permutations.sort((a, b) => b.priority - a.priority)
        },
        
        // Log swarm test results
        logSwarmResults(results) {
          console.log('📊 Swarm Test Results:', JSON.stringify(results, null, 2))
          return results
        }
      })
      
      return config
    }
  },
  
  component: {
    devServer: {
      framework: 'nuxt',
      bundler: 'vite'
    },
    supportFile: 'cypress/support/component.js',
    specPattern: 'cypress/component/**/*.cy.{js,jsx,ts,tsx}',
    viewportWidth: 1000,
    viewportHeight: 660
  },
  
  retries: {
    runMode: 2,
    openMode: 0
  },
  
  experimentalStudio: true,
  experimentalWebKitSupport: true
})

// 80/20 Priority calculation for test optimization
function getPriority(component, flow) {
  // 80/20 prioritization - focus on critical components and flows
  const criticalComponents = ['pipeline-control-center', 'websocket-bridge', 'reactor-integration']
  const criticalFlows = ['ash-integration', 'reactor-integration', 'channel-communication']
  
  let priority = 1
  if (criticalComponents.includes(component)) priority += 3
  if (criticalFlows.includes(flow)) priority += 2
  
  return priority
}