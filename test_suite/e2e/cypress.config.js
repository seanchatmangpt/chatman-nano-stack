const { defineConfig } = require('cypress')

module.exports = defineConfig({
  e2e: {
    baseUrl: 'http://localhost:3000',
    viewportWidth: 1280,
    viewportHeight: 720,
    defaultCommandTimeout: 10000,
    requestTimeout: 15000,
    responseTimeout: 15000,
    video: true,
    screenshotOnRunFailure: true,
    
    // Test files
    specPattern: 'cypress/integration/**/*.cy.{js,jsx,ts,tsx}',
    
    // Support file
    supportFile: 'cypress/support/e2e.js',
    
    // Fixtures
    fixturesFolder: 'cypress/fixtures',
    
    // Screenshots and videos
    screenshotsFolder: 'cypress/screenshots',
    videosFolder: 'cypress/videos',
    
    setupNodeEvents(on, config) {
      // Custom tasks for 80/20 optimization testing
      on('task', {
        // Performance measurement task
        measurePerformance(testData) {
          const { baseline, optimized } = testData
          const improvement = (baseline - optimized) / baseline
          return {
            improvement_percentage: Math.round(improvement * 100),
            meets_80_20_criteria: improvement >= 0.2,
            baseline_duration: baseline,
            optimized_duration: optimized
          }
        },
        
        // 80/20 optimization validation
        validate80_20Optimization(data) {
          const { stages, criticalStages } = data
          const criticalRatio = criticalStages.length / stages.length
          
          return {
            critical_stage_ratio: Math.round(criticalRatio * 100),
            meets_pareto_principle: criticalRatio <= 0.3 && criticalRatio >= 0.15, // 15-30% for flexibility
            total_stages: stages.length,
            critical_stages: criticalStages.length
          }
        },
        
        // Swarm intelligence metrics
        validateSwarmMetrics(metrics) {
          return {
            learning_effectiveness: metrics.patterns_learned > 0,
            confidence_acceptable: metrics.confidence_score >= 0.7,
            recommendation_quality: metrics.recommendations_applied > 0
          }
        },
        
        // Channel performance validation
        validateChannelPerformance(channelData) {
          const { messagesProcessed, processingTime, errorRate } = channelData
          
          return {
            throughput: messagesProcessed / (processingTime / 1000), // messages per second
            performance_acceptable: processingTime < 5000, // under 5 seconds
            reliability_acceptable: errorRate < 0.05, // less than 5% error rate
            efficiency_score: Math.round((messagesProcessed / processingTime) * 1000)
          }
        }
      })
      
      // Browser launch options
      on('before:browser:launch', (browser = {}, launchOptions) => {
        if (browser.name === 'chrome') {
          launchOptions.args.push('--disable-web-security')
          launchOptions.args.push('--disable-features=VizDisplayCompositor')
        }
        
        return launchOptions
      })
      
      // Environment variables
      config.env = {
        ...config.env,
        WEBSOCKET_URL: process.env.WEBSOCKET_URL || 'ws://localhost:4000/socket/websocket',
        API_BASE_URL: process.env.API_BASE_URL || 'http://localhost:4000/api',
        OPTIMIZATION_MODE: '80_20',
        TEST_USER_ID: 'cypress_test_user'
      }
      
      return config
    },
    
    // Environment-specific configurations
    env: {
      optimization_threshold: 0.2, // 20% improvement requirement
      critical_stage_ratio: 0.25, // 25% of stages should be critical
      confidence_threshold: 0.8, // 80% confidence for recommendations
      max_execution_time: 10000, // 10 seconds max execution time
      
      // Phoenix channel configuration
      channel_timeout: 5000,
      websocket_timeout: 10000,
      
      // Swarm intelligence thresholds
      min_learning_patterns: 1,
      min_confidence_score: 0.7,
      max_recommendation_response_time: 3000
    }
  },
  
  // Component testing configuration
  component: {
    devServer: {
      framework: 'nuxt',
      bundler: 'vite'
    },
    
    specPattern: 'cypress/component/**/*.cy.{js,jsx,ts,tsx}',
    supportFile: 'cypress/support/component.js'
  },
  
  // Cross-browser testing
  browsers: [
    {
      name: 'chrome',
      channel: 'stable'
    },
    {
      name: 'firefox',
      channel: 'stable' 
    },
    {
      name: 'edge',
      channel: 'stable'
    }
  ],
  
  // Retry configuration
  retries: {
    runMode: 2,
    openMode: 1
  },
  
  // Parallelization
  parallelization: true,
  
  // Test isolation
  testIsolation: true,
  
  // Custom reporter configuration
  reporter: 'cypress-multi-reporters',
  reporterOptions: {
    configFile: 'reporter-config.json'
  }
})