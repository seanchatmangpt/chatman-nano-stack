describe('Ultrathink Pipeline E2E - 80/20 Optimization', () => {
  beforeEach(() => {
    cy.visit('/swarm-pipeline')
    cy.wait(1000) // Allow components to initialize
  })

  describe('Pipeline Connector Interface', () => {
    it('should load with default 80/20 optimization settings', () => {
      cy.get('[data-testid="ultrathink-pipeline-connector"]').should('be.visible')
      cy.get('[data-testid="optimization-strategy"]').should('contain', 'skip_non_critical')
      cy.get('[data-testid="selected-domain"]').should('contain', 'cybersecurity')
    })

    it('should display all pipeline stages with critical/non-critical indicators', () => {
      cy.get('[data-testid="pipeline-stage"]').should('have.length', 8)
      
      // Verify critical stages are marked
      cy.get('[data-testid="stage-typer"]').should('have.class', 'critical')
      cy.get('[data-testid="stage-turtle"]').should('have.class', 'critical')
      cy.get('[data-testid="stage-ash"]').should('have.class', 'critical')
      cy.get('[data-testid="stage-reactor"]').should('have.class', 'critical')
      cy.get('[data-testid="stage-k8s"]').should('have.class', 'critical')
      
      // Verify non-critical stages
      cy.get('[data-testid="stage-ttl2dspy"]').should('have.class', 'non-critical')
      cy.get('[data-testid="stage-erlang"]').should('have.class', 'non-critical')
    })

    it('should allow domain selection and update optimization', () => {
      cy.get('[data-testid="domain-selector"]').click()
      cy.get('[data-testid="domain-healthcare"]').click()
      
      // Should update to minimal path optimization
      cy.get('[data-testid="optimization-strategy"]').should('contain', 'minimal_path')
      
      // Should show fewer active stages for healthcare
      cy.get('[data-testid="active-stage"]').should('have.length.lessThan', 6)
    })
  })

  describe('80/20 Optimization Strategies', () => {
    it('should execute skip_non_critical strategy successfully', () => {
      cy.get('[data-testid="optimization-selector"]').select('skip_non_critical')
      cy.get('[data-testid="execute-pipeline-btn"]').click()
      
      // Should show execution progress
      cy.get('[data-testid="execution-status"]').should('contain', 'executing')
      
      // Should only execute critical stages
      cy.get('[data-testid="executing-stage"]').should('have.length', 5)
      
      // Wait for completion
      cy.get('[data-testid="execution-status"]', { timeout: 10000 })
        .should('contain', 'completed')
      
      // Verify time saved
      cy.get('[data-testid="time-saved"]').should('contain', 'ms')
      cy.get('[data-testid="efficiency-gain"]').should('exist')
    })

    it('should demonstrate parallel execution optimization', () => {
      cy.get('[data-testid="optimization-selector"]').select('parallel_execution')
      cy.get('[data-testid="execute-pipeline-btn"]').click()
      
      // Should show parallel execution indicators
      cy.get('[data-testid="parallel-stage-group"]').should('exist')
      
      // Should complete faster than sequential
      const startTime = Date.now()
      cy.get('[data-testid="execution-status"]', { timeout: 15000 })
        .should('contain', 'completed')
        .then(() => {
          const endTime = Date.now()
          const duration = endTime - startTime
          cy.wrap(duration).should('be.lessThan', 8000) // Under 8 seconds
        })
    })

    it('should apply minimal_path for healthcare domain', () => {
      cy.get('[data-testid="domain-selector"]').select('healthcare')
      cy.get('[data-testid="execute-pipeline-btn"]').click()
      
      // Should only execute essential stages
      cy.get('[data-testid="executing-stage"]').should('have.length', 3)
      
      // Should include typer, turtle, k8s
      cy.get('[data-testid="stage-typer"]').should('have.class', 'executing')
      cy.get('[data-testid="stage-turtle"]').should('have.class', 'executing')
      cy.get('[data-testid="stage-k8s"]').should('have.class', 'executing')
    })

    it('should show performance improvements with optimization', () => {
      // Execute baseline (full pipeline)
      cy.get('[data-testid="optimization-selector"]').select('adaptive_routing')
      cy.get('[data-testid="execute-pipeline-btn"]').click()
      cy.get('[data-testid="execution-status"]', { timeout: 15000 })
        .should('contain', 'completed')
      
      cy.get('[data-testid="execution-duration"]').invoke('text').then(baselineDuration => {
        // Execute optimized
        cy.get('[data-testid="optimization-selector"]').select('skip_non_critical')
        cy.get('[data-testid="execute-pipeline-btn"]').click()
        cy.get('[data-testid="execution-status"]', { timeout: 10000 })
          .should('contain', 'completed')
        
        cy.get('[data-testid="execution-duration"]').invoke('text').then(optimizedDuration => {
          const baseline = parseInt(baselineDuration.replace(/\D/g, ''))
          const optimized = parseInt(optimizedDuration.replace(/\D/g, ''))
          
          // Should show at least 20% improvement
          expect(optimized).to.be.lessThan(baseline * 0.8)
        })
      })
    })
  })

  describe('Permutation Generation and Selection', () => {
    it('should generate multiple optimization permutations', () => {
      cy.get('[data-testid="generate-permutations-btn"]').click()
      
      cy.get('[data-testid="permutation-result"]').should('have.length.greaterThan', 3)
      
      // Should mark optimal permutation
      cy.get('[data-testid="optimal-permutation"]').should('exist')
      cy.get('[data-testid="optimal-permutation"]')
        .find('[data-testid="efficiency-score"]')
        .should('contain', '%')
        .invoke('text')
        .then(efficiency => {
          const score = parseInt(efficiency.replace(/\D/g, ''))
          expect(score).to.be.greaterThan(80)
        })
    })

    it('should allow selection and execution of specific permutation', () => {
      cy.get('[data-testid="generate-permutations-btn"]').click()
      
      // Select a high-efficiency permutation
      cy.get('[data-testid="permutation-result"]')
        .first()
        .find('[data-testid="select-permutation-btn"]')
        .click()
      
      cy.get('[data-testid="selected-permutation"]').should('be.visible')
      
      cy.get('[data-testid="execute-selected-permutation-btn"]').click()
      
      cy.get('[data-testid="execution-status"]', { timeout: 10000 })
        .should('contain', 'completed')
    })

    it('should display permutation efficiency metrics', () => {
      cy.get('[data-testid="generate-permutations-btn"]').click()
      
      cy.get('[data-testid="permutation-result"]').each($permutation => {
        cy.wrap($permutation).within(() => {
          cy.get('[data-testid="efficiency-score"]').should('be.visible')
          cy.get('[data-testid="execution-path"]').should('be.visible')
          cy.get('[data-testid="estimated-duration"]').should('be.visible')
          cy.get('[data-testid="optimization-strategy"]').should('be.visible')
        })
      })
    })
  })

  describe('Real-time Notifications and Channels', () => {
    it('should display notification channels interface', () => {
      cy.get('[data-testid="ash-reactor-notifications"]').should('be.visible')
      cy.get('[data-testid="notification-controls"]').should('be.visible')
      cy.get('[data-testid="notification-stream"]').should('be.visible')
    })

    it('should filter notifications based on 80/20 optimization', () => {
      // Enable 80/20 filtering
      cy.get('[data-testid="optimization-toggle"]').check()
      
      // Execute pipeline to generate notifications
      cy.get('[data-testid="execute-pipeline-btn"]').click()
      
      // Should prioritize critical notifications
      cy.get('[data-testid="notification-item"]')
        .should('have.length.greaterThan', 0)
      
      cy.get('[data-testid="notification-item"][data-priority="critical"]')
        .should('have.length.greaterThan', 0)
      
      // Non-critical notifications should be fewer or batched
      cy.get('[data-testid="notification-item"][data-priority="low"]')
        .should('have.length.lessThan', 5)
    })

    it('should subscribe to critical channels only in 80/20 mode', () => {
      cy.get('[data-testid="channel-subscription"]').click()
      
      cy.get('[data-testid="channel-ash_resources"]').check()
      cy.get('[data-testid="channel-reactor_workflows"]').check()
      cy.get('[data-testid="channel-error_alerts"]').check()
      cy.get('[data-testid="channel-debug_logs"]').check()
      
      cy.get('[data-testid="subscribe-btn"]').click()
      
      // Should filter to critical channels only
      cy.get('[data-testid="subscribed-channel"]')
        .should('contain', 'ash_resources')
        .and('contain', 'reactor_workflows')
        .and('contain', 'error_alerts')
        .and('not.contain', 'debug_logs')
    })

    it('should handle real-time WebSocket updates', () => {
      // Start pipeline execution
      cy.get('[data-testid="execute-pipeline-btn"]').click()
      
      // Should receive real-time stage updates
      cy.get('[data-testid="stage-typer"]').should('have.class', 'executing')
      
      // Should show progression through stages
      cy.get('[data-testid="stage-typer"]', { timeout: 5000 })
        .should('have.class', 'completed')
      
      cy.get('[data-testid="stage-turtle"]').should('have.class', 'executing')
      
      // Should update notification stream in real-time
      cy.get('[data-testid="notification-stream"]')
        .children()
        .should('have.length.greaterThan', 1)
    })
  })

  describe('Swarm Intelligence Integration', () => {
    it('should apply optimization recommendations', () => {
      cy.get('[data-testid="swarm-optimize-btn"]').click()
      
      cy.get('[data-testid="optimization-modal"]').should('be.visible')
      
      cy.get('[data-testid="optimization-target"]').select('pipeline')
      cy.get('[data-testid="optimization-strategy"]').select('80_20')
      
      cy.get('[data-testid="apply-optimization-btn"]').click()
      
      // Should show optimization applied
      cy.get('[data-testid="optimization-applied"]').should('be.visible')
      cy.get('[data-testid="estimated-improvement"]').should('contain', '%')
    })

    it('should learn from execution patterns', () => {
      // Execute pipeline multiple times to generate learning data
      for (let i = 0; i < 3; i++) {
        cy.get('[data-testid="execute-pipeline-btn"]').click()
        cy.get('[data-testid="execution-status"]', { timeout: 10000 })
          .should('contain', 'completed')
        cy.wait(1000)
      }
      
      cy.get('[data-testid="swarm-learn-btn"]').click()
      
      cy.get('[data-testid="learning-result"]').should('be.visible')
      cy.get('[data-testid="patterns-learned"]').should('contain.text', '3')
      cy.get('[data-testid="confidence-score"]').should('be.visible')
    })

    it('should generate contextual recommendations', () => {
      // Set high complexity scenario
      cy.get('[data-testid="complexity-slider"]').invoke('val', 8).trigger('input')
      
      cy.get('[data-testid="get-recommendations-btn"]').click()
      
      cy.get('[data-testid="recommendations-list"]').should('be.visible')
      cy.get('[data-testid="recommendation-item"]')
        .should('have.length.greaterThan', 0)
      
      // Should include high complexity recommendations
      cy.get('[data-testid="recommendation-item"]')
        .first()
        .should('contain', 'parallel')
    })
  })

  describe('Performance and Error Handling', () => {
    it('should handle pipeline execution errors gracefully', () => {
      // Force an error by selecting invalid configuration
      cy.get('[data-testid="debug-mode"]').check()
      cy.get('[data-testid="force-error"]').check()
      
      cy.get('[data-testid="execute-pipeline-btn"]').click()
      
      cy.get('[data-testid="execution-status"]', { timeout: 5000 })
        .should('contain', 'failed')
      
      // Should show error notification
      cy.get('[data-testid="error-notification"]').should('be.visible')
      
      // Interface should remain functional
      cy.get('[data-testid="execute-pipeline-btn"]').should('not.be.disabled')
    })

    it('should maintain performance under load', () => {
      // Simulate rapid interactions
      for (let i = 0; i < 10; i++) {
        cy.get('[data-testid="generate-permutations-btn"]').click()
        cy.wait(100)
      }
      
      // Interface should remain responsive
      cy.get('[data-testid="optimization-selector"]').should('be.enabled')
      cy.get('[data-testid="execute-pipeline-btn"]').should('be.enabled')
    })

    it('should handle WebSocket disconnection gracefully', () => {
      // Start execution
      cy.get('[data-testid="execute-pipeline-btn"]').click()
      
      // Simulate network disconnection
      cy.window().then(win => {
        if (win.WebSocket) {
          win.WebSocket.prototype.close.call()
        }
      })
      
      // Should show connection status
      cy.get('[data-testid="connection-status"]')
        .should('contain', 'disconnected')
      
      // Should attempt reconnection
      cy.get('[data-testid="connection-status"]', { timeout: 10000 })
        .should('contain', 'connected')
    })
  })

  describe('80/20 Optimization Validation', () => {
    it('should demonstrate 80/20 principle in practice', () => {
      cy.get('[data-testid="optimization-selector"]').select('skip_non_critical')
      cy.get('[data-testid="execute-pipeline-btn"]').click()
      
      cy.get('[data-testid="execution-status"]', { timeout: 10000 })
        .should('contain', 'completed')
      
      // Should show 80/20 metrics
      cy.get('[data-testid="optimization-metrics"]').should('be.visible')
      
      // 20% of stages should provide 80% of value
      cy.get('[data-testid="critical-stage-ratio"]')
        .should('contain', '62.5%') // 5 critical out of 8 total stages
      
      cy.get('[data-testid="value-delivered"]')
        .invoke('text')
        .then(value => {
          const percentage = parseInt(value.replace(/\D/g, ''))
          expect(percentage).to.be.greaterThan(80)
        })
    })

    it('should show measurable performance improvement', () => {
      // Measure baseline performance
      cy.get('[data-testid="optimization-selector"]').select('adaptive_routing')
      cy.get('[data-testid="execute-pipeline-btn"]').click()
      
      const baselineStart = Date.now()
      cy.get('[data-testid="execution-status"]', { timeout: 15000 })
        .should('contain', 'completed')
        .then(() => {
          const baselineEnd = Date.now()
          const baselineDuration = baselineEnd - baselineStart
          
          // Measure optimized performance
          cy.get('[data-testid="optimization-selector"]').select('skip_non_critical')
          cy.get('[data-testid="execute-pipeline-btn"]').click()
          
          const optimizedStart = Date.now()
          cy.get('[data-testid="execution-status"]', { timeout: 10000 })
            .should('contain', 'completed')
            .then(() => {
              const optimizedEnd = Date.now()
              const optimizedDuration = optimizedEnd - optimizedStart
              
              // Should show at least 20% improvement
              const improvement = (baselineDuration - optimizedDuration) / baselineDuration
              expect(improvement).to.be.greaterThan(0.2)
            })
        })
    })

    it('should maintain critical functionality while optimizing', () => {
      cy.get('[data-testid="optimization-selector"]').select('minimal_path')
      cy.get('[data-testid="execute-pipeline-btn"]').click()
      
      cy.get('[data-testid="execution-status"]', { timeout: 10000 })
        .should('contain', 'completed')
      
      // Should still produce valid outputs
      cy.get('[data-testid="pipeline-outputs"]').should('be.visible')
      cy.get('[data-testid="output-item"]').should('have.length.greaterThan', 0)
      
      // Essential functionality should be preserved
      cy.get('[data-testid="output-item"]')
        .should('contain', 'processed_data')
        .and('contain', 'ash_resources')
        .and('contain', 'k8s_manifests')
    })
  })

  describe('Cross-browser Compatibility', () => {
    it('should work correctly in different browsers', () => {
      // This test should be run across multiple browsers via Cypress config
      cy.get('[data-testid="ultrathink-pipeline-connector"]').should('be.visible')
      cy.get('[data-testid="execute-pipeline-btn"]').click()
      cy.get('[data-testid="execution-status"]', { timeout: 10000 })
        .should('contain', 'completed')
    })
  })

  describe('Accessibility', () => {
    it('should be accessible to screen readers', () => {
      cy.get('[data-testid="execute-pipeline-btn"]')
        .should('have.attr', 'aria-label')
      
      cy.get('[data-testid="optimization-selector"]')
        .should('have.attr', 'aria-label')
      
      cy.get('[data-testid="pipeline-stage"]')
        .each($stage => {
          cy.wrap($stage).should('have.attr', 'role')
        })
    })

    it('should support keyboard navigation', () => {
      cy.get('body').tab()
      cy.focused().should('have.attr', 'data-testid')
      
      cy.focused().tab()
      cy.focused().should('have.attr', 'data-testid')
    })
  })
})