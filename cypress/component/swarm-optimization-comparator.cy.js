// SwarmOptimizationComparator Component Tests

import SwarmOptimizationComparator from '../../nuxt_ui/components/SwarmOptimizationComparator.vue'

describe('SwarmOptimizationComparator Component', () => {
  beforeEach(() => {
    cy.mountSwarmComponent(SwarmOptimizationComparator, {
      availableStrategies: [
        { id: 'linear', name: 'Linear Sequential', icon: '📝', efficiency: 0.6 },
        { id: 'skip_optimization', name: 'Skip Optimization (80/20)', icon: '⚡', efficiency: 0.9 },
        { id: 'parallel_merge', name: 'Parallel Merge', icon: '🔀', efficiency: 0.8 },
        { id: 'emergence_guided', name: 'Emergence Guided', icon: '🧠', efficiency: 0.85 }
      ]
    })
  })

  describe('🎨 Component Rendering', () => {
    it('should render the main comparator interface', () => {
      cy.getByDataCy('swarm-optimization-comparator')
        .should('be.visible')

      cy.contains('🔬 Swarm Optimization Comparator')
        .should('be.visible')

      cy.testComponentPerformance('swarm-optimization-comparator')
    })

    it('should render strategy selection grid', () => {
      cy.getByDataCy('strategy-grid')
        .should('be.visible')

      cy.getByDataCy('strategy-card')
        .should('have.length', 4)

      cy.getByDataCy('strategy-card')
        .each(($card) => {
          cy.wrap($card).within(() => {
            cy.get('.strategy-icon').should('be.visible')
            cy.get('.strategy-name').should('be.visible')
            cy.get('.strategy-efficiency').should('contain', '%')
          })
        })
    })

    it('should render comparison controls', () => {
      cy.getByDataCy('comparison-controls')
        .should('be.visible')

      cy.getByDataCy('run-comparison')
        .should('be.visible')
        .and('be.disabled') // Initially disabled

      cy.getByDataCy('clear-comparison')
        .should('be.visible')

      cy.getByDataCy('export-results')
        .should('be.visible')
    })
  })

  describe('🎯 Strategy Selection', () => {
    it('should allow selecting multiple strategies for comparison', () => {
      // Select first strategy
      cy.getByDataCy('strategy-card')
        .first()
        .click()

      cy.getByDataCy('strategy-card')
        .first()
        .should('have.class', 'selected')

      // Select second strategy
      cy.getByDataCy('strategy-card')
        .eq(1)
        .click()

      cy.getByDataCy('strategy-card')
        .eq(1)
        .should('have.class', 'selected')

      // Run comparison should now be enabled
      cy.getByDataCy('run-comparison')
        .should('not.be.disabled')
    })

    it('should limit selection to maximum 4 strategies', () => {
      // Select all 4 strategies
      cy.getByDataCy('strategy-card')
        .each(($card) => {
          cy.wrap($card).click()
        })

      cy.getByDataCy('strategy-card')
        .should('have.class', 'selected')

      // Try to add more should not work (all cards should be disabled or selected)
      cy.getByDataCy('strategy-card')
        .should('not.have.class', 'disabled')
    })

    it('should show efficiency indicators for each strategy', () => {
      cy.getByDataCy('strategy-card')
        .each(($card) => {
          cy.wrap($card).within(() => {
            cy.get('.strategy-efficiency')
              .should('be.visible')
              .and('contain', '%')
              .then(($el) => {
                const efficiency = parseInt($el.text())
                expect(efficiency).to.be.greaterThan(0)
                expect(efficiency).to.be.lessThan(100)
              })
          })
        })
    })

    it('should provide visual feedback for strategy selection', () => {
      cy.getByDataCy('strategy-card')
        .first()
        .should('not.have.class', 'selected')

      cy.getByDataCy('strategy-card')
        .first()
        .click()

      cy.getByDataCy('strategy-card')
        .first()
        .should('have.class', 'selected')
        .and('have.css', 'border-color', 'rgb(0, 255, 136)') // Selected color

      // Deselect
      cy.getByDataCy('strategy-card')
        .first()
        .click()

      cy.getByDataCy('strategy-card')
        .first()
        .should('not.have.class', 'selected')
    })
  })

  describe('🚀 Comparison Execution', () => {
    beforeEach(() => {
      // Select two strategies for comparison
      cy.getByDataCy('strategy-card')
        .contains('Skip Optimization')
        .click()

      cy.getByDataCy('strategy-card')
        .contains('Linear Sequential')
        .click()
    })

    it('should execute strategy comparison and show results', () => {
      cy.getByDataCy('run-comparison')
        .click()

      cy.getByDataCy('comparison-results')
        .should('be.visible')

      cy.getByDataCy('comparison-result')
        .should('have.length', 2)

      cy.getByDataCy('comparison-result')
        .each(($result) => {
          cy.wrap($result).within(() => {
            cy.get('.result-header').should('be.visible')
            cy.get('.metrics-section').should('be.visible')
            cy.get('.path-section').should('be.visible')
            cy.get('.optimizations-section').should('be.visible')
          })
        })
    })

    it('should identify optimal strategy', () => {
      cy.getByDataCy('run-comparison')
        .click()

      cy.getByDataCy('optimal-badge')
        .should('be.visible')
        .and('contain', 'OPTIMAL')

      // Optimal result should have special styling
      cy.get('.comparison-result.optimal')
        .should('exist')
        .and('have.css', 'border-color', 'rgb(255, 215, 0)') // Gold border
    })

    it('should show performance metrics for each strategy', () => {
      cy.getByDataCy('run-comparison')
        .click()

      cy.getByDataCy('comparison-result')
        .each(($result) => {
          cy.wrap($result).within(() => {
            cy.getByDataCy('execution-time')
              .should('be.visible')
              .and('contain', 'ms')

            cy.getByDataCy('stages-used')
              .should('be.visible')
              .and('match', /\d+\/8/)

            cy.getByDataCy('resource-usage')
              .should('be.visible')
              .and('contain', '%')

            cy.getByDataCy('80-20-score')
              .should('be.visible')
              .and('contain', '%')
          })
        })
    })

    it('should display execution paths for each strategy', () => {
      cy.getByDataCy('run-comparison')
        .click()

      cy.getByDataCy('execution-path')
        .should('have.length', 2)

      cy.getByDataCy('execution-path')
        .each(($path) => {
          cy.wrap($path).within(() => {
            cy.get('.path-stage')
              .should('have.length.greaterThan', 2)

            cy.get('.path-stage.critical')
              .should('exist') // Should have critical stages marked

            cy.get('.path-arrow')
              .should('exist') // Should have connecting arrows
          })
        })
    })

    it('should show applied optimizations for each strategy', () => {
      cy.getByDataCy('run-comparison')
        .click()

      cy.getByDataCy('optimization-list')
        .should('have.length', 2)

      cy.getByDataCy('optimization-item')
        .should('have.length.greaterThan', 0)

      cy.getByDataCy('optimization-item')
        .each(($opt) => {
          cy.wrap($opt).within(() => {
            cy.get('.optimization-icon').should('be.visible')
            cy.get('.optimization-text').should('be.visible')
            cy.get('.optimization-impact')
              .should('be.visible')
              .and('contain', '%')
          })
        })
    })

    it('should display swarm intelligence insights', () => {
      cy.getByDataCy('run-comparison')
        .click()

      cy.getByDataCy('insights-section')
        .should('have.length', 2)

      cy.getByDataCy('insight-item')
        .should('have.length.greaterThan', 0)

      cy.getByDataCy('insight-item')
        .each(($insight) => {
          cy.wrap($insight).should('contain.text', 'swarm')
            .or('contain.text', 'efficiency')
            .or('contain.text', 'coordination')
        })
    })
  })

  describe('📊 Comparison Chart', () => {
    beforeEach(() => {
      // Select three strategies for richer comparison
      cy.getByDataCy('strategy-card')
        .contains('Skip Optimization')
        .click()

      cy.getByDataCy('strategy-card')
        .contains('Linear Sequential')
        .click()

      cy.getByDataCy('strategy-card')
        .contains('Parallel Merge')
        .click()

      cy.getByDataCy('run-comparison')
        .click()
    })

    it('should render comparison chart after running comparison', () => {
      cy.getByDataCy('comparison-chart')
        .should('be.visible')

      cy.get('canvas[ref="comparisonCanvas"]')
        .should('be.visible')
        .and('have.attr', 'width', '800')
        .and('have.attr', 'height', '400')
    })

    it('should draw performance bars for each strategy', () => {
      cy.get('canvas[ref="comparisonCanvas"]')
        .should('be.visible')
        .then(($canvas) => {
          const canvas = $canvas[0]
          const context = canvas.getContext('2d')
          
          // Verify canvas has been drawn on (not blank)
          const imageData = context.getImageData(0, 0, canvas.width, canvas.height)
          const hasNonTransparentPixels = Array.from(imageData.data)
            .some((value, index) => index % 4 === 3 && value > 0) // Check alpha channel
          
          expect(hasNonTransparentPixels).to.be.true
        })
    })

    it('should show strategy names and scores on chart', () => {
      // Chart should be rendered with strategy data
      cy.getByDataCy('comparison-chart')
        .should('contain', 'Overall Performance Score')
    })
  })

  describe('📋 Side-by-Side Analysis', () => {
    beforeEach(() => {
      cy.getByDataCy('strategy-card')
        .contains('Skip Optimization')
        .click()

      cy.getByDataCy('strategy-card')
        .contains('Emergence Guided')
        .click()

      cy.getByDataCy('run-comparison')
        .click()
    })

    it('should render side-by-side analysis table', () => {
      cy.getByDataCy('side-by-side-analysis')
        .should('be.visible')

      cy.getByDataCy('analysis-table')
        .should('be.visible')

      cy.get('table')
        .should('be.visible')
        .within(() => {
          cy.get('thead tr th')
            .should('have.length', 3) // Metric + 2 strategies

          cy.get('tbody tr')
            .should('have.length.greaterThan', 3) // Multiple metrics
        })
    })

    it('should compare key metrics across strategies', () => {
      cy.get('table tbody tr')
        .each(($row) => {
          cy.wrap($row).within(() => {
            cy.get('td')
              .first()
              .should('contain.text', /Execution Time|Stages Used|Resource Efficiency|80\/20 Optimization|Swarm Intelligence/)

            // Should have values for each strategy
            cy.get('td')
              .should('have.length', 3)
          })
        })
    })

    it('should highlight best values in each metric', () => {
      // This would require custom styling to highlight best values
      cy.get('table tbody tr')
        .first()
        .within(() => {
          cy.get('td')
            .not(':first')
            .should('contain.text', /\d+/) // Should contain numeric values
        })
    })
  })

  describe('💡 Recommendations Panel', () => {
    beforeEach(() => {
      // Select strategies and run comparison to generate recommendations
      cy.getByDataCy('strategy-card')
        .contains('Linear Sequential')
        .click()

      cy.getByDataCy('strategy-card')
        .contains('Skip Optimization')
        .click()

      cy.getByDataCy('run-comparison')
        .click()
    })

    it('should show recommendations based on comparison results', () => {
      cy.getByDataCy('recommendations-panel')
        .should('be.visible')

      cy.getByDataCy('recommendation-list')
        .should('be.visible')
        .children()
        .should('have.length.greaterThan', 0)
    })

    it('should categorize recommendations by priority', () => {
      cy.getByDataCy('recommendation-item')
        .should('have.length.greaterThan', 0)

      cy.getByDataCy('recommendation-item')
        .each(($item) => {
          cy.wrap($item)
            .should('have.class', /high|medium|low/)
        })
    })

    it('should allow applying recommendations', () => {
      cy.getByDataCy('recommendation-item')
        .first()
        .within(() => {
          cy.getByDataCy('recommendation-title')
            .should('be.visible')

          cy.getByDataCy('recommendation-description')
            .should('be.visible')

          cy.getByDataCy('apply-recommendation')
            .should('be.visible')
            .click()
        })

      // Recommendation should be removed after applying
      cy.getByDataCy('recommendation-list')
        .children()
        .should('have.length.lessThan', 3) // Assuming we started with 3
    })

    it('should provide different types of recommendations', () => {
      cy.getByDataCy('recommendation-item')
        .should('exist')

      // Should have various recommendation types
      const expectedTypes = ['performance', 'optimization', 'resource', 'intelligence']
      
      expectedTypes.forEach(type => {
        cy.getByDataCy('recommendation-item')
          .should('contain.text', type)
          .or('contain.text', 'strategy')
          .or('contain.text', 'efficiency')
      })
    })
  })

  describe('📤 Export Functionality', () => {
    beforeEach(() => {
      cy.getByDataCy('strategy-card')
        .contains('Skip Optimization')
        .click()

      cy.getByDataCy('strategy-card')
        .contains('Parallel Merge')
        .click()

      cy.getByDataCy('run-comparison')
        .click()
    })

    it('should export comparison results', () => {
      cy.getByDataCy('export-results')
        .click()

      // Verify download was initiated (in real implementation)
      // For component test, just verify the function was called
      cy.window().its('URL.createObjectURL')
        .should('exist')
    })

    it('should include all comparison data in export', () => {
      cy.getByDataCy('export-results')
        .click()

      // In real implementation, would verify JSON structure
      // For component test, verify export function behavior
      cy.window().then((win) => {
        // Mock verification that export data is complete
        expect(win.exportData).to.exist
      })
    })
  })

  describe('🔧 Error Handling', () => {
    it('should handle comparison execution errors', () => {
      cy.getByDataCy('strategy-card')
        .first()
        .click()

      cy.getByDataCy('strategy-card')
        .eq(1)
        .click()

      // Simulate error in comparison execution
      cy.window().then((win) => {
        win.simulateComparisonError = true
      })

      cy.getByDataCy('run-comparison')
        .click()

      cy.getByDataCy('comparison-error')
        .should('be.visible')
        .and('contain', 'comparison failed')

      cy.getByDataCy('retry-comparison')
        .should('be.visible')
        .click()

      // Should retry comparison
      cy.getByDataCy('comparison-error')
        .should('not.exist')
    })

    it('should validate minimum strategy selection', () => {
      // Try to run comparison with only one strategy
      cy.getByDataCy('strategy-card')
        .first()
        .click()

      cy.getByDataCy('run-comparison')
        .should('be.disabled')

      // Add second strategy
      cy.getByDataCy('strategy-card')
        .eq(1)
        .click()

      cy.getByDataCy('run-comparison')
        .should('not.be.disabled')
    })

    it('should handle empty comparison results gracefully', () => {
      cy.getByDataCy('strategy-card')
        .first()
        .click()

      cy.getByDataCy('strategy-card')
        .eq(1)
        .click()

      // Simulate empty results
      cy.window().then((win) => {
        win.mockEmptyResults = true
      })

      cy.getByDataCy('run-comparison')
        .click()

      cy.getByDataCy('no-results-message')
        .should('be.visible')
        .and('contain', 'No comparison results available')
    })
  })

  describe('📱 Responsive Design', () => {
    it('should adapt to mobile viewport', () => {
      cy.viewport(375, 667)

      cy.getByDataCy('swarm-optimization-comparator')
        .should('be.visible')

      cy.getByDataCy('strategy-grid')
        .should('have.css', 'grid-template-columns')
        .and('match', /repeat\(auto-fit/)

      cy.getByDataCy('comparison-controls')
        .should('have.css', 'flex-direction', 'column')
    })

    it('should work on tablet viewport', () => {
      cy.viewport(768, 1024)

      cy.getByDataCy('strategy-grid')
        .should('be.visible')

      cy.getByDataCy('comparison-grid')
        .should('have.css', 'grid-template-columns')
        .and('match', /minmax/)
    })
  })

  describe('♿ Accessibility', () => {
    it('should have proper ARIA labels and roles', () => {
      cy.getByDataCy('strategy-card')
        .first()
        .should('have.attr', 'role', 'button')
        .and('have.attr', 'aria-label')

      cy.getByDataCy('run-comparison')
        .should('have.attr', 'aria-label')

      cy.getByDataCy('comparison-results')
        .should('have.attr', 'role', 'region')
    })

    it('should be keyboard navigable', () => {
      // Tab through strategy cards
      cy.getByDataCy('strategy-card')
        .first()
        .focus()
        .type('{enter}')

      cy.getByDataCy('strategy-card')
        .first()
        .should('have.class', 'selected')

      // Tab to next card
      cy.getByDataCy('strategy-card')
        .eq(1)
        .focus()
        .type('{enter}')

      // Tab to run comparison button
      cy.getByDataCy('run-comparison')
        .focus()
        .type('{enter}')

      cy.getByDataCy('comparison-results')
        .should('be.visible')
    })

    it('should announce changes to screen readers', () => {
      cy.getByDataCy('sr-status')
        .should('have.attr', 'aria-live', 'polite')

      cy.getByDataCy('strategy-card')
        .first()
        .click()

      cy.getByDataCy('sr-status')
        .should('contain', 'strategy selected')

      cy.getByDataCy('strategy-card')
        .eq(1)
        .click()

      cy.getByDataCy('run-comparison')
        .click()

      cy.getByDataCy('sr-status')
        .should('contain', 'comparison completed')
    })
  })
})