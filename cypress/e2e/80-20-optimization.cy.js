// 80/20 Optimization Tests
// Tests Pareto principle implementation in swarm pipeline

describe('80/20 Optimization Tests', () => {
  beforeEach(() => {
    cy.task('initializeSwarm')
    cy.visit('/swarm-pipeline')
    cy.connectToSwarmChannel()
  })

  afterEach(() => {
    cy.disconnectSwarmChannel()
  })

  describe('⚡ Skip Optimization Strategy', () => {
    it('should identify and execute only critical stages (80/20 principle)', () => {
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'healthcare',
        complexity: 0.2,
        priority: 'critical'
      })

      // Verify only 4 critical stages are executed out of 8 total (50% = 80/20)
      cy.verifyAshReactorExecution(['typer', 'turtle', 'ash', 'k8s'])

      cy.getByDataCy('stage-count')
        .should('contain', '4 of 8 stages')

      cy.getByDataCy('optimization-score')
        .should('contain', '90%')
        .and('have.class', 'excellent')

      // Verify skipped stages are identified
      const skippedStages = ['ttl2dspy', 'bitactor', 'erlang', 'reactor']
      skippedStages.forEach(stage => {
        cy.getByDataCy(`skipped-stage-${stage}`)
          .should('be.visible')
          .and('have.class', 'skipped')
      })
    })

    it('should achieve 50% execution time reduction', () => {
      // First run linear strategy for baseline
      const startLinear = Date.now()
      cy.executeSwarmStrategy('linear', {
        domain: 'healthcare',
        complexity: 0.2
      })
      const linearTime = Date.now() - startLinear

      cy.wait(1000)

      // Then run skip optimization
      const startOptimized = Date.now()
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'healthcare',
        complexity: 0.2
      })
      const optimizedTime = Date.now() - startOptimized

      // Should be at least 40% faster (allowing for variance)
      const improvement = (linearTime - optimizedTime) / linearTime
      expect(improvement).to.be.greaterThan(0.4)

      cy.getByDataCy('time-improvement')
        .should('contain', '%')
        .and('contain', 'faster')

      cy.getByDataCy('execution-metrics')
        .within(() => {
          cy.contains('optimized')
          cy.contains('<200ms')
        })
    })

    it('should reduce resource usage by 50%', () => {
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'finance',
        complexity: 0.3
      })

      cy.request({
        method: 'GET',
        url: `${Cypress.env('REACTOR_ENDPOINT')}/performance/latest`,
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(200)
        
        const performance = response.body
        expect(performance.resource_usage.memory).to.be.lessThan(30) // <30MB vs 60MB baseline
        expect(performance.resource_usage.cpu).to.be.lessThan(20) // <20% vs 40% baseline
        
        expect(performance).to.have.property('efficiency_gain')
        expect(performance.efficiency_gain).to.be.greaterThan(0.5) // >50% efficiency
      })

      cy.verifyOptimizationMetrics({
        stageReduction: '50%',
        resourceSaving: '50%',
        efficiency: '90%'
      })
    })

    it('should maintain output quality with reduced stages', () => {
      // Execute full pipeline
      cy.executeSwarmStrategy('linear', {
        domain: 'cybersecurity',
        complexity: 0.4
      })

      cy.request({
        method: 'GET',
        url: `${Cypress.env('REACTOR_ENDPOINT')}/output/quality`,
        failOnStatusCode: false
      }).then((response) => {
        const fullPipelineQuality = response.body.quality_score
        
        // Execute optimized pipeline
        cy.executeSwarmStrategy('skip_optimization', {
          domain: 'cybersecurity',
          complexity: 0.4
        })

        cy.request({
          method: 'GET',
          url: `${Cypress.env('REACTOR_ENDPOINT')}/output/quality`,
          failOnStatusCode: false
        }).then((optimizedResponse) => {
          const optimizedQuality = optimizedResponse.body.quality_score
          
          // Quality should be at least 90% of full pipeline (acceptable 80/20 tradeoff)
          const qualityRatio = optimizedQuality / fullPipelineQuality
          expect(qualityRatio).to.be.greaterThan(0.9)
        })
      })

      cy.getByDataCy('quality-score')
        .should('contain', '%')
        .then(($el) => {
          const score = parseInt($el.text())
          expect(score).to.be.greaterThan(90)
        })
    })
  })

  describe('🎯 Critical Stage Identification', () => {
    it('should correctly identify critical stages for different domains', () => {
      const domainTests = [
        {
          domain: 'healthcare',
          expectedCritical: ['typer', 'turtle', 'ash', 'k8s'],
          expectedSkipped: ['ttl2dspy', 'bitactor', 'erlang', 'reactor']
        },
        {
          domain: 'finance',
          expectedCritical: ['typer', 'turtle', 'ash', 'reactor', 'k8s'],
          expectedSkipped: ['ttl2dspy', 'bitactor', 'erlang']
        },
        {
          domain: 'cybersecurity',
          expectedCritical: ['typer', 'turtle', 'ttl2dspy', 'ash', 'k8s'],
          expectedSkipped: ['bitactor', 'erlang', 'reactor']
        }
      ]

      domainTests.forEach(test => {
        cy.executeSwarmStrategy('skip_optimization', {
          domain: test.domain,
          complexity: 0.3
        })

        // Verify critical stages are executed
        test.expectedCritical.forEach(stage => {
          cy.getByDataCy(`critical-stage-${stage}`)
            .should('be.visible')
            .and('have.class', 'critical')
        })

        // Verify non-critical stages are skipped
        test.expectedSkipped.forEach(stage => {
          cy.getByDataCy(`skipped-stage-${stage}`)
            .should('be.visible')
            .and('have.class', 'skipped')
        })

        cy.getByDataCy('domain-optimization')
          .should('contain', test.domain)
          .and('contain', `${test.expectedCritical.length} critical`)
      })
    })

    it('should adapt critical stages based on complexity', () => {
      // Low complexity - minimal stages
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'healthcare',
        complexity: 0.1
      })

      cy.getByDataCy('executed-stages')
        .children()
        .should('have.length', 3) // Ultra-minimal for very low complexity

      // Medium complexity - standard critical stages
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'healthcare',
        complexity: 0.5
      })

      cy.getByDataCy('executed-stages')
        .children()
        .should('have.length', 4) // Standard critical stages

      // High complexity - expanded critical set
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'healthcare',
        complexity: 0.9
      })

      cy.getByDataCy('executed-stages')
        .children()
        .should('have.length.greaterThan', 4) // Expanded for high complexity
    })

    it('should provide rationale for stage selection', () => {
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'finance',
        complexity: 0.4
      })

      cy.getByDataCy('optimization-rationale')
        .should('be.visible')

      cy.getByDataCy('critical-rationale')
        .should('contain', 'high impact')
        .and('contain', 'essential')

      cy.getByDataCy('skipped-rationale')
        .should('contain', 'low impact')
        .and('contain', 'optional')

      // Each critical stage should have explanation
      cy.get('[data-cy^="stage-rationale-"]')
        .should('have.length.greaterThan', 3)
        .each(($el) => {
          cy.wrap($el).should('contain.text', 'because')
        })
    })
  })

  describe('📊 Pareto Analysis', () => {
    it('should generate Pareto charts showing 80/20 distribution', () => {
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'cybersecurity',
        complexity: 0.6
      })

      cy.getByDataCy('pareto-chart')
        .should('be.visible')

      cy.getByDataCy('pareto-bars')
        .children()
        .should('have.length', 8) // All 8 stages represented

      // Verify 80/20 visualization
      cy.getByDataCy('pareto-80-line')
        .should('be.visible')
        .and('have.css', 'stroke', 'rgb(255, 136, 0)') // Orange line at 80%

      cy.getByDataCy('cumulative-percentage')
        .should('contain', '80%')

      cy.getByDataCy('critical-stages-percentage')
        .should('contain', '20%')
        .or('contain', '50%') // 4 out of 8 stages = 50%
    })

    it('should show impact analysis for each stage', () => {
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'healthcare',
        complexity: 0.4
      })

      const criticalStages = ['typer', 'turtle', 'ash', 'k8s']
      
      criticalStages.forEach(stage => {
        cy.getByDataCy(`impact-analysis-${stage}`)
          .should('be.visible')
          .within(() => {
            cy.getByDataCy('impact-score')
              .should('contain', '%')
              .then(($el) => {
                const score = parseInt($el.text())
                expect(score).to.be.greaterThan(70) // High impact stages
              })
            
            cy.getByDataCy('effort-score')
              .should('contain', '%')
            
            cy.getByDataCy('roi-score')
              .should('contain', '%')
          })
      })
    })

    it('should calculate optimization ROI', () => {
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'finance',
        complexity: 0.5
      })

      cy.getByDataCy('optimization-roi')
        .should('be.visible')
        .within(() => {
          cy.getByDataCy('time-saved')
            .should('contain', 'ms saved')
          
          cy.getByDataCy('resources-saved')
            .should('contain', 'MB saved')
          
          cy.getByDataCy('efficiency-gain')
            .should('contain', '%')
            .then(($el) => {
              const gain = parseInt($el.text())
              expect(gain).to.be.greaterThan(40) // Significant efficiency gain
            })
          
          cy.getByDataCy('overall-roi')
            .should('contain', 'x')
            .then(($el) => {
              const roi = parseFloat($el.text())
              expect(roi).to.be.greaterThan(2) // At least 2x ROI
            })
        })
    })
  })

  describe('🔄 Adaptive Optimization', () => {
    it('should learn from execution patterns to improve 80/20 selection', () => {
      // Execute multiple times to build pattern data
      const executions = [
        { domain: 'healthcare', complexity: 0.3 },
        { domain: 'healthcare', complexity: 0.4 },
        { domain: 'healthcare', complexity: 0.5 }
      ]

      executions.forEach((config, index) => {
        cy.executeSwarmStrategy('skip_optimization', config)
        
        cy.request({
          method: 'GET',
          url: `${Cypress.env('REACTOR_ENDPOINT')}/learning/patterns`,
          failOnStatusCode: false
        }).then((response) => {
          expect(response.status).to.eq(200)
          expect(response.body).to.have.property('pattern_count')
          expect(response.body.pattern_count).to.eq(index + 1)
        })
      })

      // Next execution should use learned patterns
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'healthcare',
        complexity: 0.4,
        use_learning: true
      })

      cy.getByDataCy('learning-applied')
        .should('be.visible')
        .and('contain', 'pattern optimization')

      cy.getByDataCy('learned-efficiency')
        .should('contain', '%')
        .then(($el) => {
          const efficiency = parseInt($el.text())
          expect(efficiency).to.be.greaterThan(90) // Improved through learning
        })
    })

    it('should adjust optimization based on domain feedback', () => {
      // Execute with feedback simulation
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'cybersecurity',
        complexity: 0.6,
        feedback: {
          ttl2dspy: 'highly_valuable', // Suggest this should be included
          bitactor: 'low_value' // Confirm this can be skipped
        }
      })

      cy.request({
        method: 'GET',
        url: `${Cypress.env('REACTOR_ENDPOINT')}/optimization/adaptation`,
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(200)
        expect(response.body).to.have.property('adaptations')
        
        const adaptations = response.body.adaptations
        expect(adaptations.some(a => a.stage === 'ttl2dspy' && a.action === 'include')).to.be.true
        expect(adaptations.some(a => a.stage === 'bitactor' && a.action === 'exclude')).to.be.true
      })

      // Next execution should reflect adaptation
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'cybersecurity',
        complexity: 0.6
      })

      cy.getByDataCy('executed-stages')
        .should('contain', 'ttl2dspy') // Now included based on feedback

      cy.getByDataCy('skipped-stages')
        .should('contain', 'bitactor') // Still skipped
    })
  })

  describe('🎛️ Manual Optimization Controls', () => {
    it('should allow manual override of 80/20 selection', () => {
      cy.visit('/swarm-pipeline/optimization')
      
      cy.getByDataCy('manual-optimization')
        .should('be.visible')

      // Toggle individual stages
      cy.getByDataCy('stage-toggle-ttl2dspy')
        .should('not.be.checked')
        .check()

      cy.getByDataCy('stage-toggle-reactor')
        .should('not.be.checked')
        .check()

      cy.getByDataCy('apply-manual-selection')
        .click()

      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'healthcare',
        complexity: 0.4,
        manual_override: true
      })

      // Verify manual selection is respected
      cy.getByDataCy('executed-stages')
        .should('contain', 'ttl2dspy')
        .and('contain', 'reactor')

      cy.getByDataCy('optimization-mode')
        .should('contain', 'manual override')
    })

    it('should provide optimization suggestions', () => {
      cy.visit('/swarm-pipeline/optimization')

      cy.getByDataCy('get-suggestions')
        .click()

      cy.getByDataCy('optimization-suggestions')
        .should('be.visible')
        .children()
        .should('have.length.greaterThan', 0)

      cy.getByDataCy('suggestion-item')
        .first()
        .within(() => {
          cy.getByDataCy('suggestion-text')
            .should('contain', 'optimization')
          
          cy.getByDataCy('impact-estimate')
            .should('contain', '%')
          
          cy.getByDataCy('apply-suggestion')
            .click()
        })

      cy.getByDataCy('suggestion-applied')
        .should('be.visible')
        .and('contain', 'optimization applied')
    })

    it('should allow custom 80/20 thresholds', () => {
      cy.visit('/swarm-pipeline/optimization')

      cy.getByDataCy('pareto-threshold')
        .clear()
        .type('70')

      cy.getByDataCy('apply-threshold')
        .click()

      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'finance',
        complexity: 0.5,
        custom_threshold: 70
      })

      // Should use 70/30 instead of 80/20
      cy.getByDataCy('optimization-threshold')
        .should('contain', '70/30')

      cy.getByDataCy('executed-stages')
        .children()
        .should('have.length.greaterThan', 4) // More stages for 70/30
    })
  })

  describe('📈 Optimization Metrics and Reporting', () => {
    it('should track optimization metrics over time', () => {
      // Execute multiple optimizations
      const configs = [
        { domain: 'healthcare', complexity: 0.3 },
        { domain: 'finance', complexity: 0.5 },
        { domain: 'cybersecurity', complexity: 0.7 }
      ]

      configs.forEach(config => {
        cy.executeSwarmStrategy('skip_optimization', config)
      })

      cy.request({
        method: 'GET',
        url: `${Cypress.env('REACTOR_ENDPOINT')}/metrics/optimization_history`,
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.eq(200)
        expect(response.body).to.have.property('executions')
        expect(response.body.executions).to.have.length(3)
        
        response.body.executions.forEach(exec => {
          expect(exec).to.have.property('optimization_score')
          expect(exec).to.have.property('time_saved')
          expect(exec).to.have.property('resources_saved')
          expect(exec.optimization_score).to.be.greaterThan(0.8)
        })
      })

      cy.getByDataCy('optimization-history')
        .should('be.visible')
        .and('contain', '3 optimizations')
    })

    it('should generate optimization reports', () => {
      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'cybersecurity',
        complexity: 0.6
      })

      cy.getByDataCy('generate-report')
        .click()

      cy.getByDataCy('optimization-report')
        .should('be.visible')
        .within(() => {
          cy.contains('80/20 Analysis Report')
          cy.contains('Executive Summary')
          cy.contains('Critical Stages Identified')
          cy.contains('Performance Improvements')
          cy.contains('Resource Savings')
          cy.contains('Recommendations')
        })

      cy.getByDataCy('export-report')
        .click()

      // Verify download initiated
      cy.readFile('cypress/downloads/optimization-report.pdf', { timeout: 10000 })
        .should('exist')
    })

    it('should show real-time optimization dashboard', () => {
      cy.visit('/swarm-pipeline/dashboard')

      cy.getByDataCy('optimization-dashboard')
        .should('be.visible')

      cy.executeSwarmStrategy('skip_optimization', {
        domain: 'healthcare',
        complexity: 0.4
      })

      // Dashboard should update in real-time
      cy.getByDataCy('live-optimization-metrics')
        .should('be.visible')
        .within(() => {
          cy.getByDataCy('current-efficiency')
            .should('contain', '%')
          
          cy.getByDataCy('stages-executing')
            .should('contain', '4')
          
          cy.getByDataCy('time-remaining')
            .should('contain', 'ms')
          
          cy.getByDataCy('optimization-score')
            .should('contain', '%')
        })

      cy.getByDataCy('optimization-chart')
        .should('be.visible')
        .and('have.css', 'animation-play-state', 'running')
    })
  })
})