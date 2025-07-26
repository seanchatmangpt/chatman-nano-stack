#!/usr/bin/env node

/**
 * BitActor Nuxt Variants 80/20 Validation Script
 * Validates all variant files without requiring a running server
 * Focuses on the 20% of tests that cover 80% of critical functionality
 */

const fs = require('fs')
const path = require('path')

// TTL constraints for validation
const TTL_CONSTRAINTS = {
  global: 8000, // 8 seconds
  stages: {
    typer: 1000,
    turtle: 1000, 
    ttl2dspy: 1000,
    bitactor: 1500,
    erlang: 1000,
    ash: 1200,
    reactor: 800,
    k8s: 500
  }
}

// Variant files to validate
const VARIANT_FILES = [
  // Original Nuxt variants
  'nuxt_cybersecurity_components_variant.vue',
  'nuxt_cybersecurity_layouts_variant.vue',
  'nuxt_pipeline_monitoring_pages_variant.vue', 
  'nuxt_ttl_metrics_components_variant.vue',
  'nuxt_ssr_variant_ttl_aware.js',
  'nuxt_websocket_variant_realtime.js',
  'nuxt_bitactor_hybrid_bridge_variant.js',
  
  // Nuxt UI variants
  'nuxt_ui_dashboard_variant.vue',
  'nuxt_ui_pipeline_visualizer_variant.vue',
  'nuxt_ui_swarm_management_variant.vue',
  'nuxt_ui_ttl_metrics_dashboard_variant.vue',
  'nuxt_ui_security_monitoring_variant.vue',
  'nuxt_ui_interactive_pipeline_builder_variant.vue',
  'nuxt_ui_bitactor_config_variant.vue'
]

class BitActorValidator {
  constructor() {
    this.results = {
      passed: 0,
      failed: 0,
      violations: [],
      metrics: {},
      startTime: Date.now()
    }
  }

  log(message, level = 'INFO') {
    const timestamp = new Date().toISOString()
    const prefix = level === 'ERROR' ? '❌' : level === 'WARN' ? '⚠️' : '✅'
    console.log(`${prefix} [${timestamp}] ${message}`)
  }

  validateFileExists(filePath) {
    const fullPath = path.join(__dirname, filePath)
    return fs.existsSync(fullPath)
  }

  async readFileWithTTL(filePath, ttlBudget) {
    const startTime = Date.now()
    
    try {
      const content = fs.readFileSync(filePath, 'utf8')
      const duration = Date.now() - startTime
      
      if (duration > ttlBudget) {
        this.results.violations.push({
          file: filePath,
          violation: 'TTL_EXCEEDED',
          duration,
          budget: ttlBudget
        })
        this.log(`TTL violation: ${path.basename(filePath)} took ${duration}ms > ${ttlBudget}ms`, 'ERROR')
        return null
      }
      
      this.results.metrics[filePath] = { duration, budget: ttlBudget, compliance: true }
      return content
    } catch (error) {
      this.log(`File read error: ${path.basename(filePath)} - ${error.message}`, 'ERROR')
      return null
    }
  }

  validateVueComponent(content, filePath) {
    const checks = {
      hasTemplate: content.includes('<template>'),
      hasScript: content.includes('<script>'),
      hasStyle: content.includes('<style'),
      hasExport: content.includes('export default'),
      hasName: /name:\s*['"`][^'"`]+['"`]/.test(content),
      isScoped: content.includes('scoped')
    }
    
    const passed = Object.values(checks).filter(Boolean).length
    const total = Object.keys(checks).length
    const score = (passed / total) * 100
    
    if (score < 80) {
      this.results.violations.push({
        file: filePath,
        violation: 'VUE_STRUCTURE',
        score,
        checks
      })
      this.log(`Vue structure: ${path.basename(filePath)} scored ${score.toFixed(1)}%`, 'WARN')
    } else {
      this.log(`Vue structure: ${path.basename(filePath)} scored ${score.toFixed(1)}%`)
    }
    
    return score >= 80
  }

  validateJavaScript(content, filePath) {
    const checks = {
      hasExport: content.includes('export') || content.includes('module.exports'),
      hasErrorHandling: /try\s*\{[\s\S]*catch|\.catch\(|throw\s+|error|Error/.test(content),
      noTypeScript: !content.includes('typescript') && !content.includes('interface ') && !content.includes('type '),
      hasAsyncPatterns: /async|await|Promise|\.then\(/.test(content)
    }
    
    const passed = Object.values(checks).filter(Boolean).length
    const total = Object.keys(checks).length
    const score = (passed / total) * 100
    
    if (score < 75) {
      this.results.violations.push({
        file: filePath,
        violation: 'JS_STRUCTURE',
        score,
        checks
      })
      this.log(`JS structure: ${path.basename(filePath)} scored ${score.toFixed(1)}%`, 'WARN')
    } else {
      this.log(`JS structure: ${path.basename(filePath)} scored ${score.toFixed(1)}%`)
    }
    
    return score >= 75
  }

  validatePipelineIntegration(content, filePath) {
    const pipelineStages = ['typer', 'turtle', 'ttl2dspy', 'bitactor', 'erlang', 'ash', 'reactor', 'k8s']
    let stageCount = 0
    
    pipelineStages.forEach(stage => {
      if (content.toLowerCase().includes(stage)) {
        stageCount++
      }
    })
    
    const coverage = (stageCount / pipelineStages.length) * 100
    
    if (coverage < 50) {
      this.results.violations.push({
        file: filePath,
        violation: 'PIPELINE_INTEGRATION',
        coverage,
        stagesFound: stageCount
      })
      this.log(`Pipeline integration: ${path.basename(filePath)} has ${coverage.toFixed(1)}% stage coverage`, 'WARN')
    } else {
      this.log(`Pipeline integration: ${path.basename(filePath)} has ${coverage.toFixed(1)}% stage coverage`)
    }
    
    return coverage >= 50
  }

  validateTTLAwareness(content, filePath) {
    const ttlPatterns = [
      /ttl.*budget/i,
      /time.*constraint/i,
      /timeout|duration/i,
      /nanosecond|millisecond/i,
      /constraint.*enforcement/i
    ]
    
    let ttlScore = 0
    ttlPatterns.forEach(pattern => {
      if (pattern.test(content)) {
        ttlScore++
      }
    })
    
    const awareness = (ttlScore / ttlPatterns.length) * 100
    
    if (awareness < 20) {
      this.results.violations.push({
        file: filePath,
        violation: 'TTL_AWARENESS',
        awareness,
        patternsFound: ttlScore
      })
      this.log(`TTL awareness: ${path.basename(filePath)} has ${awareness.toFixed(1)}% TTL pattern coverage`, 'WARN')
    } else {
      this.log(`TTL awareness: ${path.basename(filePath)} has ${awareness.toFixed(1)}% TTL pattern coverage`)
    }
    
    return awareness >= 20
  }

  async validateFile(filePath) {
    this.log(`Validating ${path.basename(filePath)}...`)
    
    if (!this.validateFileExists(filePath)) {
      this.results.failed++
      this.results.violations.push({
        file: filePath,
        violation: 'FILE_NOT_FOUND'
      })
      this.log(`File not found: ${filePath}`, 'ERROR')
      return false
    }
    
    const ttlBudget = filePath.includes('ui_') ? 2000 : 1000 // UI variants get higher budget
    const content = await this.readFileWithTTL(filePath, ttlBudget)
    
    if (!content) {
      this.results.failed++
      return false
    }
    
    let validationsPassed = 0
    let totalValidations = 0
    
    // Validate based on file type
    if (filePath.endsWith('.vue')) {
      totalValidations++
      if (this.validateVueComponent(content, filePath)) {
        validationsPassed++
      }
    } else if (filePath.endsWith('.js')) {
      totalValidations++
      if (this.validateJavaScript(content, filePath)) {
        validationsPassed++
      }
    }
    
    // Common validations
    totalValidations += 2
    if (this.validatePipelineIntegration(content, filePath)) {
      validationsPassed++
    }
    if (this.validateTTLAwareness(content, filePath)) {
      validationsPassed++
    }
    
    const success = validationsPassed >= Math.ceil(totalValidations * 0.8) // 80% pass rate
    
    if (success) {
      this.results.passed++
      this.log(`${path.basename(filePath)} passed validation (${validationsPassed}/${totalValidations})`)
    } else {
      this.results.failed++
      this.log(`${path.basename(filePath)} failed validation (${validationsPassed}/${totalValidations})`, 'ERROR')
    }
    
    return success
  }

  async runValidation() {
    this.log('Starting BitActor Nuxt Variants 80/20 Validation...')
    this.log(`Validating ${VARIANT_FILES.length} variant files`)
    
    for (const filePath of VARIANT_FILES) {
      await this.validateFile(filePath)
    }
    
    this.generateReport()
  }

  generateReport() {
    const duration = Date.now() - this.results.startTime
    const totalFiles = this.results.passed + this.results.failed
    const successRate = (this.results.passed / totalFiles) * 100
    
    console.log('\n' + '='.repeat(80))
    console.log('BitActor Nuxt Variants 80/20 Validation Report')
    console.log('='.repeat(80))
    console.log(`Total Files: ${totalFiles}`)
    console.log(`Passed: ${this.results.passed}`)
    console.log(`Failed: ${this.results.failed}`)
    console.log(`Success Rate: ${successRate.toFixed(1)}%`)
    console.log(`Total Duration: ${duration}ms`)
    console.log(`TTL Compliance: ${duration < TTL_CONSTRAINTS.global ? '✅ PASS' : '❌ FAIL'}`)
    
    if (this.results.violations.length > 0) {
      console.log('\nViolations:')
      this.results.violations.forEach(violation => {
        console.log(`  ❌ ${path.basename(violation.file)}: ${violation.violation}`)
      })
    }
    
    // TTL Metrics
    console.log('\nTTL Performance Metrics:')
    Object.entries(this.results.metrics).forEach(([file, metrics]) => {
      const efficiency = (metrics.duration / metrics.budget) * 100
      const status = efficiency > 100 ? '❌' : efficiency > 80 ? '⚠️' : '✅'
      console.log(`  ${status} ${path.basename(file)}: ${metrics.duration}ms (${efficiency.toFixed(1)}%)`)
    })
    
    console.log('='.repeat(80))
    
    // Exit with appropriate code
    const exitCode = successRate >= 80 && duration < TTL_CONSTRAINTS.global ? 0 : 1
    process.exit(exitCode)
  }
}

// Run validation
const validator = new BitActorValidator()
validator.runValidation().catch(error => {
  console.error('❌ Validation failed:', error.message)
  process.exit(1)
})