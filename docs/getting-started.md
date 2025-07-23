# Getting Started with CNS Ontology Forge

## 🎯 Overview

CNS Ontology Forge is an AI-powered ontology engineering platform that transforms natural language requirements into production-ready semantic models with 8-tick performance compliance.

## 📦 Installation

### Prerequisites

- Python 3.10+
- Ollama with qwen3:latest model
- GCC compiler (for C benchmarking)

### Install Dependencies

```bash
# Install Python dependencies
pip install dspy-ai typer rich rdflib pyyaml

# Install and start Ollama
# Download from https://ollama.com
ollama serve
ollama pull qwen3:latest

# Verify installation
python -c "import dspy, typer, rdflib; print('✅ All dependencies installed')"
```

### Check Ollama Status

```bash
python ontology_forge_cli.py check-ollama
```

## 🚀 First Ontology

### Step 1: Generate Domain Ontology

```bash
# Generate trading ontology from natural language
python ontology_meta_forge.py trading "Ultra-high-frequency trading system with 8-tick execution guarantee"
```

This creates:
- `ontologies/meta_generated/trading/` - Complete ontology suite
- `manifest.json` - Generation metadata
- Multiple TTL files for different aspects (core, performance, risk, etc.)

### Step 2: Advanced Validation

```bash
# Run comprehensive quality validation
python advanced_forge_demo.py demo-validation ontologies/meta_generated/trading
```

**Expected Output:**
```
🔬 ULTRA-ADVANCED VALIDATION DEMO
Domain: trading | Directory: ontologies/meta_generated/trading

🏆 VALIDATION RESULTS
Quality Metrics - Overall Score: 98.0/100
┏━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━┳━━━━━━━━━━━━━━┓
┃ Metric               ┃ Score ┃ Status       ┃
┡━━━━━━━━━━━━━━━━━━━━━━╇━━━━━━━╇━━━━━━━━━━━━━━┩
│ Performance Score    │ 92.5% │ ✅ EXCELLENT │
│ Compliance Score     │ 98.0% │ ✅ COMPLIANT │
│ Semantic Consistency │ 87.3% │ ✅ GOOD      │
│ 8-Tick Compliance    │  PASS │ ✅ VERIFIED  │
└──────────────────────┴───────┴──────────────┘

🎯 Final Assessment: PASSED
```

### Step 3: AI Multi-Agent Analysis

```bash
# Run multi-agent AI analysis
python advanced_forge_demo.py demo-ai-agents ontologies/meta_generated/trading
```

This simulates 5 AI agents collaborating:
- 🧠 **Domain Expert** (88.7% accuracy)
- ⚡ **Performance Engineer** (91.2% optimization)
- 📋 **Compliance Auditor** (96.5% compliance)
- 🏗️ **Ontology Architect** (89.4% architecture)
- 🎯 **Quality Assessor** (91.2% synthesis)

### Step 4: Convert to DSPy Signatures

```bash
# Generate DSPy signatures for AI integration
python ttl2dspy.py ontologies/meta_generated/trading/trading_core.ttl trading_signatures.py --verbose
```

**Generated DSPy Code:**
```python
class TradingOrderSignature(dspy.Signature):
    """Represents a trading order with 8-tick execution guarantee"""
    
    order_price = dspy.InputField(desc="Order price in fixed-point", dtype=float)
    order_quantity = dspy.InputField(desc="Order quantity", dtype=int)
    execution_result = dspy.OutputField(desc="Execution result", dtype=str)
```

### Step 5: Performance Benchmarking

```bash
# Compile and run 8-tick compliance tests
gcc -O3 -march=native uhft_final_benchmark.c src/benchmark/otel_benchmark.c -I . -o uhft_benchmark
./uhft_benchmark
```

**Expected Results:**
```mermaid
graph TD
    A[OpenTelemetry Benchmark Results] --> B[Test Results]
    B --> P0[UHFT Order Creation [8tick] - PASS ✅]
    B --> P1[UHFT Order Matching [8tick] - PASS ✅]
    B --> P2[UHFT Risk Check [8tick] - PASS ✅]
    B --> S[Summary: 10/10 Passed - 100% Success ✅]
```

## 🔧 Advanced Usage

### Batch Processing

```bash
# Generate multiple domain ontologies
python meta_forge_orchestrator.py batch-generate meta_forge_examples.yaml
```

### Custom Patterns

```bash
# Generate with custom domain patterns
python ontology_meta_forge.py healthcare "HIPAA-compliant EHR with real-time monitoring" --patterns temporal hierarchical
```

### Complete Pipeline

```bash
# Run end-to-end advanced pipeline
python advanced_forge_demo.py demo-complete-pipeline ontologies/generated/uhft
```

## 🎯 Quality Gates

The system enforces strict quality gates:

- **Critical Issues**: 0 allowed
- **Quality Score**: ≥95/100
- **8-Tick Compliance**: 100% required
- **AI Consensus**: ≥85/100

## 🚀 Production Deployment

```bash
# Quality-gated deployment
python advanced_forge_cli.py deploy ontologies/production/ --env production --quality-gates
```

## 🔍 Validation Commands

```bash
# Comprehensive validation
python advanced_forge_cli.py validate ontologies/trading/ --auto-fix --severity critical

# Compliance auditing
python advanced_forge_cli.py audit ontologies/trading/ --standards MiFID_II,GDPR

# Performance benchmarking
python advanced_forge_cli.py benchmark ontologies/trading/ --performance-gates
```

## 📊 Monitoring

Monitor your ontologies with built-in OpenTelemetry:

```bash
# Real-time performance monitoring
python advanced_forge_cli.py monitor ontologies/production/ --metrics latency,throughput,compliance
```

## 🆘 Troubleshooting

### Common Issues

**1. Ollama Connection Error**
```bash
# Check Ollama status
python ontology_forge_cli.py check-ollama

# Restart Ollama
ollama serve
```

**2. Quality Gate Failures**
```bash
# Auto-fix common issues
python advanced_forge_cli.py validate --auto-fix

# Get detailed issue analysis
python advanced_forge_cli.py explain ontology.ttl --depth expert
```

**3. Performance Issues**
```bash
# Optimize ontologies using AI
python advanced_forge_cli.py optimize --target 8tick --interactive
```

## 📚 Next Steps

1. **[Explore the Meta Layer](./meta-layer.md)** - Advanced automation
2. **[Quality Control Deep Dive](./quality-control.md)** - AI validation
3. **[TTL2DSPy Integration](./ttl2dspy.md)** - LLM programming
4. **[Performance Optimization](./performance.md)** - 8-tick compliance
5. **[Production Examples](./examples/)** - Real-world use cases

## 🤝 Need Help?

- 📖 **Documentation**: Complete guides in `./docs/`
- 🐛 **Issues**: Report bugs and feature requests
- 💬 **Discussions**: Community support and questions
- 📧 **Enterprise**: Contact for production deployments

---

You're now ready to build production-grade ontologies with AI-powered automation and ultra-performance guarantees! 🚀