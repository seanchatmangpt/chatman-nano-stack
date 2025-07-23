# 🚀 CF CLI Quick Reference Card

## Installation & Setup
```bash
uv sync                    # Install dependencies  
ollama pull qwen3:latest   # Get AI model
cf status                  # Check system status
```

## Core Commands

### 🧠 Ultra-Intelligence Design
```bash
cf ultrathink "task description"                    # Basic ultra-intelligence
cf ultrathink "task" --context "extra info"        # With context
cf ultrathink "task" --dry-run                      # See reasoning only
```

### ⚡ Implementation  
```bash
cf implement file.md                                # Basic implementation
cf implement file.md --focus functionality         # Code only
cf implement file.md --focus tests                 # Tests only  
cf implement file.md --focus both                  # Code + tests
cf implement file.md --ai-level hyper              # Maximum intelligence
```

### 🎯 Completion
```bash
cf finish file.md                                   # Merge functionality + tests
cf finish file.md --no-merge-tests                 # Complete without merging
```

### 📊 Validation & Benchmarking
```bash
cf validate file.md                                 # Basic validation
cf validate file.md --check-business-value         # Include business analysis
cf validate file.md --check-telemetry              # Include telemetry check
cf benchmark                                        # Run benchmarks
cf benchmark --report --validate-telemetry         # Full benchmark + report
```

### 🧹 Cleanup & Maintenance
```bash
cf clean                                            # Remove mocks (default)
cf clean --target mock                             # Remove mock implementations  
cf clean --target errors                           # Add crash-first testing
cf clean --target all                              # Complete cleanup
cf fix                                              # Fix claude-flow issues
```

### 🎨 Custom & Advanced
```bash
cf custom "custom prompt"                          # Custom ultra-intelligence
cf custom "prompt" --no-use-dspy                   # Skip DSPy enhancement
```

## BitActor Production Workflow

### Phase 1: Architecture
```bash
cf ultrathink "design production BitActor with Erlang/OTP" --dry-run
cf implement docs/architecture/bitactor_production.md --ai-level hyper
```

### Phase 2: Core Implementation  
```bash
cf implement bitactor_otp/mix.exs --focus functionality
cf implement bitactor_otp/lib/bitactor/supervisor.ex --focus functionality
cf implement src/cns/bitactor_nif.c --ai-level hyper
```

### Phase 3: Testing
```bash
cf implement test/bitactor_test.exs --focus tests --ai-level hyper
cf benchmark --report --validate-telemetry
```

### Phase 4: Finalization
```bash
cf finish bitactor_otp/ --merge-tests
cf validate bitactor_otp/ --check-business-value --check-telemetry
cf clean --target all
```

## DSPy Intelligence Levels

| Level | Usage | Best For |
|-------|-------|----------|
| `normal` | Simple tasks | Basic implementations |
| `high` | Complex logic | Advanced algorithms |  
| `hyper` | Production systems | Mission-critical code |

## Focus Areas

| Focus | Description | When to Use |
|-------|-------------|-------------|
| `functionality` | Implementation only | When tests exist |
| `tests` | Test generation only | When code exists |
| `both` | Code + comprehensive tests | New features |

## Status Indicators

| Symbol | Meaning |
|--------|---------|
| ✅ | Component available/working |
| ❌ | Component missing/broken |
| ⚠️ | Warning/fallback mode |
| 🧠 | DSPy reasoning active |
| 🦙 | Ollama qwen3:latest |
| 🌊 | Claude Flow |

## Troubleshooting

```bash
cf status                           # Check all components
ollama serve                        # Start ollama if needed
ollama pull qwen3:latest           # Update model
npm install -g claude-flow@alpha   # Update claude-flow
uv sync                            # Update dependencies
```

## Pro Tips

1. **Always start with `--dry-run`** to see DSPy reasoning
2. **Use `--context`** for better AI understanding
3. **Iterate**: ultrathink → implement → validate → finish
4. **Clean frequently** with `cf clean --target all`
5. **Monitor status** with `cf status` before major operations

---
*Ultra-intelligence at your fingertips. Production excellence simplified.*