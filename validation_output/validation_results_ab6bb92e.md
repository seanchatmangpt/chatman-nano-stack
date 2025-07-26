# CNS Forge TTL → Ash.Reactor Validation Results

**Validation ID:** `ab6bb92e`  
**Status:** ❌ FAILED  
**Duration:** 4370ms  
**Success Rate:** 75.0%

## Validation Pipeline

```mermaid
graph TD
  Start(["🚀 Start Validation<br/>ab6bb92e"]) --> 1
  1["✅ Ttl file check<br/>1ms"]
  2["✅ Ttl parsing<br/>2205ms"]
  3["✅ Project generation<br/>2161ms"]
  4["❌ File validation<br/>0ms"]
  4["📊 Complete<br/>failed"] --> End(["🏁 End"])
  
  1 --> 2
  2 --> 3
  3 --> 4
  
  classDef passed fill:#d4edda,stroke:#28a745,stroke-width:2px
  classDef failed fill:#f8d7da,stroke:#dc3545,stroke-width:2px
  classDef process fill:#cce5ff,stroke:#007bff,stroke-width:2px
  
  class Start,End process
  class 1 passed
  class 2 passed
  class 3 passed
  class 4 failed
```

## Performance Metrics

```mermaid
pie title Validation Step Performance (ms)
    "Ttl file check" : 1
    "Ttl parsing" : 2205
    "Project generation" : 2161
    "File validation" : 0
```

## Validation Results

| Step | Status | Duration | Key Metrics |
|------|--------|----------|-------------|
| Ttl file check | ✅ passed | 1ms | 23949 bytes |
| Ttl parsing | ✅ passed | 2205ms | 58 concepts |
| Project generation | ✅ passed | 2161ms | 4 files |
| File validation | ❌ failed | 0ms | 0 files |

## System Status

❌ **SYSTEM ISSUES**: The TTL → Project Generation pipeline has problems

### Critical Path Analysis

- **TTL File Access**: ✅
- **Semantic Parsing**: ✅
- **Project Generation**: ✅
- **File Validation**: ❌

## Recommendations

- 📝 Check generated file structure and content
