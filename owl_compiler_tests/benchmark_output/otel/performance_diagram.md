# OWL Compiler Performance


```mermaid
graph LR
    subgraph "OWL Compiler Performance"
        A[TTL Input] --> B[Parse & Compile]
        B --> C[C Header]
        B --> D[C Implementation]
        B --> E[JSON Metadata]
        
        B -.->|avg: 155.3ms| F[Performance]
    end
    
    subgraph "Metrics"
        F --> G[P50: 146.1ms]
        F --> H[P90: 176.1ms]
        F --> I[P99: 176.1ms]
    end
```
