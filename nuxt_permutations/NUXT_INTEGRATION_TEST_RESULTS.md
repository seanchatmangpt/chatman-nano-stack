# Nuxt.js Integration Test Results

**Test Run ID:** nuxt_test_1753514298  
**Timestamp:** 2025-07-26T00:18:18.561643  
**Overall Status:** FAILED  

## Test Results Summary

```mermaid
graph TD
    A[Nuxt.js Integration Tests] --> B[Total Projects: 3]
    B --> C[Overall Status: FAILED]
    
    %% Project Results
    C --> nuxt_ash_graphql[nuxt-ash-graphql<br/>Status: failed<br/>Type: GraphQL Integration]
    class nuxt_ash_graphql red
    C --> nuxt_bitactor_performance[nuxt-bitactor-performance<br/>Status: failed<br/>Type: Performance Dashboard]
    class nuxt_bitactor_performance red
    C --> nuxt_semantic_web[nuxt-semantic-web<br/>Status: failed<br/>Type: SSR/SSG Semantic Web]
    class nuxt_semantic_web red

    %% OTEL Metrics
    C --> OTEL[OTEL Metrics<br/>Success Rate: 72.22%<br/>Total Tests: 18]
    class OTEL blue

    classDef green fill:#d4edda,stroke:#155724,color:#155724
    classDef red fill:#f8d7da,stroke:#721c24,color:#721c24
    classDef blue fill:#cce5ff,stroke:#004085,color:#004085
```

## OTEL Metrics

```json
{
  "span_name": "nuxt_integration_tests",
  "trace_id": "trace_nuxt_test_1753514298",
  "start_time": "2025-07-26T00:18:18.561643",
  "end_time": "2025-07-26T00:18:18.891534",
  "attributes": {
    "test.framework": "nuxt_integration_tester",
    "test.total_projects": 3,
    "test.overall_status": "failed"
  },
  "metrics": {
    "projects_tested": 3,
    "projects_passed": 0,
    "projects_failed": 3,
    "total_tests_run": 18,
    "test_success_rate": 72.22
  },
  "events": [
    {
      "name": "project_test_completed",
      "timestamp": "2025-07-26T00:18:18.891578",
      "attributes": {
        "project.name": "nuxt-ash-graphql",
        "project.status": "failed",
        "project.type": "GraphQL Integration"
      }
    },
    {
      "name": "project_test_completed",
      "timestamp": "2025-07-26T00:18:18.891581",
      "attributes": {
        "project.name": "nuxt-bitactor-performance",
        "project.status": "failed",
        "project.type": "Performance Dashboard"
      }
    },
    {
      "name": "project_test_completed",
      "timestamp": "2025-07-26T00:18:18.891582",
      "attributes": {
        "project.name": "nuxt-semantic-web",
        "project.status": "failed",
        "project.type": "SSR/SSG Semantic Web"
      }
    }
  ]
}
```
