# CNS v8.0 Data Flow Architecture

## Overview

This document describes how data flows through the CNS v8.0 system, from semantic specifications through AOT compilation to runtime execution. The system implements a declarative-to-imperative transformation pipeline with real-time performance monitoring and feedback loops.

## Data Flow Layers

```
┌─────────────────────────────────────────────────────────────────┐
│                    CNS v8.0 Data Flow Layers                   │
├─────────────────────────────────────────────────────────────────┤
│ Specification Data Flow                                         │
│ TTL/SHACL/SPARQL → RDF Graph → Semantic Analysis → IR          │
├─────────────────────────────────────────────────────────────────┤
│ Compilation Data Flow                                           │
│ IR → Code Templates → Generated C → Native Binary              │
├─────────────────────────────────────────────────────────────────┤
│ Runtime Data Flow                                               │
│ Input Events → BitActor Processing → RingBus → Output          │
├─────────────────────────────────────────────────────────────────┤
│ Monitoring Data Flow                                            │
│ Telemetry → Health Analysis → Self-Healing → Performance       │
└─────────────────────────────────────────────────────────────────┘
```

## Specification Data Flow

### Input Specifications

**OWL Ontologies** (`/ontologies/generated/uhft/`)
```turtle
# uhft_core.ttl
:Order a owl:Class ;
    rdfs:label "Trading Order" ;
    rdfs:comment "8-tick execution guarantee" ;
    rdfs:subClassOf cns:BitActor .

:orderPrice a owl:DatatypeProperty ;
    rdfs:domain :Order ;
    rdfs:range xsd:decimal ;
    rdfs:comment "Fixed-point price representation" .
```

**SHACL Constraints** (`/ir/shacl-governance.ttl`)
```turtle
# Trading order validation shape
:OrderShape a sh:NodeShape ;
    sh:targetClass :Order ;
    sh:property [
        sh:path :orderPrice ;
        sh:minInclusive 0.01 ;
        sh:maxInclusive 999999.99 ;
        sh:datatype xsd:decimal ;
    ] .
```

**SPARQL Queries** (`/queries/`)
```sparql
# Market access query
PREFIX cap: <http://chatman.ai/capability#>
SELECT ?actor ?market
WHERE {
    ?actor cap:hasCapability ?cap .
    ?cap cap:type cap:MarketDataAccess .
    ?cap cap:market ?market .
    FILTER(?expiry > NOW())
}
```

### Semantic Processing Pipeline

**Stage 1: Parsing and Graph Construction**
```python
# owl_compiler.py
def parse_ontology(ttl_file: Path) -> Graph:
    graph = Graph()
    graph.parse(ttl_file, format='turtle')
    return graph

# Data Structure
RDFGraph = {
    'triples': [(subject, predicate, object), ...],
    'namespaces': {'cns': 'http://cns.io/ontology#', ...},
    'classes': [OWLClass(...), ...],
    'properties': [OWLProperty(...), ...]
}
```

**Stage 2: Semantic Analysis**
```python
# Extract class hierarchy and relationships
class_hierarchy = {
    ':Order': {
        'parents': ['cns:BitActor'],
        'properties': [':orderPrice', ':orderQuantity'],
        'constraints': ['price_range', 'quantity_positive']
    }
}

# Property analysis
property_analysis = {
    ':orderPrice': {
        'domain': [':Order'],
        'range': ['xsd:decimal'],
        'constraints': {'min': 0.01, 'max': 999999.99}
    }
}
```

**Stage 3: Intermediate Representation (IR)**
```json
{
    "ontology_metadata": {
        "version": "1.0.0",
        "timestamp": "2025-07-22T23:58:30",
        "total_classes": 4,
        "total_properties": 4
    },
    "classes": [
        {
            "uri": "http://cns.io/uhft#Order",
            "name": "Order",
            "parent": "BitActor_t",
            "properties": [
                {
                    "name": "orderPrice",
                    "type": "decimal_t",
                    "constraints": {"min": 0.01, "max": 999999.99}
                }
            ]
        }
    ]
}
```

## Compilation Data Flow

### Template-Driven Code Generation

**Input: IR + Templates**
```jinja2
{# header.c.j2 template #}
typedef struct {
    BitActor_t base;
    {% for prop in class.properties %}
    {{ prop.c_type }} {{ prop.name }};
    {% endfor %}
} {{ class.name }}_t;

{% for prop in class.properties %}
bool {{ class.name|lower }}_validate_{{ prop.name }}({{ class.name }}_t* self);
{% endfor %}
```

**Generated C Header**
```c
// Generated from uhft_core.ttl
typedef struct {
    BitActor_t base;
    decimal_t orderPrice;
    uint32_t orderQuantity;
    uint64_t timestamp_tsc;
} Order_t;

bool order_validate_orderPrice(Order_t* self);
bool order_validate_orderQuantity(Order_t* self);
```

**Generated C Implementation**
```c
// Generated validation from SHACL constraints
bool order_validate_orderPrice(Order_t* self) {
    // Bit-optimized constraint checking
    uint64_t price_bits = decimal_to_bits(self->orderPrice);
    uint64_t violations = 0;
    
    violations |= (price_bits < MIN_PRICE_BITS) << 0;
    violations |= (price_bits > MAX_PRICE_BITS) << 1;
    
    return violations == 0;
}
```

### Compilation Pipeline Data Flow

**Stage 1: C Code Generation**
```python
# aot_lifecycle.py compilation metrics
CompilationTarget = {
    'name': 'uhft_trading',
    'architecture': 'x86_64',
    'optimization_level': 'O3',
    'generated_files': [
        'uhft_core.h',
        'uhft_core.c', 
        'uhft_core.json'
    ]
}
```

**Stage 2: Native Compilation**
```bash
# Makefile compilation with performance flags
CC = clang
CFLAGS = -O3 -Wall -march=native -falign-functions=64
LDFLAGS = -lm -pthread

# Generated compilation commands
clang -O3 -march=native generated_c/uhft_core/uhft_core.c -o uhft_trading
```

**Stage 3: Binary Validation**
```c
// Gatekeeper validation of compiled binary
typedef struct {
    double cpk_requirement;      // >= 1.3
    uint32_t max_ticks;         // <= 8
    uint64_t max_memory_usage;  // Memory constraints
} QualityGate_t;
```

## Runtime Data Flow

### Event Processing Pipeline

**Input Event Structure**
```c
typedef struct {
    uint64_t event_id;          // Unique event identifier
    uint64_t timestamp_tsc;     // High-resolution timestamp
    uint32_t event_type;        // Application-defined type
    uint32_t payload_size;      // Payload size in bytes
    uint8_t payload[];          // Variable-length payload
} InputEvent_t;
```

**BitActor Processing Flow**
```c
// Generated from SPARQL query compilation
int process_market_event(BitActor_t* actor, InputEvent_t* event) {
    BIT_ACTOR_START_TIMING(actor);
    
    // Stage 1: Input validation (generated from SHACL)
    if (!validate_market_event(event)) {
        BIT_ACTOR_END_TIMING(actor);
        return -1;
    }
    
    // Stage 2: Business logic (generated from OWL)
    MarketData_t* market_data = extract_market_data(event);
    Order_t* orders = find_matching_orders(market_data);
    
    // Stage 3: Constraint checking (generated from SHACL)
    for (int i = 0; i < order_count; i++) {
        if (!order_validate(&orders[i])) {
            continue; // Skip invalid orders
        }
        
        // Stage 4: Query execution (generated from SPARQL)
        execute_trading_query(&orders[i], market_data);
    }
    
    BIT_ACTOR_END_TIMING(actor);
    return 0;
}
```

### Inter-Component Communication

**RingBus Message Flow**
```c
// Message creation and sending
RingBusMessage_t message = {
    .message_id = generate_message_id(),
    .sender_id = actor->actor_id,
    .receiver_id = target_actor_id,
    .timestamp_tsc = __builtin_ia32_rdtsc(),
    .priority = RING_BUS_PRIORITY_HIGH,
    .payload_size = sizeof(Order_t)
};

// Copy order data to message payload
memcpy(message.payload, &order, sizeof(Order_t));

// Send with performance timing
RING_BUS_START_TIMING();
int result = ring_bus_send(message_bus, &message);
RING_BUS_END_TIMING(message_bus, "order_send");
```

**Lock-Free Queue Operations**
```c
// Producer side (lock-free enqueue)
static int ring_bus_enqueue(RingBus_t* bus, RingBusMessage_t* msg) {
    uint64_t current_tail = atomic_load(&bus->producer_cursor);
    uint64_t next_tail = (current_tail + 1) & bus->capacity_mask;
    
    // Check if queue is full
    if (next_tail == atomic_load(&bus->consumer_cursor)) {
        atomic_fetch_add(&bus->stats.messages_dropped, 1);
        return -1; // Queue full
    }
    
    // Copy message to slot
    bus->slots[current_tail].message = *msg;
    
    // Publish the message
    atomic_store(&bus->slots[current_tail].sequence, current_tail);
    atomic_store(&bus->producer_cursor, next_tail);
    
    atomic_fetch_add(&bus->stats.messages_sent, 1);
    return 0;
}
```

### Output Generation

**Result Aggregation**
```c
typedef struct {
    uint64_t query_id;          // Query identifier
    uint32_t result_count;      // Number of results
    uint64_t execution_ticks;   // Execution time
    ResultBinding bindings[];   // Variable bindings
} QueryResult_t;

// Generated result processing
int aggregate_query_results(QueryResult_t* results, int count) {
    ResultSet_t aggregate = {0};
    
    for (int i = 0; i < count; i++) {
        merge_result_bindings(&aggregate, &results[i]);
    }
    
    return serialize_results(&aggregate);
}
```

## Monitoring Data Flow

### Telemetry Collection

**Performance Metrics Flow**
```c
// Real-time metric collection
typedef struct {
    atomic_uint_fast64_t tick_count;
    atomic_uint_fast32_t operation_count;
    atomic_uint_fast32_t violation_count;
    double cpk_current;
} TelemetryData_t;

// Metric collection macro
#define COLLECT_TELEMETRY(actor, operation) \
    do { \
        uint64_t start_tsc = __builtin_ia32_rdtsc(); \
        operation; \
        uint64_t end_tsc = __builtin_ia32_rdtsc(); \
        atomic_fetch_add(&actor->metrics.tick_count, end_tsc - start_tsc); \
        atomic_fetch_add(&actor->metrics.operation_count, 1); \
    } while(0)
```

**Health Monitoring Pipeline**
```c
// Continuous health assessment
typedef struct {
    double current_cpk;         // Process capability
    uint32_t violation_rate;    // Violations per million
    uint64_t average_latency;   // Average tick count
    bool system_healthy;        // Overall health status
} HealthMetrics_t;

// Health calculation
double calculate_system_cpk(BitActor_t* actors[], int count) {
    double total_cpk = 0.0;
    int healthy_count = 0;
    
    for (int i = 0; i < count; i++) {
        if (bit_actor_is_healthy(actors[i])) {
            total_cpk += bit_actor_get_cpk(actors[i]);
            healthy_count++;
        }
    }
    
    return healthy_count > 0 ? total_cpk / healthy_count : 0.0;
}
```

### Self-Healing Data Flow

**Anomaly Detection**
```c
// Statistical anomaly detection
typedef struct {
    double control_limits[2];   // Upper and lower control limits
    double running_mean;        // Running average
    double running_variance;    // Running variance
    uint32_t sample_count;      // Number of samples
} ControlChart_t;

bool detect_performance_anomaly(ControlChart_t* chart, double new_value) {
    // Update running statistics
    update_control_chart(chart, new_value);
    
    // Check control limits
    return (new_value < chart->control_limits[0] || 
            new_value > chart->control_limits[1]);
}
```

**Healing Action Flow**
```c
// Automated healing response
typedef struct {
    HealingStrategy_t strategy;     // Healing approach
    uint32_t success_rate;          // Historical success rate
    uint64_t average_recovery_time; // Recovery time statistics
} HealingAction_t;

int trigger_self_healing(BitActor_t* actor, PerformanceAnomaly_t* anomaly) {
    HealingAction_t* action = select_healing_strategy(anomaly);
    
    uint64_t start_time = __builtin_ia32_rdtsc();
    int result = execute_healing_action(actor, action);
    uint64_t recovery_time = __builtin_ia32_rdtsc() - start_time;
    
    // Update healing statistics
    update_healing_metrics(action, result, recovery_time);
    
    return result;
}
```

## Data Transformation Stages

### Semantic → Imperative Transformation

**Input (Declarative)**:
```turtle
# Semantic specification
:Order rdfs:subClassOf cns:BitActor ;
    :hasProperty :orderPrice ;
    :constraint [ sh:minInclusive 0.01 ] .
```

**Output (Imperative)**:
```c
// Generated imperative code
typedef struct {
    BitActor_t base;
    decimal_t orderPrice;
} Order_t;

static inline bool validate_order_price(Order_t* order) {
    return decimal_to_double(order->orderPrice) >= 0.01;
}
```

### Constraint → Bit-Pattern Transformation

**Input (SHACL Constraint)**:
```turtle
sh:property [
    sh:path :orderPrice ;
    sh:minInclusive 0.01 ;
    sh:maxInclusive 999999.99 ;
    sh:datatype xsd:decimal ;
] .
```

**Output (Bit Operations)**:
```c
// Optimized bit-level validation
#define MIN_PRICE_BITS 0x0000000000000001ULL  // 0.01 in fixed-point
#define MAX_PRICE_BITS 0x000F423FFFFFFFFULL  // 999999.99 in fixed-point

static inline bool validate_price_range(uint64_t price_bits) {
    uint64_t violations = 0;
    violations |= (price_bits < MIN_PRICE_BITS) << 0;
    violations |= (price_bits > MAX_PRICE_BITS) << 1;
    return violations == 0;
}
```

### Query → Execution Plan Transformation

**Input (SPARQL Query)**:
```sparql
SELECT ?actor ?market
WHERE {
    ?actor cap:hasCapability ?cap .
    ?cap cap:type cap:MarketDataAccess .
    ?cap cap:market ?market .
}
```

**Output (Execution Chain)**:
```c
// Generated execution plan
typedef struct {
    uint32_t step_count;
    ExecutionStep steps[MAX_STEPS];
    BitActor_t* actors[MAX_ACTORS];
} QueryExecutionPlan_t;

int execute_market_access_query(QueryExecutionPlan_t* plan) {
    // Step 1: Find actors with capabilities
    BitActor_t** actors = find_actors_with_capability();
    
    // Step 2: Filter by market data access
    filter_by_capability_type(actors, CAP_MARKET_DATA_ACCESS);
    
    // Step 3: Extract market associations
    return extract_market_bindings(actors);
}
```

## Performance Optimization Data Flow

### Cache-Aware Data Layout

**Memory Layout Optimization**:
```c
// Cache-line aligned structures
struct alignas(64) CacheOptimizedOrder {
    // Hot path data (frequently accessed)
    uint64_t order_id;          // 8 bytes
    decimal_t price;            // 8 bytes  
    uint32_t quantity;          // 4 bytes
    uint32_t flags;             // 4 bytes
    uint64_t timestamp_tsc;     // 8 bytes
    uint64_t padding1;          // 8 bytes (total: 40 bytes)
    
    // Cold path data (less frequently accessed)
    char symbol[24];            // 24 bytes (total: 64 bytes = 1 cache line)
};
```

### Branch Prediction Optimization

**Profile-Guided Optimization**:
```c
// Likely/unlikely annotations for branch prediction
static inline bool validate_order_fast(Order_t* order) {
    // Most orders are valid, so optimize for the common case
    if (__builtin_expect(order->price > 0 && order->quantity > 0, 1)) {
        return validate_advanced_constraints(order);
    } else {
        // Unlikely path: handle invalid orders
        return false;
    }
}
```

## Data Flow Monitoring

### Real-Time Flow Analysis

**Flow Metrics Collection**:
```c
typedef struct {
    uint64_t messages_per_second;   // Throughput measurement
    double average_latency_ns;      // Latency measurement
    uint32_t queue_depth;           // Backpressure measurement
    double utilization_percent;     // Resource utilization
} FlowMetrics_t;

// Real-time flow monitoring
void update_flow_metrics(FlowMetrics_t* metrics, uint64_t start_tsc, uint64_t end_tsc) {
    uint64_t latency_ticks = end_tsc - start_tsc;
    double latency_ns = ticks_to_nanoseconds(latency_ticks);
    
    // Update running averages
    metrics->average_latency_ns = 
        (metrics->average_latency_ns * 0.9) + (latency_ns * 0.1);
    
    // Check performance thresholds
    if (latency_ns > MAX_ALLOWED_LATENCY_NS) {
        trigger_flow_control_adjustment();
    }
}
```

---

*This document describes the complete data flow architecture of CNS v8.0, from semantic specifications through runtime execution and monitoring. For implementation details, see the component architecture documentation.*