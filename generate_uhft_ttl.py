#!/usr/bin/env python3
"""
Generate comprehensive UHFT (Ultra-High-Frequency Trading) TTL files
Creates all possible semantic artifacts for a complete UHFT system
"""

import os
from pathlib import Path
from datetime import datetime
import hashlib

# Create output directory
output_dir = Path("ontologies/generated/uhft")
output_dir.mkdir(parents=True, exist_ok=True)

# UHFT-specific namespaces
UHFT_NS = "http://cns.io/uhft#"
CNS_NS = "http://cns.io/ontology#"

def generate_ttl_header(title, description):
    """Generate standard TTL header"""
    return f"""@prefix : <{UHFT_NS}> .
@prefix cns: <{CNS_NS}> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xml: <http://www.w3.org/XML/1998/namespace> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix shacl: <http://www.w3.org/ns/shacl#> .
@prefix dcterms: <http://purl.org/dc/terms/> .

# {title}
# {description}
# Generated: {datetime.now().isoformat()}
# For Ultra-High-Frequency Trading Systems

"""

# Core UHFT Ontology
uhft_core = generate_ttl_header(
    "UHFT Core Ontology",
    "Foundational classes for Ultra-High-Frequency Trading"
) + """
# Core Trading Classes
:Order a owl:Class ;
    rdfs:label "Trading Order" ;
    rdfs:comment "Represents a trading order with 8-tick execution guarantee" ;
    rdfs:subClassOf cns:BitActor .

:OrderBook a owl:Class ;
    rdfs:label "Order Book" ;
    rdfs:comment "Lock-free order book implementation" ;
    rdfs:subClassOf cns:Arena .

:MatchingEngine a owl:Class ;
    rdfs:label "Matching Engine" ;
    rdfs:comment "Ultra-low-latency order matching engine" ;
    rdfs:subClassOf cns:RingBus .

:MarketData a owl:Class ;
    rdfs:label "Market Data" ;
    rdfs:comment "Real-time market data feed" ;
    rdfs:subClassOf cns:Fiber .

# Properties
:orderPrice a owl:DatatypeProperty ;
    rdfs:domain :Order ;
    rdfs:range xsd:decimal ;
    rdfs:comment "Order price in fixed-point representation" .

:orderQuantity a owl:DatatypeProperty ;
    rdfs:domain :Order ;
    rdfs:range xsd:integer ;
    rdfs:comment "Order quantity" .

:orderTimestamp a owl:DatatypeProperty ;
    rdfs:domain :Order ;
    rdfs:range xsd:long ;
    rdfs:comment "Order timestamp in nanoseconds" .

:executionLatency a owl:DatatypeProperty ;
    rdfs:domain :MatchingEngine ;
    rdfs:range xsd:integer ;
    rdfs:comment "Execution latency in CPU ticks (must be ≤8)" .
"""

# Market Microstructure Ontology
market_microstructure = generate_ttl_header(
    "UHFT Market Microstructure",
    "Detailed market microstructure semantics"
) + """
# Market Microstructure Classes
:Spread a owl:Class ;
    rdfs:label "Bid-Ask Spread" ;
    rdfs:comment "Represents bid-ask spread with tick precision" .

:TickSize a owl:Class ;
    rdfs:label "Tick Size" ;
    rdfs:comment "Minimum price movement" .

:LiquidityPool a owl:Class ;
    rdfs:label "Liquidity Pool" ;
    rdfs:comment "Aggregated liquidity at price level" ;
    rdfs:subClassOf cns:Arena .

:PriceLevel a owl:Class ;
    rdfs:label "Price Level" ;
    rdfs:comment "Single price level in order book" .

# Microstructure Properties
:bestBid a owl:ObjectProperty ;
    rdfs:domain :OrderBook ;
    rdfs:range :PriceLevel .

:bestAsk a owl:ObjectProperty ;
    rdfs:domain :OrderBook ;
    rdfs:range :PriceLevel .

:depth a owl:DatatypeProperty ;
    rdfs:domain :PriceLevel ;
    rdfs:range xsd:integer .

:tickValue a owl:DatatypeProperty ;
    rdfs:domain :TickSize ;
    rdfs:range xsd:decimal .
"""

# Risk Management Ontology
risk_management = generate_ttl_header(
    "UHFT Risk Management",
    "Real-time risk controls and circuit breakers"
) + """
# Risk Management Classes
:RiskLimit a owl:Class ;
    rdfs:label "Risk Limit" ;
    rdfs:comment "Real-time risk limit enforcement" .

:CircuitBreaker a owl:Class ;
    rdfs:label "Circuit Breaker" ;
    rdfs:comment "Automated trading halt mechanism" ;
    rdfs:subClassOf cns:BitActor .

:PositionTracker a owl:Class ;
    rdfs:label "Position Tracker" ;
    rdfs:comment "Real-time position tracking with 1-tick updates" ;
    rdfs:subClassOf cns:Fiber .

:MarginCalculator a owl:Class ;
    rdfs:label "Margin Calculator" ;
    rdfs:comment "Real-time margin calculation engine" .

# Risk Properties
:maxPosition a owl:DatatypeProperty ;
    rdfs:domain :RiskLimit ;
    rdfs:range xsd:long .

:maxOrderValue a owl:DatatypeProperty ;
    rdfs:domain :RiskLimit ;
    rdfs:range xsd:decimal .

:breachCount a owl:DatatypeProperty ;
    rdfs:domain :CircuitBreaker ;
    rdfs:range xsd:integer .

:marginRequirement a owl:DatatypeProperty ;
    rdfs:domain :MarginCalculator ;
    rdfs:range xsd:decimal .
"""

# Network Protocol Ontology
network_protocol = generate_ttl_header(
    "UHFT Network Protocols",
    "Ultra-low-latency network protocol definitions"
) + """
# Network Protocol Classes
:FIXMessage a owl:Class ;
    rdfs:label "FIX Protocol Message" ;
    rdfs:comment "Financial Information eXchange protocol message" .

:BinaryProtocol a owl:Class ;
    rdfs:label "Binary Protocol" ;
    rdfs:comment "Custom binary protocol for minimum latency" ;
    rdfs:subClassOf cns:BitActor .

:MulticastFeed a owl:Class ;
    rdfs:label "Multicast Feed" ;
    rdfs:comment "UDP multicast market data feed" ;
    rdfs:subClassOf cns:RingBus .

:TCPSession a owl:Class ;
    rdfs:label "TCP Session" ;
    rdfs:comment "Reliable order entry session" .

# Protocol Properties
:messageType a owl:DatatypeProperty ;
    rdfs:domain :FIXMessage ;
    rdfs:range xsd:string .

:sequenceNumber a owl:DatatypeProperty ;
    rdfs:domain :BinaryProtocol ;
    rdfs:range xsd:long .

:packetLatency a owl:DatatypeProperty ;
    rdfs:domain :MulticastFeed ;
    rdfs:range xsd:integer ;
    rdfs:comment "Packet processing latency in nanoseconds" .
"""

# Strategy Ontology
strategy_ontology = generate_ttl_header(
    "UHFT Trading Strategies",
    "High-frequency trading strategy definitions"
) + """
# Trading Strategy Classes
:MarketMaking a owl:Class ;
    rdfs:label "Market Making Strategy" ;
    rdfs:comment "Continuous bid-ask quoting strategy" ;
    rdfs:subClassOf cns:Fiber .

:Arbitrage a owl:Class ;
    rdfs:label "Arbitrage Strategy" ;
    rdfs:comment "Cross-market arbitrage with sub-microsecond execution" .

:MomentumTrading a owl:Class ;
    rdfs:label "Momentum Trading" ;
    rdfs:comment "Microsecond momentum detection and execution" .

:StatisticalArbitrage a owl:Class ;
    rdfs:label "Statistical Arbitrage" ;
    rdfs:comment "Pairs trading and mean reversion strategies" .

# Strategy Properties
:profitTarget a owl:DatatypeProperty ;
    rdfs:domain :MarketMaking ;
    rdfs:range xsd:decimal .

:stopLoss a owl:DatatypeProperty ;
    rdfs:domain :MarketMaking ;
    rdfs:range xsd:decimal .

:correlationThreshold a owl:DatatypeProperty ;
    rdfs:domain :StatisticalArbitrage ;
    rdfs:range xsd:float .
"""

# Infrastructure Ontology
infrastructure = generate_ttl_header(
    "UHFT Infrastructure",
    "Hardware and system infrastructure for UHFT"
) + """
# Infrastructure Classes
:FPGAAccelerator a owl:Class ;
    rdfs:label "FPGA Accelerator" ;
    rdfs:comment "Hardware acceleration for order matching" .

:KernelBypass a owl:Class ;
    rdfs:label "Kernel Bypass" ;
    rdfs:comment "Zero-copy kernel bypass networking" ;
    rdfs:subClassOf cns:Arena .

:CPUCore a owl:Class ;
    rdfs:label "CPU Core" ;
    rdfs:comment "Dedicated CPU core with isolated interrupts" .

:MemoryPool a owl:Class ;
    rdfs:label "Memory Pool" ;
    rdfs:comment "Pre-allocated memory pool for zero allocation" ;
    rdfs:subClassOf cns:Arena .

# Infrastructure Properties
:coreAffinity a owl:DatatypeProperty ;
    rdfs:domain :CPUCore ;
    rdfs:range xsd:integer .

:numaNode a owl:DatatypeProperty ;
    rdfs:domain :MemoryPool ;
    rdfs:range xsd:integer .

:pcieLanes a owl:DatatypeProperty ;
    rdfs:domain :FPGAAccelerator ;
    rdfs:range xsd:integer .
"""

# SHACL Constraints for UHFT
shacl_constraints = generate_ttl_header(
    "UHFT SHACL Constraints",
    "Validation shapes for UHFT compliance"
) + """
# Order Validation Shape
:OrderShape a shacl:NodeShape ;
    shacl:targetClass :Order ;
    shacl:property [
        shacl:path :orderPrice ;
        shacl:minInclusive 0.0 ;
        shacl:datatype xsd:decimal ;
        shacl:message "Order price must be non-negative decimal" ;
    ] ;
    shacl:property [
        shacl:path :orderQuantity ;
        shacl:minInclusive 1 ;
        shacl:maxInclusive 1000000 ;
        shacl:datatype xsd:integer ;
        shacl:message "Order quantity must be between 1 and 1000000" ;
    ] ;
    shacl:property [
        shacl:path :executionLatency ;
        shacl:maxInclusive 8 ;
        shacl:message "Execution latency must not exceed 8 ticks" ;
    ] .

# Risk Limit Shape
:RiskLimitShape a shacl:NodeShape ;
    shacl:targetClass :RiskLimit ;
    shacl:property [
        shacl:path :maxPosition ;
        shacl:minInclusive 0 ;
        shacl:message "Maximum position must be non-negative" ;
    ] ;
    shacl:property [
        shacl:path :maxOrderValue ;
        shacl:minInclusive 0.0 ;
        shacl:message "Maximum order value must be non-negative" ;
    ] .

# Latency Constraint Shape
:LatencyShape a shacl:NodeShape ;
    shacl:targetClass :MatchingEngine ;
    shacl:property [
        shacl:path :executionLatency ;
        shacl:maxInclusive 8 ;
        shacl:severity shacl:Violation ;
        shacl:message "CRITICAL: Matching engine must guarantee ≤8 tick execution" ;
    ] .
"""

# SPARQL Queries for UHFT
sparql_queries = """# SPARQL Queries for UHFT Analysis

# Query 1: Find all orders exceeding latency threshold
PREFIX : <http://cns.io/uhft#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT ?order ?latency
WHERE {
    ?order a :Order ;
           :executionLatency ?latency .
    FILTER (?latency > 8)
}
ORDER BY DESC(?latency)

---

# Query 2: Calculate average spread by time window
PREFIX : <http://cns.io/uhft#>

SELECT ?timeWindow (AVG(?spread) as ?avgSpread)
WHERE {
    ?book a :OrderBook ;
          :timestamp ?time ;
          :currentSpread ?spread .
    BIND(FLOOR(?time / 1000000) as ?timeWindow)
}
GROUP BY ?timeWindow
ORDER BY ?timeWindow

---

# Query 3: Identify risk limit breaches
PREFIX : <http://cns.io/uhft#>

SELECT ?entity ?limit ?actual ?breach
WHERE {
    ?entity :hasRiskLimit ?limit .
    ?limit :maxPosition ?maxPos .
    ?entity :currentPosition ?actual .
    BIND((?actual > ?maxPos) as ?breach)
    FILTER(?breach = true)
}

---

# Query 4: Market making profitability
PREFIX : <http://cns.io/uhft#>

SELECT ?strategy (SUM(?profit) as ?totalProfit) (COUNT(?trade) as ?tradeCount)
WHERE {
    ?strategy a :MarketMaking ;
              :executedTrade ?trade .
    ?trade :profit ?profit .
}
GROUP BY ?strategy
ORDER BY DESC(?totalProfit)
"""

# Performance Benchmark Ontology
performance_benchmark = generate_ttl_header(
    "UHFT Performance Benchmarks",
    "Performance metrics and benchmarking ontology"
) + """
# Performance Classes
:LatencyBenchmark a owl:Class ;
    rdfs:label "Latency Benchmark" ;
    rdfs:comment "Measures end-to-end latency" .

:ThroughputBenchmark a owl:Class ;
    rdfs:label "Throughput Benchmark" ;
    rdfs:comment "Measures orders per second" .

:TickCompliance a owl:Class ;
    rdfs:label "8-Tick Compliance" ;
    rdfs:comment "Validates 8-tick execution guarantee" .

# Benchmark Properties
:p50Latency a owl:DatatypeProperty ;
    rdfs:domain :LatencyBenchmark ;
    rdfs:range xsd:integer ;
    rdfs:comment "50th percentile latency in nanoseconds" .

:p99Latency a owl:DatatypeProperty ;
    rdfs:domain :LatencyBenchmark ;
    rdfs:range xsd:integer ;
    rdfs:comment "99th percentile latency in nanoseconds" .

:ordersPerSecond a owl:DatatypeProperty ;
    rdfs:domain :ThroughputBenchmark ;
    rdfs:range xsd:long ;
    rdfs:comment "Maximum sustained orders per second" .

:tickViolations a owl:DatatypeProperty ;
    rdfs:domain :TickCompliance ;
    rdfs:range xsd:integer ;
    rdfs:comment "Number of 8-tick violations (must be 0)" .
"""

# Generate all TTL files
ttl_files = {
    "uhft_core.ttl": uhft_core,
    "market_microstructure.ttl": market_microstructure,
    "risk_management.ttl": risk_management,
    "network_protocol.ttl": network_protocol,
    "strategy.ttl": strategy_ontology,
    "infrastructure.ttl": infrastructure,
    "shacl_constraints.ttl": shacl_constraints,
    "sparql_queries.sparql": sparql_queries,
    "performance_benchmark.ttl": performance_benchmark
}

# Write all files
for filename, content in ttl_files.items():
    filepath = output_dir / filename
    with open(filepath, 'w') as f:
        f.write(content)
    print(f"Generated: {filepath}")

# Create a master ontology that imports all others
master_ontology = generate_ttl_header(
    "UHFT Master Ontology",
    "Imports all UHFT ontology modules"
) + """
# Import all UHFT modules
<http://cns.io/uhft/core> a owl:Ontology ;
    owl:imports <uhft_core.ttl> ,
                <market_microstructure.ttl> ,
                <risk_management.ttl> ,
                <network_protocol.ttl> ,
                <strategy.ttl> ,
                <infrastructure.ttl> ,
                <performance_benchmark.ttl> ;
    dcterms:title "CNS Ultra-High-Frequency Trading Ontology" ;
    dcterms:description "Complete semantic model for UHFT systems with 8-tick compliance" ;
    dcterms:creator "CNS Ontology Forge" ;
    dcterms:date "2024-01-23"^^xsd:date ;
    owl:versionInfo "1.0.0" .
"""

with open(output_dir / "uhft_master.ttl", 'w') as f:
    f.write(master_ontology)

print(f"\nGenerated {len(ttl_files) + 1} UHFT ontology files in {output_dir}")
print("\nUHFT Ontology Modules:")
print("- Core Trading Concepts")
print("- Market Microstructure") 
print("- Risk Management")
print("- Network Protocols")
print("- Trading Strategies")
print("- Infrastructure")
print("- SHACL Constraints")
print("- SPARQL Queries")
print("- Performance Benchmarks")
print("- Master Ontology (imports all)")