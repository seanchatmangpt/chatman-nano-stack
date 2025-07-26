Feature: OTEL Telemetry Swarm Reactor Intelligence
  As a distributed system
  I want to analyze telemetry events for patterns
  So that I can create emergent AI behavior from observability data

  Background:
    Given the OTEL orchestrator is running
    And the swarm state is initialized
    And TTL constraints are configured

  @critical @performance
  Scenario: Detect resource lifecycle patterns
    Given a correlation chain with events:
      | event_type               | timestamp | duration_ms |
      | ash.create.start        | 1000      | 0           |
      | ash.changeset           | 1100      | 0           |
      | ash.validation          | 1200      | 50          |
      | ash.create.stop         | 1500      | 500         |
      | ash.update.start        | 2000      | 0           |
      | ash.update.stop         | 2300      | 300         |
    When the swarm reactor processes the correlation
    Then it should detect a "resource_lifecycle" pattern
    And the emergence factor should increase by at least 0.1
    And it should emit telemetry event "[:cns_forge, :telemetry_swarm, :intelligence_calculated]"

  @critical @ttl
  Scenario: Identify TTL violations
    Given a swarm state with ttl_compliance_rate of 0.95
    And TTL budget constraints:
      | constraint              | value_ns    |
      | max_processing_ns      | 1_000_000   |
      | max_step_execution_ns  | 500_000     |
    When processing an event with duration 5_000_000 nanoseconds
    Then it should detect a "ttl_violation" pattern
    And the ttl_compliance_rate should decrease
    And it should generate recommendation "Increase TTL budget for this operation"

  @integration @correlation
  Scenario: Handle missing correlation IDs
    Given an event without correlation_id
    When the swarm reactor processes the event
    Then it should generate a new correlation_id
    And it should add the event to correlations map
    And it should not fail processing

  @performance @emergence
  Scenario: Calculate emergence factor from patterns
    Given a swarm state with patterns:
      | pattern             | instance_count |
      | resource_lifecycle  | 15             |
      | ttl_bounded        | 20             |
      | error_cascade      | 2              |
      | low_correlation    | 5              |
    And total reactor runs of 1000
    When calculating emergence factor
    Then the emergence factor should be between 0.7 and 0.9
    And the swarm intelligence level should be "HIGHLY INTELLIGENT"

  @optimization @recommendations
  Scenario: Generate optimization recommendations
    Given low emergence factor of 0.2
    And patterns detected:
      | pattern          | instances |
      | low_correlation  | 50        |
    When generating recommendations
    Then it should suggest "Enable correlation IDs across all services"
    And recommendations should be added to optimization_queue
    And critical recommendations should be applied immediately

  @saga @compensation
  Scenario: Handle reactor step failures gracefully
    Given a telemetry event that will cause processing error
    When the swarm reactor processes the event
    Then it should catch the error
    And it should not crash the orchestrator
    And it should emit error telemetry
    And the swarm state should remain consistent

  @memory @performance
  Scenario: Clean old correlations to prevent memory growth
    Given correlations older than 5 minutes:
      | correlation_id | age_minutes |
      | old-corr-1    | 10          |
      | old-corr-2    | 7           |
      | recent-corr-1 | 2           |
    When the orchestrator runs periodic cleanup
    Then old correlations should be removed
    And recent correlations should be retained
    And memory usage should remain bounded

  @health @monitoring
  Scenario Outline: Assess swarm health based on emergence
    Given emergence factor of <emergence>
    When assessing swarm health
    Then health status should be "<health>"

    Examples:
      | emergence | health     |
      | 0.9       | excellent  |
      | 0.7       | good       |
      | 0.5       | fair       |
      | 0.3       | poor       |
      | 0.1       | critical   |

  @integration @bitactor
  Scenario: Process BitActor telemetry events
    Given BitActor events:
      | event                                      | metadata           |
      | cns_forge.bit_actor.bit_actor_spawned     | {actor_id: "123"} |
      | cns_forge.bit_actor.bit_actor_hop_processed| {hops: 3}         |
      | cns_forge.bit_actor.bit_actor_terminated  | {reason: "done"}  |
    When the swarm processes BitActor events
    Then it should correlate BitActor lifecycle
    And it should track BitActor performance metrics
    And it should detect BitActor patterns

  @concurrent @stress
  Scenario: Handle high-volume concurrent events
    Given 1000 concurrent telemetry events
    When all events fire simultaneously
    Then the orchestrator should process all events
    And no events should be dropped
    And emergence factor should increase significantly
    And processing time should be under 5 seconds