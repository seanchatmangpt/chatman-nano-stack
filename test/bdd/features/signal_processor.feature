Feature: Signal Processor Reactor Workflow
  As a signal processing system
  I want to process signals for BitActors
  So that BitActors can react to market signals with TTL constraints

  Background:
    Given a BitActor exists with id "test-bitactor-id"
    And the BitActor has a TTL budget of 10ms

  @positive @happy_path
  Scenario: Successfully process single signal
    Given I have 1 signal with id "signal-1"
    When I run the Signal Processor for the BitActor
    Then the BitActor should be loaded successfully
    And 1 signal should be processed successfully
    And 0 signals should fail
    And the result should show the BitActor id

  @positive @batch_processing
  Scenario: Successfully process multiple signals
    Given I have 5 signals with ids ["signal-1", "signal-2", "signal-3", "signal-4", "signal-5"]
    When I run the Signal Processor for the BitActor
    Then the BitActor should be loaded successfully
    And 5 signals should be processed successfully
    And 0 signals should fail
    And the total should be 5

  @negative @error_handling
  Scenario: BitActor not found
    Given I have a non-existent BitActor id "invalid-id"
    And I have 1 signal with id "signal-1"
    When I run the Signal Processor
    Then the BitActor loading should fail
    And the error should contain "BitActor not found: invalid-id"

  @negative @ttl_violation
  Scenario: Signal processing TTL violation
    Given the BitActor has a TTL budget of 1ms
    And I have 3 signals that take long to process
    When I run the Signal Processor for the BitActor
    Then some signals should fail due to TTL violation
    And the failed count should be greater than 0
    And successful count should be less than total

  @edge_case @empty_signals
  Scenario: Process empty signal list
    Given I have 0 signals
    When I run the Signal Processor for the BitActor
    Then the BitActor should be loaded successfully
    And 0 signals should be processed
    And the total should be 0

  @performance @load_test
  Scenario: Process large batch of signals
    Given I have 100 signals
    When I run the Signal Processor for the BitActor
    Then the processing should complete within 5 seconds
    And the result should show successful and failed counts
    And the total should be 100