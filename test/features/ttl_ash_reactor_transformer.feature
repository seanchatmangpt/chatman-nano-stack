Feature: TTL to Ash.Reactor Transformation
  As a developer
  I want to transform TTL ontologies into Ash.Reactor workflows
  So that I can generate complete Elixir projects from semantic definitions

  Background:
    Given a valid TTL ontology file
    And the TTL transformer module is loaded
    And output directory is writable

  @critical @parsing
  Scenario: Parse TTL ontology with multiple classes
    Given TTL content with:
      """
      @prefix : <http://example.org/> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      
      :Person a owl:Class .
      :Organization a owl:Class .
      :worksFor a owl:ObjectProperty ;
        rdfs:domain :Person ;
        rdfs:range :Organization .
      """
    When I parse the TTL content
    Then I should extract 2 classes
    And I should extract 1 property
    And classes should include "Person" and "Organization"
    And property "worksFor" should link "Person" to "Organization"

  @critical @generation
  Scenario: Generate Ash resources from TTL classes
    Given parsed TTL with classes:
      | class_name | uri               |
      | Asset      | cyber:Asset       |
      | Threat     | cyber:Threat      |
      | Control    | cyber:Control     |
    When I generate Ash resources
    Then I should create 3 resource modules
    And each resource should have:
      | attribute        | type               |
      | id              | uuid_primary_key   |
      | ttl_uri         | string             |
      | created_at      | utc_datetime_usec  |
      | updated_at      | utc_datetime_usec  |
    And resources should use Ash.DataLayer.Ets

  @critical @reactor
  Scenario: Generate main Ash.Reactor workflow
    Given TTL classes for Security, Incident, Response
    When I generate the main reactor
    Then the reactor should:
      | requirement                          | implementation                    |
      | Use Reactor                         | use Reactor                       |
      | Have TTL context initialization     | step :initialize_ttl_context      |
      | Validate ontology structure         | step :validate_ontology           |
      | Process each class                  | step :process_security, etc       |
      | Aggregate results with TTL check    | step :aggregate_results           |
      | Return aggregated results           | return :aggregate_results         |
    And each processing step should enforce TTL constraints

  @performance @ttl
  Scenario: Enforce TTL constraints in generated code
    Given TTL constraints:
      | constraint                | value_ns        |
      | max_total_execution_ns   | 10_000_000_000  |
      | max_step_execution_ns    | 1_000_000_000   |
    When generating reactor steps
    Then each step should check execution time
    And steps exceeding TTL should return error
    And aggregate step should validate total execution time

  @integration @relationships
  Scenario: Generate relationships from TTL properties
    Given TTL properties:
      | property      | domain      | range       |
      | hasVulnerability | Asset    | Vulnerability |
      | mitigatedBy   | Threat      | Control     |
    When generating resources
    Then Asset resource should have:
      """
      belongs_to :vulnerability, CnsForge.TTLResources.Vulnerability
      """
    And Threat resource should have:
      """
      belongs_to :control, CnsForge.TTLResources.Control
      """

  @files @output
  Scenario: Write generated files to filesystem
    Given successful generation of:
      | type      | count |
      | resources | 5     |
      | reactors  | 6     |
      | domain    | 1     |
    When writing files to output directory
    Then I should create files:
      | pattern                        | count |
      | *_resource.ex                 | 5     |
      | ttlmainreactor.ex             | 1     |
      | ttl*reactor.ex                | 5     |
      | ttl_domain.ex                 | 1     |
    And all files should be valid Elixir syntax

  @error @handling
  Scenario: Handle malformed TTL gracefully
    Given TTL content with syntax errors:
      """
      @prefix : <http://example.org/> 
      :BadClass a owl:Class
      """
    When I attempt to parse the TTL
    Then parsing should fail with error message
    And no files should be generated
    And the error should be logged

  @validation @semantic
  Scenario: Validate semantic consistency
    Given TTL with circular property definitions
    When parsing and generating
    Then the transformer should detect circular references
    And it should log warnings
    But it should still generate valid code

  @domain @configuration
  Scenario: Generate Ash.Domain with proper configuration
    Given resources and reactors are generated
    When generating the domain module
    Then it should:
      | include                        | code                                    |
      | Use Ash.Domain                | use Ash.Domain                          |
      | List all resources            | resource CnsForge.TTLResources.*        |
      | Authorization settings        | authorize :when_requested               |
      | TTL processing function       | process_ontology_with_ttl_bounds/2      |
    And the TTL processing function should run the main reactor

  @prefixes @namespaces
  Scenario: Handle multiple namespace prefixes
    Given TTL with prefixes:
      | prefix | uri                              |
      | owl    | http://www.w3.org/2002/07/owl#  |
      | rdfs   | http://www.w3.org/2000/01/rdf-schema# |
      | cyber  | http://cybersecurity.org/        |
      | custom | http://example.org/custom#       |
    When parsing prefixes
    Then all 4 prefixes should be extracted
    And URIs should be properly resolved in class names

  @performance @optimization
  Scenario: Generate optimized reactor code
    Given 50+ TTL classes to process
    When generating reactor workflow
    Then steps should be configured with:
      | setting        | value                    |
      | async?         | true (where safe)        |
      | max_retries    | 0 (for low latency)     |
      | timeout        | based on TTL constraints |
    And parallel processing should be used where possible