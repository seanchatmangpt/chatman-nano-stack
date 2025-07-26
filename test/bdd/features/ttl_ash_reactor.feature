Feature: TTL to Ash Reactor Transformation
  As a developer
  I want to transform TTL ontologies into Ash.Reactor workflows
  So that I can process semantic data with TTL-bounded execution

  Background:
    Given the TTL to Ash Reactor transformer is loaded
    And I have a valid TTL ontology

  @critical @transformation
  Scenario: Transform simple TTL with single class
    Given a TTL ontology with content:
      """
      @prefix cns: <http://cns.io/ontology#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      
      cns:TestClass a owl:Class ;
          rdfs:label "Test Class" .
      """
    When I transform the TTL to Ash Reactor
    Then the transformation should succeed
    And 1 Ash resource should be generated
    And 1 reactor workflow should be created
    And the main reactor should include "TestClass" processing step

  @critical @shacl
  Scenario: Transform TTL with SHACL shapes
    Given a TTL ontology with SHACL shapes:
      """
      @prefix sh: <http://www.w3.org/ns/shacl#> .
      @prefix cns: <http://cns.io/ontology#> .
      
      cns:AgentShape a sh:NodeShape ;
          sh:targetClass cns:Agent ;
          sh:property [
              sh:path cns:agentId ;
              sh:datatype xsd:string
          ] .
      """
    When I transform the TTL to Ash Reactor
    Then the transformation should succeed
    And the generated resource should have "agent_id" attribute
    And the attribute should have type "string"

  @ttl_bounds @performance
  Scenario: Enforce TTL execution constraints
    Given a TTL ontology with TTL budget constraints
    And the max execution time is set to "1000000" nanoseconds
    When I execute a reactor workflow
    And the execution exceeds the TTL budget
    Then the reactor should return an error
    And the error should contain "TTL constraint violation"

  @relationships
  Scenario: Transform TTL with object properties
    Given a TTL ontology with relationships:
      """
      cns:processes a owl:ObjectProperty ;
          rdfs:domain cns:Agent ;
          rdfs:range cns:Signal .
      """
    When I transform the TTL to Ash Reactor
    Then the Agent resource should have a "belongs_to" relationship to Signal
    And the relationship should be properly configured

  @reactor_steps
  Scenario Outline: Reactor workflow steps execute in order
    Given a reactor workflow with steps: <steps>
    When I run the reactor
    Then each step should execute in the defined order
    And step dependencies should be respected
    And the final result should aggregate all step outputs

    Examples:
      | steps                                               |
      | "initialize_ttl_context,validate_ontology,process" |
      | "validate,process_class_a,process_class_b,aggregate" |

  @error_handling
  Scenario: Handle malformed TTL gracefully
    Given an invalid TTL ontology:
      """
      @prefix broken
      this is not valid turtle
      """
    When I attempt to transform the TTL
    Then the transformation should fail
    And return a descriptive error message
    And not crash the system

  @coverage @metrics
  Scenario: Measure transformation performance
    Given a complex TTL ontology with 10 classes
    When I transform with performance tracking enabled
    Then metrics should include:
      | metric               | min_value |
      | parsing_time_ms      | 0         |
      | transformation_time  | 0         |
      | resources_generated  | 10        |
      | reactors_generated   | 11        |
    And total time should be under 1000ms