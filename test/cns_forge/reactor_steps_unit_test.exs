defmodule CnsForge.ReactorStepsUnitTest do
  @moduledoc """
  Unit tests for individual Ash.Reactor steps
  Ensures each step functions correctly in isolation
  """
  
  use ExUnit.Case, async: true
  use Ash.Test
  
  alias CnsForge.AshReactorHyperIntelligenceSwarm
  
  describe "AdversarialAnalyzer step" do
    test "identifies missing input connections" do
      # Mock validation report with missing inputs
      validation_data = %{
        step_validations: [
          %{
            step_id: "update_author_post_count",
            step_type: "update",
            issues_found: [
              %{
                severity: "warning",
                category: "execution",
                description: "No inputs were resolved for this step"
              }
            ]
          }
        ]
      }
      
      # Run analyzer step
      result = AdversarialAnalyzer.analyze(%{
        validation_reports: validation_data,
        system_components: []
      })
      
      assert {:ok, weaknesses} = result
      assert length(weaknesses) > 0
      
      weakness = Enum.find(weaknesses, & &1.component == "update_author_post_count")
      assert weakness.weakness_type == "missing_input"
      assert weakness.severity == "high"
    end
    
    test "identifies security vulnerabilities" do
      validation_data = %{
        security_defenses: %{
          defense_effectiveness: "74%"
        },
        adversarial_analysis: %{
          critical_issues: 8
        }
      }
      
      result = AdversarialAnalyzer.analyze(%{
        validation_reports: validation_data,
        system_components: []
      })
      
      assert {:ok, weaknesses} = result
      security_vulns = Enum.filter(weaknesses, & &1.weakness_type == "security_vuln")
      assert length(security_vulns) == 8
      assert Enum.all?(security_vulns, & &1.severity == "critical")
    end
    
    test "identifies OTEL coverage gaps" do
      validation_data = %{
        otel_coverage: %{
          metric_completeness: %{
            execution_count: %{coverage_percent: 81.08},
            error_rate: %{coverage_percent: 75.03},
            resource_utilization: %{coverage_percent: 82.75}
          }
        }
      }
      
      result = AdversarialAnalyzer.analyze(%{
        validation_reports: validation_data,
        system_components: []
      })
      
      assert {:ok, weaknesses} = result
      otel_gaps = Enum.filter(weaknesses, & &1.weakness_type == "otel_gap")
      assert length(otel_gaps) == 3
    end
  end
  
  describe "HealingConnectionFactory step" do
    test "creates healing connections for critical issues" do
      weaknesses = [
        %{
          component: "update_author_post_count",
          weakness_type: "missing_input",
          severity: "high",
          auto_fixable: true
        },
        %{
          component: "authentication_bypass",
          weakness_type: "security_vuln",
          severity: "critical",
          auto_fixable: true
        }
      ]
      
      result = HealingConnectionFactory.create(%{
        weaknesses: weaknesses,
        severity_filter: ["critical", "high"]
      })
      
      assert {:ok, connections} = result
      assert length(connections) == 2
      
      # Verify input connector created
      input_conn = Enum.find(connections, & &1.connection_type == "data_flow")
      assert input_conn.source == "create_post.output.author_id"
      assert input_conn.target == "update_author_post_count.input.author_id"
      assert input_conn.auto_recovery_enabled
      
      # Verify security defense created
      security_conn = Enum.find(connections, & &1.connection_type == "security_filter")
      assert security_conn.circuit_breaker_threshold == 0.1  # Very sensitive
    end
    
    test "prioritizes critical issues with 80/20 approach" do
      weaknesses = [
        %{severity: "critical", auto_fixable: true},
        %{severity: "critical", auto_fixable: true},
        %{severity: "high", auto_fixable: true},
        %{severity: "high", auto_fixable: true},
        %{severity: "medium", auto_fixable: true},
        %{severity: "medium", auto_fixable: true},
        %{severity: "low", auto_fixable: true},
        %{severity: "low", auto_fixable: true}
      ]
      
      result = HealingConnectionFactory.create(%{
        weaknesses: weaknesses,
        severity_filter: ["critical", "high"]
      })
      
      assert {:ok, connections} = result
      # Should create connections for all critical and high (80%)
      assert length(connections) >= 4
      # May create some medium/low (20%)
      assert length(connections) <= 6
    end
  end
  
  describe "InputResolutionHealer step" do
    test "fixes missing input connections" do
      broken_steps = [
        %{
          name: "update_author_post_count",
          type: "update",
          inputs: [],
          expected_inputs: [:author_id]
        }
      ]
      
      reactor_configs = %{
        "create_post" => %{
          outputs: %{
            post: %{id: :uuid},
            author: %{id: :uuid}
          }
        }
      }
      
      result = InputResolutionHealer.heal(%{
        broken_steps: broken_steps,
        reactor_definitions: reactor_configs
      })
      
      assert {:ok, healed} = result
      assert length(healed) == 1
      
      connection = hd(healed)
      assert connection.source_step == "create_post"
      assert connection.source_output == "author.id"
      assert connection.target_step == "update_author_post_count"
      assert connection.target_input == "author_id"
    end
    
    test "infers connections using semantic analysis" do
      broken_step = %{
        name: "notify_author",
        expected_type: :author_id
      }
      
      semantic_graph = %{
        nodes: [
          %{name: "create_post", outputs: %{author: %{id: :uuid}}},
          %{name: "create_comment", outputs: %{user: %{id: :uuid}}}
        ]
      }
      
      # Should prefer "author" over "user" based on semantic similarity
      inferred = InputResolutionHealer.infer_connection(broken_step, semantic_graph)
      
      assert inferred.source == "create_post"
      assert inferred.confidence > 0.8
    end
  end
  
  describe "SecurityDefenseReactor step" do
    test "deploys comprehensive security defenses" do
      attack_patterns = [
        %{type: "sql_injection", severity: "critical"},
        %{type: "xss", severity: "critical"},
        %{type: "csrf", severity: "critical"}
      ]
      
      result = SecurityDefenseReactor.deploy(%{
        attack_patterns: attack_patterns,
        existing_defenses: %{}
      })
      
      assert {:ok, defenses} = result
      assert length(defenses) >= 3
      
      # Each defense should have multiple layers
      Enum.each(defenses, fn defense ->
        assert length(defense.defense_layers) >= 3
        assert defense.effectiveness_target == 0.9
      end)
    end
    
    test "monitors attack attempts in real-time" do
      defenses = [
        %{id: "def_1", type: "sql_injection", blocked_count: 0}
      ]
      
      # Simulate attack
      attack = %{type: "sql_injection", payload: "'; DROP TABLE--"}
      
      result = SecurityDefenseReactor.monitor_attack(attack, defenses)
      
      assert {:blocked, updated_defense} = result
      assert updated_defense.blocked_count == 1
    end
  end
  
  describe "OTELBridgeReactor step" do
    test "identifies OTEL coverage gaps" do
      current_coverage = %{
        execution_count: 81.08,
        error_rate: 75.03,
        resource_utilization: 82.75
      }
      
      result = OTELBridgeReactor.identify_gaps(%{
        current_coverage: current_coverage,
        target_coverage: 100
      })
      
      assert {:ok, gaps} = result
      assert length(gaps) == 3
      
      exec_gap = Enum.find(gaps, & &1.metric == "execution_count")
      assert exec_gap.coverage_gap == 18.92
    end
    
    test "creates missing OTEL instruments" do
      missing_instruments = [
        %{
          metric_type: "execution_count",
          component: "update_author_post_count",
          recommended_sampling: 1.0
        }
      ]
      
      result = OTELBridgeReactor.create_instruments(missing_instruments)
      
      assert {:ok, instruments} = result
      assert length(instruments) == 1
      
      instrument = hd(instruments)
      assert instrument.sampling_rate == 1.0
      assert instrument.metric_type == "execution_count"
    end
    
    test "validates 100% coverage achieved" do
      coverage_result = %{
        coverage_percent: 100.0,
        metrics_instrumented: 64,
        components_covered: 5
      }
      
      result = OTELBridgeReactor.validate_coverage(coverage_result, 100)
      
      assert {:ok, :coverage_complete} = result
    end
  end
  
  describe "HealthMonitorReactor step" do
    test "monitors connection health" do
      connections = [
        %{
          id: "conn_1",
          current_health: 0.9,
          circuit_breaker_threshold: 0.5
        },
        %{
          id: "conn_2", 
          current_health: 0.3,
          circuit_breaker_threshold: 0.5
        }
      ]
      
      result = HealthMonitorReactor.check_health(connections)
      
      assert {:ok, health_report} = result
      assert health_report.healthy_count == 1
      assert health_report.unhealthy_count == 1
      assert length(health_report.needs_healing) == 1
    end
    
    test "triggers healing for unhealthy connections" do
      unhealthy_conn = %{
        id: "conn_1",
        current_health: 0.3,
        circuit_breaker_threshold: 0.5,
        fallback_strategies: ["retry", "circuit_break", "reroute"]
      }
      
      result = HealthMonitorReactor.heal_connection(unhealthy_conn)
      
      assert {:ok, healed_conn} = result
      assert healed_conn.current_health > unhealthy_conn.current_health
      assert healed_conn.recoveries == 1
    end
  end
  
  describe "AdaptiveIntelligenceReactor step" do
    test "learns from attack patterns" do
      attack_history = [
        %{type: "sql_injection", timestamp: ~U[2025-01-25 10:00:00Z], blocked: true},
        %{type: "sql_injection", timestamp: ~U[2025-01-25 10:05:00Z], blocked: true},
        %{type: "xss", timestamp: ~U[2025-01-25 10:10:00Z], blocked: true}
      ]
      
      result = AdaptiveIntelligenceReactor.analyze_patterns(attack_history)
      
      assert {:ok, patterns} = result
      assert patterns.most_common_attack == "sql_injection"
      assert patterns.attack_frequency > 0
    end
    
    test "updates defense strategies based on learning" do
      learning_data = %{
        attack_patterns: %{sql_injection: 10, xss: 5},
        defense_effectiveness: %{sql_injection: 0.9, xss: 0.95}
      }
      
      result = AdaptiveIntelligenceReactor.update_strategies(learning_data)
      
      assert {:ok, new_strategies} = result
      # Should strengthen defenses for more common attacks
      assert new_strategies.sql_injection.priority == "high"
    end
  end
  
  describe "Reactor compensation handlers" do
    test "rollback security defenses on failure" do
      error = %{type: :deployment_failed, reason: "insufficient resources"}
      context = %{deployed_defenses: ["def_1", "def_2"]}
      
      result = SecurityDefenseReactor.rollback(error, context)
      
      assert {:ok, :rolled_back} = result
    end
    
    test "restore connections on healing failure" do
      error = %{type: :healing_failed, reason: "incompatible types"}
      context = %{original_connections: [%{id: "conn_1", health: 0.5}]}
      
      result = ConnectionHealer.restore(error, context)
      
      assert {:ok, :restored} = result
    end
  end
end