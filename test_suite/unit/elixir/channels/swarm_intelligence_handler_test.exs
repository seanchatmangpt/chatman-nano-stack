defmodule CnsWeb.Channels.SwarmIntelligenceHandlerTest do
  @moduledoc """
  Unit tests for SwarmIntelligenceHandler with 80/20 optimization validation.
  Tests adaptive learning, optimization recommendations, and swarm coordination.
  """
  
  use CnsWeb.ChannelCase
  use ExUnit.Case, async: true
  
  alias CnsWeb.Channels.SwarmIntelligenceHandler
  alias CnsWeb.UserSocket
  
  setup do
    socket = socket(UserSocket, "user_id", %{
      current_user: %{id: "test_user", role: "operator"},
      optimization_mode: "80_20",
      swarm_preferences: %{
        learning_rate: 0.1,
        adaptation_threshold: 0.8
      }
    })
    
    %{socket: socket}
  end
  
  describe "optimization recommendations" do
    test "generates optimization recommendations based on current state", %{socket: socket} do
      payload = %{
        "target" => "pipeline",
        "current_metrics" => %{
          "avg_duration" => 450,
          "success_rate" => 0.85,
          "bottlenecks" => ["ash", "reactor"]
        },
        "constraints" => %{
          "max_duration" => 400,
          "min_success_rate" => 0.9
        }
      }
      
      result = SwarmIntelligenceHandler.generate_recommendations(payload, %{}, socket)
      
      assert {:reply, {:ok, recommendations}, _socket} = result
      assert Map.has_key?(recommendations, :improvements)
      assert Map.has_key?(recommendations, :estimated_gains)
      assert length(recommendations.improvements) > 0
    end
    
    test "applies swarm optimization to pipeline", %{socket: socket} do
      payload = %{
        "target" => "pipeline",
        "strategy" => "80_20",
        "optimization_goals" => [
          "reduce_duration",
          "improve_success_rate",
          "minimize_resource_usage"
        ]
      }
      
      result = SwarmIntelligenceHandler.optimize(payload, %{}, socket)
      
      assert {:reply, {:ok, optimization_result}, _socket} = result
      assert Map.has_key?(optimization_result, :optimizations_applied)
      assert Map.has_key?(optimization_result, :estimated_improvement)
    end
    
    test "recommends stage-specific optimizations", %{socket: socket} do
      payload = %{
        "stage" => "ash",
        "performance_data" => %{
          "avg_duration" => 200,
          "error_rate" => 0.05,
          "resource_usage" => %{"cpu" => 0.8, "memory" => 0.6}
        }
      }
      
      result = SwarmIntelligenceHandler.recommend_stage_optimization(payload, %{}, socket)
      
      assert {:reply, {:ok, stage_recommendations}, _socket} = result
      assert is_list(stage_recommendations.optimizations)
    end
  end
  
  describe "adaptive learning" do
    test "learns from execution patterns and outcomes", %{socket: socket} do
      payload = %{
        "execution_id" => "learning_execution_123",
        "domain" => "cybersecurity",
        "metrics" => %{
          "duration" => 350,
          "success_rate" => 0.92,
          "efficiency" => 0.88
        },
        "outcomes" => %{
          "success" => true,
          "errors" => [],
          "user_satisfaction" => 0.9
        },
        "context" => %{
          "time_of_day" => "morning",
          "system_load" => 0.4,
          "user_expertise" => "advanced"
        }
      }
      
      result = SwarmIntelligenceHandler.learn(payload, %{}, socket)
      
      assert {:reply, {:ok, learning_result}, _socket} = result
      assert Map.has_key?(learning_result, :patterns_learned)
      assert Map.has_key?(learning_result, :confidence_score)
      assert learning_result.confidence_score >= 0.0
      assert learning_result.confidence_score <= 1.0
    end
    
    test "adapts strategies based on learned patterns", %{socket: socket} do
      # First, establish learning pattern
      learning_payload = %{
        "pattern_type" => "domain_optimization",
        "domain" => "healthcare",
        "successful_strategies" => ["minimal_path", "skip_non_critical"],
        "failed_strategies" => ["parallel_execution"]
      }
      
      SwarmIntelligenceHandler.record_pattern(learning_payload, %{}, socket)
      
      # Then request adaptation
      adaptation_payload = %{
        "domain" => "healthcare",
        "current_strategy" => "parallel_execution"
      }
      
      result = SwarmIntelligenceHandler.adapt_strategy(adaptation_payload, %{}, socket)
      
      assert {:reply, {:ok, adaptation}, _socket} = result
      assert adaptation.recommended_strategy in ["minimal_path", "skip_non_critical"]
    end
    
    test "builds knowledge base from multiple executions", %{socket: socket} do
      # Simulate multiple execution learnings
      executions = [
        %{"domain" => "cybersecurity", "success" => true, "duration" => 300},
        %{"domain" => "cybersecurity", "success" => true, "duration" => 320},
        %{"domain" => "healthcare", "success" => true, "duration" => 180},
        %{"domain" => "healthcare", "success" => false, "duration" => 250}
      ]
      
      for execution <- executions do
        SwarmIntelligenceHandler.add_to_knowledge_base(execution, %{}, socket)
      end
      
      result = SwarmIntelligenceHandler.get_knowledge_insights(%{}, %{}, socket)
      
      assert {:reply, {:ok, insights}, _socket} = result
      assert Map.has_key?(insights, :domain_patterns)
      assert Map.has_key?(insights, :success_factors)
    end
  end
  
  describe "contextual recommendations" do
    test "generates recommendations based on current context", %{socket: socket} do
      payload = %{
        "pipeline_state" => %{
          "active_stages" => ["ash", "reactor"],
          "queued_stages" => ["k8s"],
          "failed_stages" => []
        },
        "system_metrics" => %{
          "cpu_usage" => 0.7,
          "memory_usage" => 0.5,
          "network_latency" => 20
        },
        "user_context" => %{
          "experience_level" => "intermediate",
          "time_pressure" => "high",
          "quality_requirements" => "medium"
        }
      }
      
      result = SwarmIntelligenceHandler.generate_contextual_recommendations(payload, %{}, socket)
      
      assert {:reply, {:ok, recommendations}, _socket} = result
      assert Map.has_key?(recommendations, :recommendations)
      assert Map.has_key?(recommendations, :confidence)
      assert is_list(recommendations.recommendations)
    end
    
    test "prioritizes recommendations based on impact", %{socket: socket} do
      payload = %{
        "optimization_candidates" => [
          %{"type" => "skip_stage", "impact" => 0.3, "effort" => 0.1},
          %{"type" => "parallel_execution", "impact" => 0.6, "effort" => 0.4},
          %{"type" => "cache_optimization", "impact" => 0.8, "effort" => 0.7}
        ]
      }
      
      result = SwarmIntelligenceHandler.prioritize_recommendations(payload, %{}, socket)
      
      assert {:reply, {:ok, prioritized}, _socket} = result
      
      # Should be sorted by impact/effort ratio
      impacts = Enum.map(prioritized.recommendations, & &1.priority_score)
      assert impacts == Enum.sort(impacts, &(&1 >= &2))
    end
    
    test "adapts recommendations to user preferences", %{socket: socket} do
      payload = %{
        "user_preferences" => %{
          "prefers_automation" => true,
          "risk_tolerance" => "medium",
          "performance_priority" => "speed"
        },
        "recommendation_pool" => [
          %{"type" => "manual_optimization", "automation_level" => "low"},
          %{"type" => "auto_scaling", "automation_level" => "high"},
          %{"type" => "predictive_caching", "automation_level" => "medium"}
        ]
      }
      
      result = SwarmIntelligenceHandler.adapt_to_preferences(payload, %{}, socket)
      
      assert {:reply, {:ok, adapted}, _socket} = result
      
      # Should prefer high automation recommendations
      high_automation = Enum.filter(adapted.recommendations, 
        &(&1.automation_level == "high"))
      assert length(high_automation) > 0
    end
  end
  
  describe "pattern recognition" do
    test "identifies performance patterns from historical data", %{socket: socket} do
      payload = %{
        "historical_data" => [
          %{"timestamp" => "2023-01-01T09:00:00Z", "duration" => 300, "success" => true},
          %{"timestamp" => "2023-01-01T14:00:00Z", "duration" => 450, "success" => false},
          %{"timestamp" => "2023-01-01T18:00:00Z", "duration" => 280, "success" => true},
          %{"timestamp" => "2023-01-02T09:00:00Z", "duration" => 320, "success" => true}
        ],
        "pattern_types" => ["temporal", "performance", "success_rate"]
      }
      
      result = SwarmIntelligenceHandler.identify_patterns(payload, %{}, socket)
      
      assert {:reply, {:ok, patterns}, _socket} = result
      assert Map.has_key?(patterns, :temporal_patterns)
      assert Map.has_key?(patterns, :performance_trends)
    end
    
    test "detects anomalies in execution patterns", %{socket: socket} do
      payload = %{
        "recent_executions" => [
          %{"duration" => 300, "success" => true},
          %{"duration" => 320, "success" => true},
          %{"duration" => 1200, "success" => false}, # Anomaly
          %{"duration" => 310, "success" => true}
        ],
        "baseline_metrics" => %{
          "avg_duration" => 315,
          "success_rate" => 0.95
        }
      }
      
      result = SwarmIntelligenceHandler.detect_anomalies(payload, %{}, socket)
      
      assert {:reply, {:ok, anomalies}, _socket} = result
      assert length(anomalies.detected_anomalies) > 0
    end
    
    test "learns stage dependency patterns", %{socket: socket} do
      payload = %{
        "execution_traces" => [
          %{"stages" => ["typer", "turtle", "ash", "reactor"], "success" => true},
          %{"stages" => ["typer", "ash", "reactor"], "success" => true},
          %{"stages" => ["turtle", "ash", "reactor"], "success" => false},
          %{"stages" => ["typer", "turtle", "reactor"], "success" => false}
        ]
      }
      
      result = SwarmIntelligenceHandler.learn_dependencies(payload, %{}, socket)
      
      assert {:reply, {:ok, dependencies}, _socket} = result
      assert Map.has_key?(dependencies, :critical_dependencies)
      assert Map.has_key?(dependencies, :optional_stages)
    end
  end
  
  describe "swarm coordination" do
    test "coordinates optimization across multiple agents", %{socket: socket} do
      payload = %{
        "agents" => [
          %{"id" => "agent_1", "specialty" => "pipeline_optimization"},
          %{"id" => "agent_2", "specialty" => "resource_management"},
          %{"id" => "agent_3", "specialty" => "performance_monitoring"}
        ],
        "coordination_goal" => "maximize_efficiency"
      }
      
      result = SwarmIntelligenceHandler.coordinate_agents(payload, %{}, socket)
      
      assert {:reply, {:ok, coordination}, _socket} = result
      assert Map.has_key?(coordination, :agent_assignments)
      assert Map.has_key?(coordination, :coordination_plan)
    end
    
    test "distributes learning across swarm", %{socket: socket} do
      payload = %{
        "learning_update" => %{
          "pattern" => "healthcare_optimization",
          "confidence" => 0.9,
          "applicability" => ["healthcare", "minimal_complexity"]
        },
        "target_agents" => ["agent_1", "agent_2", "agent_3"]
      }
      
      result = SwarmIntelligenceHandler.distribute_learning(payload, %{}, socket)
      
      assert {:reply, {:ok, %{distribution_successful: true}}, _socket} = result
    end
    
    test "synchronizes swarm knowledge base", %{socket: socket} do
      payload = %{
        "sync_type" => "full",
        "knowledge_version" => "1.2.3"
      }
      
      result = SwarmIntelligenceHandler.sync_knowledge(payload, %{}, socket)
      
      assert {:reply, {:ok, sync_result}, _socket} = result
      assert Map.has_key?(sync_result, :sync_completed)
      assert Map.has_key?(sync_result, :knowledge_updated)
    end
  end
  
  describe "predictive analytics" do
    test "predicts optimal execution strategies", %{socket: socket} do
      payload = %{
        "current_context" => %{
          "domain" => "cybersecurity",
          "complexity" => 0.7,
          "time_constraints" => "medium",
          "resource_availability" => 0.8
        },
        "prediction_horizon" => "next_execution"
      }
      
      result = SwarmIntelligenceHandler.predict_optimal_strategy(payload, %{}, socket)
      
      assert {:reply, {:ok, prediction}, _socket} = result
      assert Map.has_key?(prediction, :recommended_strategy)
      assert Map.has_key?(prediction, :confidence)
      assert Map.has_key?(prediction, :expected_outcomes)
    end
    
    test "forecasts system performance under different strategies", %{socket: socket} do
      payload = %{
        "strategies" => ["skip_non_critical", "parallel_execution", "minimal_path"],
        "forecast_params" => %{
          "load_scenario" => "high",
          "time_window" => "1_hour"
        }
      }
      
      result = SwarmIntelligenceHandler.forecast_performance(payload, %{}, socket)
      
      assert {:reply, {:ok, forecasts}, _socket} = result
      assert is_map(forecasts.strategy_forecasts)
      assert Map.has_key?(forecasts, :recommended_strategy)
    end
  end
  
  describe "feedback loop optimization" do
    test "processes user feedback to improve recommendations", %{socket: socket} do
      payload = %{
        "recommendation_id" => "rec_123",
        "user_feedback" => %{
          "satisfaction" => 0.8,
          "effectiveness" => 0.9,
          "ease_of_use" => 0.7,
          "comments" => "Good recommendation but complex to implement"
        }
      }
      
      result = SwarmIntelligenceHandler.process_feedback(payload, %{}, socket)
      
      assert {:reply, {:ok, %{feedback_processed: true}}, _socket} = result
    end
    
    test "adjusts learning algorithms based on feedback", %{socket: socket} do
      payload = %{
        "feedback_summary" => %{
          "avg_satisfaction" => 0.75,
          "recommendation_accuracy" => 0.82,
          "user_adoption_rate" => 0.68
        },
        "adjustment_strategy" => "conservative"
      }
      
      result = SwarmIntelligenceHandler.adjust_learning_parameters(payload, %{}, socket)
      
      assert {:reply, {:ok, adjustment}, _socket} = result
      assert Map.has_key?(adjustment, :parameters_updated)
    end
  end
  
  describe "80/20 intelligence optimization" do
    test "identifies 20% of factors contributing to 80% of performance", %{socket: socket} do
      payload = %{
        "performance_factors" => [
          %{"factor" => "stage_ordering", "impact" => 0.4},
          %{"factor" => "parallel_execution", "impact" => 0.3},
          %{"factor" => "caching", "impact" => 0.15},
          %{"factor" => "resource_allocation", "impact" => 0.1},
          %{"factor" => "monitoring_overhead", "impact" => 0.05}
        ]
      }
      
      result = SwarmIntelligenceHandler.identify_80_20_factors(payload, %{}, socket)
      
      assert {:reply, {:ok, analysis}, _socket} = result
      assert Map.has_key?(analysis, :critical_factors)
      assert Map.has_key?(analysis, :optimization_impact)
      
      # Top factors should account for majority of impact
      top_factors_impact = Enum.reduce(analysis.critical_factors, 0, 
        &(&1.impact + &2))
      assert top_factors_impact >= 0.7 # At least 70% of impact
    end
    
    test "applies 80/20 principle to recommendation prioritization", %{socket: socket} do
      payload = %{
        "recommendations" => [
          %{"id" => "rec_1", "effort" => 0.1, "impact" => 0.4},
          %{"id" => "rec_2", "effort" => 0.3, "impact" => 0.3},
          %{"id" => "rec_3", "effort" => 0.5, "impact" => 0.2},
          %{"id" => "rec_4", "effort" => 0.8, "impact" => 0.1}
        ]
      }
      
      result = SwarmIntelligenceHandler.apply_80_20_prioritization(payload, %{}, socket)
      
      assert {:reply, {:ok, prioritized}, _socket} = result
      
      # Should prioritize high impact, low effort recommendations
      top_recommendation = List.first(prioritized.recommendations)
      assert top_recommendation.impact_effort_ratio >= 4.0 # rec_1: 0.4/0.1 = 4.0
    end
  end
  
  describe "error handling and resilience" do
    test "handles insufficient data gracefully", %{socket: socket} do
      payload = %{
        "insufficient_data" => true,
        "data_points" => 1
      }
      
      result = SwarmIntelligenceHandler.generate_recommendations(payload, %{}, socket)
      
      assert {:reply, {:error, %{reason: "insufficient_data"}}, _socket} = result
    end
    
    test "degrades gracefully when learning models fail", %{socket: socket} do
      payload = %{
        "force_model_failure" => true
      }
      
      result = SwarmIntelligenceHandler.learn(payload, %{}, socket)
      
      # Should fall back to rule-based recommendations
      assert {:reply, {:ok, fallback_result}, _socket} = result
      assert fallback_result.fallback_mode == true
    end
  end
  
  describe "telemetry integration" do
    test "emits telemetry events for learning activities", %{socket: socket} do
      # Set up telemetry handler
      test_pid = self()
      
      :telemetry.attach("test_swarm_learning",
        [:cns, :swarm, :learning, :pattern_learned],
        fn _event, measurements, metadata, _config ->
          send(test_pid, {:telemetry, measurements, metadata})
        end,
        nil
      )
      
      SwarmIntelligenceHandler.learn(%{
        "execution_id" => "telemetry_test",
        "metrics" => %{"duration" => 300},
        "outcomes" => %{"success" => true}
      }, %{}, socket)
      
      # Should receive telemetry event
      assert_receive {:telemetry, measurements, metadata}, 1000
      
      assert measurements.learning_time_ms > 0
      assert metadata.execution_id == "telemetry_test"
      
      :telemetry.detach("test_swarm_learning")
    end
  end
end