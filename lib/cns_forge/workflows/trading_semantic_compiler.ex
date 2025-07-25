defmodule CNSForge.Workflows.TradingSemanticCompiler do
  @moduledoc """
  Semantic compiler for trading domain ontologies
  
  Compiles production_forex_trading.ttl and related trading ontologies into
  executable BitActor workflows using advanced Reactor patterns.
  """
  
  use Reactor

  middlewares do
    middleware CNSForge.ReactorMiddleware
    middleware Reactor.Middleware.Telemetry
  end

  input :semantic_data
  input :ttl, default: 6

  # Step 1: Extract trading-specific semantic patterns
  step :extract_trading_patterns do
    argument :data, input(:semantic_data)
    argument :ttl, input(:ttl)
    
    run fn %{data: data, ttl: ttl} ->
      if ttl <= 0 do
        {:error, :ttl_expired}
      else
        trading_patterns = %{
          instruments: extract_financial_instruments(data.semantic_patterns),
          strategies: extract_trading_strategies(data.semantic_patterns),
          risk_models: extract_risk_models(data.semantic_patterns),
          market_data: extract_market_data_patterns(data.semantic_patterns),
          orders: extract_order_patterns(data.semantic_patterns),
          portfolios: extract_portfolio_patterns(data.semantic_patterns)
        }
        
        {:ok, %{trading_patterns: trading_patterns, ttl: ttl - 1}}
      end
    end
  end

  # Step 2: Compile instrument BitActors using map pattern
  map :compile_instrument_bitactors do
    source result(:extract_trading_patterns, [:trading_patterns, :instruments])
    allow_async? true
    return :instrument_bitactor

    step :create_instrument_bitactor do
      argument :instrument, element(:compile_instrument_bitactors)
      argument :ttl, input(:ttl)
      
      run fn %{instrument: inst, ttl: ttl} ->
        bitactor = %{
          type: :trading_instrument,
          instrument_id: inst.id,
          symbol: inst.symbol,
          asset_class: inst.asset_class,
          semantic_context: inst.semantic_context,
          capabilities: [
            :price_monitoring,
            :volatility_calculation,
            :liquidity_assessment,
            :risk_metrics
          ],
          signal_handlers: compile_instrument_signal_handlers(inst),
          ttl: ttl - 1
        }
        
        {:ok, bitactor}
      end
    end

    collect :instrument_bitactor do
      argument :bitactor, result(:create_instrument_bitactor)
      
      transform fn %{bitactor: actor} ->
        %{
          bitactor: actor,
          domain: :trading_instruments,
          compiled_at: DateTime.utc_now()
        }
      end
    end
  end

  # Step 3: Compile strategy BitActors with switch pattern
  switch :compile_strategy_bitactors do
    on result(:extract_trading_patterns, [:trading_patterns, :strategies])

    matches? &has_algorithmic_strategies?/1 do
      map :compile_algorithmic_strategies do
        source result(:extract_trading_patterns, [:trading_patterns, :strategies])
        allow_async? true
        
        step :create_algo_strategy_bitactor do
          argument :strategy, element(:compile_algorithmic_strategies)
          
          run fn %{strategy: strat} ->
            compile_algorithmic_strategy_bitactor(strat)
          end
        end
      end
    end

    matches? &has_quantitative_strategies?/1 do
      map :compile_quantitative_strategies do
        source result(:extract_trading_patterns, [:trading_patterns, :strategies])
        allow_async? true
        
        step :create_quant_strategy_bitactor do
          argument :strategy, element(:compile_quantitative_strategies)
          
          run fn %{strategy: strat} ->
            compile_quantitative_strategy_bitactor(strat)
          end
        end
      end
    end

    default do
      step :compile_generic_trading_strategies do
        argument :strategies, result(:extract_trading_patterns, [:trading_patterns, :strategies])
        
        run fn %{strategies: strategies} ->
          bitactors = Enum.map(strategies, &compile_generic_strategy_bitactor/1)
          {:ok, bitactors}
        end
      end
    end
  end

  # Step 4: Compile risk management BitActors with group pattern
  group :compile_risk_management do
    before_all &setup_risk_calculation_context/3
    after_all &cleanup_risk_resources/1

    step :compile_portfolio_risk_bitactors do
      argument :risk_models, result(:extract_trading_patterns, [:trading_patterns, :risk_models])
      
      run fn %{risk_models: models} ->
        portfolio_risk_actors = Enum.map(models, fn model ->
          %{
            type: :portfolio_risk_manager,
            risk_model_id: model.id,
            risk_metrics: model.metrics,
            calculation_methods: model.calculation_methods,
            thresholds: model.thresholds,
            capabilities: [
              :var_calculation,
              :stress_testing,
              :scenario_analysis,
              :correlation_monitoring
            ],
            semantic_context: model.semantic_context
          }
        end)
        
        {:ok, portfolio_risk_actors}
      end
    end

    step :compile_position_risk_bitactors do
      argument :risk_models, result(:extract_trading_patterns, [:trading_patterns, :risk_models])
      
      run fn %{risk_models: models} ->
        position_risk_actors = Enum.map(models, fn model ->
          %{
            type: :position_risk_manager,
            model_id: model.id,
            position_limits: model.position_limits,
            exposure_limits: model.exposure_limits,
            capabilities: [
              :position_monitoring,
              :exposure_calculation,
              :limit_enforcement,
              :risk_reporting
            ],
            semantic_context: model.semantic_context
          }
        end)
        
        {:ok, position_risk_actors}
      end
    end

    step :compile_market_risk_bitactors do
      argument :risk_models, result(:extract_trading_patterns, [:trading_patterns, :risk_models])
      
      run fn %{risk_models: models} ->
        market_risk_actors = Enum.map(models, fn model ->
          %{
            type: :market_risk_manager,
            model_id: model.id,
            market_factors: model.market_factors,
            sensitivity_measures: model.sensitivity_measures,
            capabilities: [
              :delta_calculation,
              :gamma_monitoring,
              :vega_assessment,
              :theta_analysis
            ],
            semantic_context: model.semantic_context
          }
        end)
        
        {:ok, market_risk_actors}
      end
    end
  end

  # Step 5: Compile order management BitActors using around pattern
  around :order_management_transaction, &trading_transaction_wrapper/4 do
    
    step :compile_order_router_bitactors do
      argument :order_patterns, result(:extract_trading_patterns, [:trading_patterns, :orders])
      
      run fn %{order_patterns: patterns} ->
        router_actors = Enum.map(patterns.routing_rules, fn rule ->
          %{
            type: :order_router,
            routing_rule_id: rule.id,
            venue_preferences: rule.venue_preferences,
            execution_algorithms: rule.execution_algorithms,
            market_impact_models: rule.market_impact_models,
            capabilities: [
              :smart_order_routing,
              :venue_selection,
              :execution_optimization,
              :latency_management
            ],
            semantic_context: rule.semantic_context
          }
        end)
        
        {:ok, router_actors}
      end
    end

    step :compile_execution_bitactors do
      argument :order_patterns, result(:extract_trading_patterns, [:trading_patterns, :orders])
      
      run fn %{order_patterns: patterns} ->
        execution_actors = Enum.map(patterns.execution_strategies, fn strategy ->
          %{
            type: :execution_manager,
            strategy_id: strategy.id,
            execution_algorithm: strategy.algorithm,
            timing_models: strategy.timing_models,
            market_microstructure: strategy.microstructure_models,
            capabilities: [
              :order_slicing,
              :timing_optimization,
              :market_impact_minimization,
              :execution_reporting
            ],
            semantic_context: strategy.semantic_context
          }
        end)
        
        {:ok, execution_actors}
      end
    end
  end

  # Step 6: Compile market data BitActors
  step :compile_market_data_bitactors do
    argument :market_data_patterns, result(:extract_trading_patterns, [:trading_patterns, :market_data])
    
    run fn %{market_data_patterns: patterns} ->
      market_data_actors = [
        # Price feed BitActors
        %{
          type: :price_feed_manager,
          feed_sources: patterns.price_sources,
          update_frequency: patterns.update_frequency,
          capabilities: [:price_normalization, :feed_failover, :latency_monitoring],
          semantic_context: patterns.semantic_context
        },
        
        # Market depth BitActors
        %{
          type: :market_depth_manager,
          depth_levels: patterns.depth_levels,
          aggregation_rules: patterns.aggregation_rules,
          capabilities: [:depth_aggregation, :liquidity_analysis, :spread_calculation],
          semantic_context: patterns.semantic_context
        },
        
        # Trade tick BitActors
        %{
          type: :trade_tick_manager,
          tick_processing: patterns.tick_processing,
          volume_analysis: patterns.volume_analysis,
          capabilities: [:tick_aggregation, :volume_profiling, :time_sales],
          semantic_context: patterns.semantic_context
        }
      ]
      
      {:ok, market_data_actors}
    end
  end

  # Step 7: Collect all compiled trading BitActors
  collect :aggregate_trading_bitactors do
    argument :instrument_actors, result(:compile_instrument_bitactors)
    argument :strategy_actors, result(:compile_strategy_bitactors)
    argument :risk_actors, result(:compile_risk_management)
    argument :order_actors, result(:order_management_transaction)
    argument :market_data_actors, result(:compile_market_data_bitactors)
    
    transform fn inputs ->
      all_actors = [
        inputs.instrument_actors,
        List.wrap(inputs.strategy_actors),
        inputs.risk_actors.portfolio_risk ++ inputs.risk_actors.position_risk ++ inputs.risk_actors.market_risk,
        inputs.order_actors.router_actors ++ inputs.order_actors.execution_actors,
        inputs.market_data_actors
      ] |> List.flatten()
      
      actor_registry = all_actors
      |> Enum.group_by(& &1.type)
      |> Enum.map(fn {type, actors} ->
        {type, %{
          actors: actors,
          count: length(actors),
          capabilities: aggregate_type_capabilities(actors)
        }}
      end)
      |> Enum.into(%{})
      
      %{
        domain: :trading,
        bitactors: all_actors,
        actor_registry: actor_registry,
        total_count: length(all_actors),
        compilation_stats: %{
          instruments: length(inputs.instrument_actors),
          strategies: count_strategies(inputs.strategy_actors),
          risk_managers: count_risk_managers(inputs.risk_actors),
          order_managers: count_order_managers(inputs.order_actors),
          market_data: length(inputs.market_data_actors)
        }
      }
    end
  end

  return :aggregate_trading_bitactors

  # Helper functions for trading semantic compilation

  defp extract_financial_instruments(patterns) do
    patterns.classes
    |> Enum.filter(fn class ->
      String.contains?(String.downcase(class.name), ["instrument", "security", "asset", "currency"])
    end)
    |> Enum.map(fn class ->
      %{
        id: class.id,
        symbol: extract_symbol(class),
        asset_class: determine_asset_class(class),
        semantic_context: class.semantic_context
      }
    end)
  end

  defp extract_trading_strategies(patterns) do
    patterns.classes
    |> Enum.filter(fn class ->
      String.contains?(String.downcase(class.name), ["strategy", "algorithm", "model"])
    end)
    |> Enum.map(fn class ->
      %{
        id: class.id,
        name: class.name,
        strategy_type: determine_strategy_type(class),
        parameters: extract_strategy_parameters(class),
        semantic_context: class.semantic_context
      }
    end)
  end

  defp extract_risk_models(patterns) do
    patterns.classes
    |> Enum.filter(fn class ->
      String.contains?(String.downcase(class.name), ["risk", "var", "exposure", "limit"])
    end)
    |> Enum.map(fn class ->
      %{
        id: class.id,
        risk_type: determine_risk_type(class),
        metrics: extract_risk_metrics(class),
        calculation_methods: extract_calculation_methods(class),
        thresholds: extract_risk_thresholds(class),
        semantic_context: class.semantic_context
      }
    end)
  end

  defp extract_market_data_patterns(patterns) do
    %{
      price_sources: extract_price_sources(patterns),
      update_frequency: extract_update_frequency(patterns),
      depth_levels: extract_depth_levels(patterns),
      aggregation_rules: extract_aggregation_rules(patterns),
      tick_processing: extract_tick_processing(patterns),
      volume_analysis: extract_volume_analysis(patterns),
      semantic_context: patterns
    }
  end

  defp extract_order_patterns(patterns) do
    %{
      routing_rules: extract_routing_rules(patterns),
      execution_strategies: extract_execution_strategies(patterns),
      semantic_context: patterns
    }
  end

  defp extract_portfolio_patterns(patterns) do
    patterns.classes
    |> Enum.filter(fn class ->
      String.contains?(String.downcase(class.name), ["portfolio", "position", "holding"])
    end)
  end

  defp has_algorithmic_strategies?(strategies) do
    Enum.any?(strategies, fn strategy ->
      String.contains?(String.downcase(strategy.name), ["algo", "algorithm", "automated"])
    end)
  end

  defp has_quantitative_strategies?(strategies) do
    Enum.any?(strategies, fn strategy ->
      String.contains?(String.downcase(strategy.name), ["quant", "mathematical", "statistical"])
    end)
  end

  defp compile_instrument_signal_handlers(instrument) do
    %{
      price_update: create_price_update_handler(instrument),
      volume_update: create_volume_update_handler(instrument),
      volatility_calculation: create_volatility_handler(instrument),
      risk_assessment: create_risk_assessment_handler(instrument)
    }
  end

  defp compile_algorithmic_strategy_bitactor(strategy) do
    %{
      type: :algorithmic_strategy,
      strategy_id: strategy.id,
      algorithm_type: strategy.algorithm_type,
      execution_logic: compile_execution_logic(strategy),
      signal_generation: compile_signal_generation(strategy),
      risk_controls: compile_risk_controls(strategy),
      capabilities: [
        :signal_generation,
        :order_management,
        :risk_monitoring,
        :performance_tracking
      ],
      semantic_context: strategy.semantic_context
    }
  end

  defp compile_quantitative_strategy_bitactor(strategy) do
    %{
      type: :quantitative_strategy,
      strategy_id: strategy.id,
      mathematical_model: strategy.mathematical_model,
      statistical_methods: strategy.statistical_methods,
      backtesting_framework: strategy.backtesting_framework,
      capabilities: [
        :model_execution,
        :statistical_analysis,
        :backtesting,
        :optimization
      ],
      semantic_context: strategy.semantic_context
    }
  end

  defp compile_generic_strategy_bitactor(strategy) do
    %{
      type: :generic_trading_strategy,
      strategy_id: strategy.id,
      strategy_logic: strategy.logic,
      capabilities: [:basic_trading],
      semantic_context: strategy.semantic_context
    }
  end

  defp trading_transaction_wrapper(step_function, args, context, options) do
    # Start trading transaction with atomic order management
    transaction_id = generate_trading_transaction_id()
    trading_context = Map.put(context, :trading_transaction_id, transaction_id)
    
    case step_function.(args, trading_context, options) do
      {:ok, result} ->
        commit_trading_transaction(transaction_id, result)
        {:ok, result}
        
      {:error, reason} ->
        rollback_trading_transaction(transaction_id, reason)
        {:error, reason}
    end
  end

  defp setup_risk_calculation_context(_args, _context, _options) do
    {:ok, %{risk_calculation_session: UUID.uuid4()}}
  end

  defp cleanup_risk_resources(_result) do
    # Cleanup risk calculation resources
    :ok
  end

  # Simplified implementations for helper functions
  defp extract_symbol(_class), do: "UNKNOWN"
  defp determine_asset_class(_class), do: :equity
  defp determine_strategy_type(_class), do: :generic
  defp extract_strategy_parameters(_class), do: %{}
  defp determine_risk_type(_class), do: :market_risk
  defp extract_risk_metrics(_class), do: ["var", "expected_shortfall"]
  defp extract_calculation_methods(_class), do: ["historical_simulation"]
  defp extract_risk_thresholds(_class), do: %{}
  defp extract_price_sources(_patterns), do: ["bloomberg", "refinitiv"]
  defp extract_update_frequency(_patterns), do: "1ms"
  defp extract_depth_levels(_patterns), do: 10
  defp extract_aggregation_rules(_patterns), do: %{}
  defp extract_tick_processing(_patterns), do: %{}
  defp extract_volume_analysis(_patterns), do: %{}
  defp extract_routing_rules(_patterns), do: []
  defp extract_execution_strategies(_patterns), do: []
  defp create_price_update_handler(_instrument), do: &handle_price_update/1
  defp create_volume_update_handler(_instrument), do: &handle_volume_update/1
  defp create_volatility_handler(_instrument), do: &handle_volatility_calculation/1
  defp create_risk_assessment_handler(_instrument), do: &handle_risk_assessment/1
  defp compile_execution_logic(_strategy), do: %{}
  defp compile_signal_generation(_strategy), do: %{}
  defp compile_risk_controls(_strategy), do: %{}
  defp aggregate_type_capabilities(_actors), do: []
  defp count_strategies(_strategy_actors), do: 0
  defp count_risk_managers(_risk_actors), do: 0
  defp count_order_managers(_order_actors), do: 0
  defp generate_trading_transaction_id(), do: UUID.uuid4()
  defp commit_trading_transaction(_id, _result), do: :ok
  defp rollback_trading_transaction(_id, _reason), do: :ok
  
  # Mock signal handlers
  defp handle_price_update(_signal), do: :ok
  defp handle_volume_update(_signal), do: :ok
  defp handle_volatility_calculation(_signal), do: :ok
  defp handle_risk_assessment(_signal), do: :ok
end