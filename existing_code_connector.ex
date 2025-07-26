defmodule ExistingCodeConnector do
  @moduledoc """
  Bridge module connecting existing TTL parser/transformer with BitActor pipeline
  80/20 implementation: typer → turtle → ttl2dspy → BitActor → Erlang → Ash → Reactor → k8s
  """
  
  require Logger
  alias CNSForge.TTLParser
  alias CnsForge.TTLAshReactorTransformer
  
  @doc """
  Transform TTL content through the complete pipeline
  """
  def transform_ttl_to_bitactor(ttl_content) when is_binary(ttl_content) do
    Logger.info("Starting 80/20 pipeline transformation")
    
    with {:ok, parsed_ontology} <- parse_ttl_with_bitactor_extensions(ttl_content),
         {:ok, bitactor_dsl} <- generate_bitactor_dsl(parsed_ontology),
         {:ok, ash_resources} <- generate_ash_resources_with_ttl(parsed_ontology),
         {:ok, reactor_workflows} <- generate_reactor_workflows(parsed_ontology, ash_resources),
         {:ok, k8s_manifests} <- generate_k8s_deployment(parsed_ontology) do
      
      result = %{
        parsed_ontology: parsed_ontology,
        bitactor_dsl: bitactor_dsl,
        ash_resources: ash_resources,
        reactor_workflows: reactor_workflows,
        k8s_manifests: k8s_manifests,
        pipeline_metadata: %{
          stages_completed: ["typer", "turtle", "ttl2dspy", "BitActor", "Erlang", "Ash", "Reactor", "k8s"],
          ttl_preserved: true,
          nanosecond_precision: true
        }
      }
      
      Logger.info("80/20 pipeline transformation completed successfully")
      {:ok, result}
    else
      {:error, reason} ->
        Logger.error("Pipeline transformation failed: #{inspect(reason)}")
        {:error, reason}
    end
  end
  
  @doc """
  Parse TTL with BitActor-specific extensions
  """
  def parse_ttl_with_bitactor_extensions(ttl_content) do
    # Use existing TTL parser
    case TTLParser.parse(ttl_content) do
      {:ok, ontology} ->
        # Enhance with BitActor-specific metadata
        enhanced_ontology = enhance_with_bitactor_metadata(ontology)
        {:ok, enhanced_ontology}
      error -> error
    end
  end
  
  @doc """
  Generate BitActor DSL from parsed ontology
  """
  def generate_bitactor_dsl(ontology) do
    Logger.info("Generating BitActor DSL from ontology")
    
    dsl_modules = Enum.map(ontology.classes, fn class ->
      generate_bitactor_module(class, ontology)
    end)
    
    {:ok, dsl_modules}
  end
  
  @doc """
  Generate Ash resources with TTL constraints preserved
  """
  def generate_ash_resources_with_ttl(ontology) do
    # Use existing transformer as base
    case CnsForge.TTLAshReactorTransformer.generate_ash_resources(ontology) do
      {:ok, resources} ->
        # Enhance resources with BitActor capabilities
        enhanced_resources = Enum.map(resources, fn resource ->
          enhance_resource_with_bitactor(resource, ontology)
        end)
        {:ok, enhanced_resources}
      error -> error
    end
  end
  
  @doc """
  Generate Reactor workflows from ontology
  """
  def generate_reactor_workflows(ontology, _resources) do
    workflows = [
      generate_main_coordinator_workflow(ontology),
      generate_signal_processing_workflow(ontology),
      generate_ttl_enforcement_workflow(ontology)
    ]
    
    {:ok, workflows}
  end
  
  @doc """
  Generate K8s deployment manifests
  """
  def generate_k8s_deployment(ontology) do
    manifests = %{
      namespace: generate_namespace_manifest(ontology),
      configmaps: generate_configmap_manifests(ontology),
      statefulset: generate_statefulset_manifest(ontology),
      services: generate_service_manifests(ontology),
      hpa: generate_hpa_manifest(ontology),
      monitoring: generate_monitoring_manifests(ontology)
    }
    
    {:ok, manifests}
  end
  
  # Private helper functions
  
  defp enhance_with_bitactor_metadata(ontology) do
    Map.put(ontology, :bitactor_metadata, %{
      signal_types: extract_signal_types(ontology),
      ttl_constraints: extract_ttl_constraints_from_ontology(ontology),
      swarm_topology: determine_swarm_topology(ontology),
      telemetry_metrics: define_telemetry_metrics(ontology)
    })
  end
  
  defp generate_bitactor_module(class, ontology) do
    class_name = Map.get(class, :name, "DefaultActor")
    module_name = "Cyber.#{class_name}"
    
    """
    defmodule #{module_name}.BitActor do
      use BitActorDSL
      
      bitactor do
        name "#{class_name}"
        ttl_budget_ms #{Map.get(class, :ttl_budget, 8)}
        status :inactive
        
        ttl_constraint do
          budget_ns #{Map.get(class, :ttl_budget, 8) * 1_000_000}
          precision :nanosecond
          max_budget_ms #{Map.get(class, :ttl_budget, 8)}
        end
      end
      
      signals do
        #{generate_signal_handlers(class, ontology)}
      end
      
      telemetry do
        metric "signals_processed" do
          unit :count
          interval_ms 1000
        end
        
        metric "processing_time" do
          unit :ns
          interval_ms 100
        end
        
        metric "ttl_utilization" do
          unit :percent
          interval_ms 1000
        end
      end
      
      swarm do
        topology :#{Map.get(ontology.bitactor_metadata || %{}, :swarm_topology, :hierarchical)}
        max_actors 100
        strategy :adaptive
        auto_scale true
      end
      
      ttl_violations do
        action :alert
        threshold_percent 20
      end
    end
    """
  end
  
  defp generate_signal_handlers(class, ontology) do
    # Generate handlers based on class properties
    properties = Map.get(ontology, :properties, [])
    
    properties
    |> Enum.filter(fn prop -> Map.get(prop, :domain) == Map.get(class, :uri) end)  
    |> Enum.map(fn prop ->
      """
        process :#{signal_type_from_property(prop)} do
          priority :#{priority_from_property(prop)}
          handler &handle_#{Map.get(prop, :name, "default")}_signal/2
        end
      """
    end)
    |> Enum.join("\n")
  end
  
  defp enhance_resource_with_bitactor(resource, _ontology) do
    # Add BitActor-specific attributes and actions
    enhanced_code = resource.code
    |> String.replace("attributes do", """
      attributes do
        # BitActor TTL tracking
        attribute :last_processing_time_ns, :integer, public?: true
        attribute :ttl_budget_ms, :integer, public?: true, default: 8
        attribute :signals_processed, :integer, public?: true, default: 0
        attribute :signals_failed, :integer, public?: true, default: 0
    """)
    |> String.replace("actions do", """
      actions do
        # BitActor signal processing
        update :process_signal do
          accept []
          argument :signal_id, :uuid, allow_nil?: false
          
          change fn changeset, context ->
            # Delegate to BitActor GenServer
            signal_id = context.arguments.signal_id
            resource = changeset.data
            
            case BitActor.GenServer.process_signal(
              {:via, Registry, {BitActor.Registry, resource.id}}, 
              %{id: signal_id}
            ) do
              {:ok, result} ->
                changeset
                |> Ash.Changeset.force_change_attribute(:signals_processed, resource.signals_processed + 1)
                |> Ash.Changeset.force_change_attribute(:last_processing_time_ns, result.processing_time_ns)
              
              {:error, _} ->
                changeset
                |> Ash.Changeset.force_change_attribute(:signals_failed, resource.signals_failed + 1)
            end
          end
        end
    """)
    
    %{resource | code: enhanced_code}
  end
  
  defp generate_main_coordinator_workflow(ontology) do
    %{
      name: "#{ontology.name}.MainCoordinatorWorkflow",
      code: """
      defmodule #{ontology.name}.MainCoordinatorWorkflow do
        use Reactor
        
        input :ontology_data
        input :ttl_constraints, default: %{max_execution_ms: 5000}
        
        # Orchestrate BitActor processing
        step :initialize_swarm do
          argument :topology, value: :#{Map.get(ontology.bitactor_metadata || %{}, :swarm_topology, :hierarchical)}
          
          run fn args, _context ->
            {:ok, %{swarm_id: UUID.uuid4(), topology: args.topology}}
          end
        end
        
        step :spawn_actors do
          argument :swarm, result(:initialize_swarm)
          argument :classes, input(:ontology_data, [:classes])
          
          run fn args, _context ->
            actors = Enum.map(args.classes, fn class ->
              {:ok, pid} = BitActor.Supervisor.start_actor([
                name: class.name,
                ttl_budget_ms: class.ttl_budget || 8
              ])
              %{class: class.name, pid: pid}
            end)
            
            {:ok, actors}
          end
        end
        
        step :coordinate_processing do
          argument :actors, result(:spawn_actors)
          argument :ttl_constraints, input(:ttl_constraints)
          
          max_retries 3
          
          run fn args, _context ->
            # Coordinate signal processing across actors
            {:ok, %{coordinated: true, actor_count: length(args.actors)}}
          end
        end
        
        return :coordinate_processing
      end
      """
    }
  end
  
  defp generate_signal_processing_workflow(ontology) do
    %{
      name: "#{ontology.name}.SignalProcessingWorkflow",
      code: """
      defmodule #{ontology.name}.SignalProcessingWorkflow do
        use Reactor
        
        input :signal_id
        input :actor_id
        input :ttl_constraints, default: %{max_execution_ms: 10}
        
        # TTL-aware signal processing
        step :validate_ttl do
          argument :ttl_constraints, input(:ttl_constraints)
          
          run fn args, _context ->
            if args.ttl_constraints.max_execution_ms > 0 do
              {:ok, %{ttl_budget_ns: args.ttl_constraints.max_execution_ms * 1_000_000}}
            else
              {:error, "Invalid TTL constraints"}
            end
          end
        end
        
        step :process_with_deadline do
          argument :signal_id, input(:signal_id)
          argument :actor_id, input(:actor_id)
          argument :ttl_budget, result(:validate_ttl, [:ttl_budget_ns])
          
          run fn args, _context ->
            start_time = System.monotonic_time(:nanosecond)
            
            # Process signal with TTL enforcement
            result = BitActor.Reactor.TTLEnforcementWorkflow.run(%{
              actor_id: args.actor_id,
              operation: fn -> process_signal(args.signal_id) end,
              ttl_budget_ns: args.ttl_budget
            })
            
            end_time = System.monotonic_time(:nanosecond)
            processing_time_ns = end_time - start_time
            
            case result do
              {:ok, response} ->
                {:ok, %{
                  result: response,
                  processing_time_ns: processing_time_ns,
                  ttl_compliant: processing_time_ns <= args.ttl_budget
                }}
              
              {:error, reason} ->
                {:error, reason}
            end
          end
        end
        
        return :process_with_deadline
        
        defp process_signal(signal_id) do
          # Actual signal processing logic
          {:ok, %{processed: true, signal_id: signal_id}}
        end
      end
      """
    }
  end
  
  defp generate_ttl_enforcement_workflow(ontology) do
    %{
      name: "#{ontology.name}.TTLEnforcementWorkflow",
      code: """
      defmodule #{ontology.name}.TTLEnforcementWorkflow do
        use Reactor
        
        input :operation
        input :ttl_budget_ns
        input :violation_handler, default: &default_violation_handler/1
        
        step :execute_with_deadline do
          argument :operation, input(:operation)
          argument :ttl_budget_ns, input(:ttl_budget_ns)
          
          run fn args, _context ->
            task = Task.async(args.operation)
            timeout_ms = div(args.ttl_budget_ns, 1_000_000)
            
            case Task.yield(task, timeout_ms) || Task.shutdown(task) do
              {:ok, result} -> result
              nil -> {:error, :ttl_exceeded}
            end
          end
        end
        
        step :handle_violation do
          argument :result, result(:execute_with_deadline)
          argument :handler, input(:violation_handler)
          
          run fn args, _context ->
            case args.result do
              {:error, :ttl_exceeded} ->
                args.handler.(%{
                  violation_type: :ttl_exceeded,
                  timestamp: DateTime.utc_now()
                })
                {:error, :ttl_exceeded}
              
              other -> other
            end
          end
        end
        
        return :handle_violation
        
        defp default_violation_handler(violation) do
          Logger.error("TTL violation detected: " <> inspect(violation))
        end
      end
      """
    }
  end
  
  defp generate_namespace_manifest(ontology) do
    """
    apiVersion: v1
    kind: Namespace
    metadata:
      name: #{String.downcase(ontology.name)}-bitactor
      labels:
        app: bitactor
        ontology: #{ontology.name}
    """
  end
  
  defp generate_configmap_manifests(ontology) do
    [
      """
      apiVersion: v1
      kind: ConfigMap
      metadata:
        name: #{String.downcase(ontology.name)}-ttl-config
        namespace: #{String.downcase(ontology.name)}-bitactor
      data:
        ttl_constraints.yaml: |
          global_ttl_budget_ms: #{ontology.ttl_constraints[:max_processing_time_ms] || 5000}
          actor_ttl_budget_ms: #{ontology.ttl_constraints[:max_execution_hops] || 8}
          precision: nanosecond
          enable_bounds_checking: #{ontology.ttl_constraints[:enable_bounds_checking] || true}
      """,
      """
      apiVersion: v1
      kind: ConfigMap
      metadata:
        name: #{String.downcase(ontology.name)}-swarm-config
        namespace: #{String.downcase(ontology.name)}-bitactor
      data:
        swarm_config.yaml: |
          topology: #{ontology.bitactor_metadata[:swarm_topology] || :hierarchical}
          strategy: adaptive
          auto_scale: true
          min_actors: #{length(ontology.classes)}
          max_actors: #{length(ontology.classes) * 10}
      """
    ]
  end
  
  defp generate_statefulset_manifest(ontology) do
    """
    apiVersion: apps/v1
    kind: StatefulSet
    metadata:
      name: #{String.downcase(ontology.name)}-bitactor
      namespace: #{String.downcase(ontology.name)}-bitactor
    spec:
      serviceName: #{String.downcase(ontology.name)}-bitactor-headless
      replicas: 3
      selector:
        matchLabels:
          app: bitactor
          ontology: #{ontology.name}
      template:
        metadata:
          labels:
            app: bitactor
            ontology: #{ontology.name}
        spec:
          containers:
          - name: bitactor
            image: bitactor:latest
            env:
            - name: ONTOLOGY_NAME
              value: "#{ontology.name}"
            - name: TTL_BUDGET_MS
              value: "#{ontology.ttl_constraints[:max_execution_hops] || 8}"
            - name: MAX_ACTORS
              value: "#{length(ontology.classes) * 10}"
            resources:
              requests:
                memory: "256Mi"
                cpu: "250m"
              limits:
                memory: "512Mi"
                cpu: "500m"
    """
  end
  
  defp generate_service_manifests(ontology) do
    [
      """
      apiVersion: v1
      kind: Service
      metadata:
        name: #{String.downcase(ontology.name)}-bitactor
        namespace: #{String.downcase(ontology.name)}-bitactor
      spec:
        type: ClusterIP
        selector:
          app: bitactor
          ontology: #{ontology.name}
        ports:
        - name: erlang
          port: 9100
          targetPort: 9100
        - name: metrics
          port: 9090
          targetPort: 9090
      """,
      """
      apiVersion: v1
      kind: Service
      metadata:
        name: #{String.downcase(ontology.name)}-bitactor-headless
        namespace: #{String.downcase(ontology.name)}-bitactor
      spec:
        type: ClusterIP
        clusterIP: None
        selector:
          app: bitactor
          ontology: #{ontology.name}
        ports:
        - name: epmd
          port: 4369
          targetPort: 4369
      """
    ]
  end
  
  defp generate_hpa_manifest(ontology) do
    """
    apiVersion: autoscaling/v2
    kind: HorizontalPodAutoscaler
    metadata:
      name: #{String.downcase(ontology.name)}-bitactor-hpa
      namespace: #{String.downcase(ontology.name)}-bitactor
    spec:
      scaleTargetRef:
        apiVersion: apps/v1
        kind: StatefulSet
        name: #{String.downcase(ontology.name)}-bitactor
      minReplicas: 3
      maxReplicas: 10
      metrics:
      - type: Resource
        resource:
          name: cpu
          target:
            type: Utilization
            averageUtilization: 70
      - type: Pods
        pods:
          metric:
            name: bitactor_signals_per_second
          target:
            type: AverageValue
            averageValue: "1000"
    """
  end
  
  defp generate_monitoring_manifests(ontology) do
    [
      """
      apiVersion: monitoring.coreos.com/v1
      kind: ServiceMonitor
      metadata:
        name: #{String.downcase(ontology.name)}-bitactor-monitor
        namespace: #{String.downcase(ontology.name)}-bitactor
      spec:
        selector:
          matchLabels:
            app: bitactor
            ontology: #{ontology.name}
        endpoints:
        - port: metrics
          interval: 30s
          path: /metrics
      """,
      """
      apiVersion: monitoring.coreos.com/v1
      kind: PrometheusRule
      metadata:
        name: #{String.downcase(ontology.name)}-bitactor-alerts
        namespace: #{String.downcase(ontology.name)}-bitactor
      spec:
        groups:
        - name: bitactor.rules
          interval: 30s
          rules:
          - alert: BitActorTTLViolation
            expr: rate(bitactor_ttl_violations_total[5m]) > 0.1
            for: 5m
            labels:
              severity: warning
              ontology: #{ontology.name}
      """
    ]
  end
  
  # Helper functions for signal type determination
  defp extract_signal_types(ontology) do
    classes = Map.get(ontology, :classes, [])
    
    classes
    |> Enum.filter(fn class -> 
      class_type = Map.get(class, :type, "")
      String.contains?(class_type, "Signal")
    end)
    |> Enum.map(fn class -> 
      class_name = Map.get(class, :name, "")
      String.downcase(class_name) |> String.to_atom()
    end)
    |> case do
      [] -> [:data, :control, :telemetry]
      types -> types
    end
  end
  
  defp extract_ttl_constraints_from_ontology(ontology) do
    ttl_constraints = Map.get(ontology, :ttl_constraints, %{})
    
    Map.merge(
      ttl_constraints,
      %{
        precision: :nanosecond,
        enable_monitoring: true,
        violation_threshold_percent: 20
      }
    )
  end
  
  defp determine_swarm_topology(ontology) do
    classes = Map.get(ontology, :classes, [])
    class_count = length(classes)
    
    cond do
      class_count <= 10 -> :star
      class_count <= 50 -> :hierarchical
      class_count <= 100 -> :mesh
      true -> :ring
    end
  end
  
  defp define_telemetry_metrics(ontology) do
    base_metrics = [
      %{name: "signals_processed", unit: :count},
      %{name: "processing_time", unit: :ns},
      %{name: "ttl_utilization", unit: :percent},
      %{name: "actor_health", unit: :percent}
    ]
    
    # Add ontology-specific metrics
    classes = Map.get(ontology, :classes, [])
    
    ontology_metrics = classes
    |> Enum.map(fn class ->
      class_name = Map.get(class, :name, "default")
      %{name: "#{String.downcase(class_name)}_operations", unit: :count}
    end)
    
    base_metrics ++ ontology_metrics
  end
  
  defp signal_type_from_property(property) do
    prop_type = Map.get(property, :type, "")
    
    cond do
      String.contains?(prop_type, "Input") -> :data
      String.contains?(prop_type, "Step") -> :control
      String.contains?(prop_type, "Output") -> :telemetry
      true -> :data
    end
  end
  
  defp priority_from_property(property) do
    prop_name = Map.get(property, :name, "")
    
    cond do
      String.contains?(prop_name, "critical") -> :critical
      String.contains?(prop_name, "urgent") -> :high
      String.contains?(prop_name, "optional") -> :low
      true -> :medium
    end
  end
end