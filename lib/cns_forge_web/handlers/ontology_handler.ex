defmodule CnsForgeWeb.Handlers.OntologyHandler do
  @moduledoc """
  Handler for Typed Ontology stage in the ULTRATHINK pipeline.
  Manages ontology creation, validation, and transformation.
  """
  
  use ChannelHandler.Handler
  
  alias CnsForge.TypedOntology
  alias CnsForgeWeb.NotificationBroadcaster
  
  require Logger
  
  # Plugs for all actions
  plug CnsForgeWeb.ChannelPlugs.ValidatePayload
  plug CnsForgeWeb.ChannelPlugs.TrackStageMetrics
  
  # Specific plug for create action
  plug CnsForgeWeb.ChannelPlugs.CheckPermission, :create_ontology when action in [:create]
  
  @doc """
  Create a new typed ontology
  """
  def create(payload, _bindings, socket) do
    Logger.info("Creating typed ontology for pipeline #{socket.assigns.pipeline_id}")
    
    with {:ok, validated} <- validate_ontology_params(payload),
         {:ok, ontology} <- create_typed_ontology(validated) do
      
      # Store in pipeline state
      socket = assign(socket, :ontology, ontology)
      
      # Broadcast creation event
      NotificationBroadcaster.broadcast_stage_event(socket, "ontology:created", %{
        ontology_id: ontology.id,
        namespaces: length(Keyword.keys(ontology.namespaces)),
        classes: length(ontology.classes),
        properties: length(ontology.properties)
      })
      
      {:reply, {:ok, %{
        message: "Ontology created successfully",
        ontology_id: ontology.id,
        stats: ontology_stats(ontology)
      }}, socket}
    else
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  @doc """
  Validate an existing ontology
  """
  def validate(payload, _bindings, socket) do
    Logger.info("Validating ontology for pipeline #{socket.assigns.pipeline_id}")
    
    ontology = socket.assigns[:ontology] || payload["ontology"]
    
    case TypedOntology.validate(ontology) do
      {:ok, validation_result} ->
        NotificationBroadcaster.broadcast_stage_event(socket, "ontology:validated", %{
          valid: validation_result.valid?,
          warnings: validation_result.warnings,
          errors: validation_result.errors
        })
        
        {:reply, {:ok, validation_result}, socket}
        
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  @doc """
  Transform ontology for next stage (Turtle/TTL)
  """
  def transform(payload, _bindings, socket) do
    Logger.info("Transforming ontology for pipeline #{socket.assigns.pipeline_id}")
    
    ontology = socket.assigns[:ontology] || payload["ontology"]
    transform_options = Map.get(payload, "options", %{})
    
    with {:ok, transformed} <- prepare_for_turtle(ontology, transform_options) do
      socket = assign(socket, :ontology_transformed, transformed)
      
      NotificationBroadcaster.broadcast_stage_event(socket, "ontology:transformed", %{
        format: "turtle-ready",
        size: byte_size(transformed.data)
      })
      
      # Mark stage as complete
      socket = update_in(socket.assigns.stages_completed, &([:ontology | &1]))
      
      {:reply, {:ok, %{
        message: "Ontology transformed successfully",
        ready_for: "turtle_generation",
        preview: String.slice(transformed.data, 0..200) <> "..."
      }}, socket}
    else
      {:error, reason} ->
        {:reply, {:error, %{reason: reason}}, socket}
    end
  end
  
  @doc """
  Generic handler for delegated events
  """
  def handle_in(event, payload, bindings, socket) do
    Logger.debug("Handling ontology event: #{event}")
    
    case String.split(event, ":") do
      ["namespace", action] ->
        handle_namespace_action(action, payload, socket)
        
      ["class", action] ->
        handle_class_action(action, payload, socket)
        
      ["property", action] ->
        handle_property_action(action, payload, socket)
        
      _ ->
        {:reply, {:error, %{reason: "Unknown ontology event: #{event}"}}, socket}
    end
  end
  
  # Private functions
  
  defp validate_ontology_params(params) do
    required = ~w(name domain)
    
    case Enum.filter(required, &(not Map.has_key?(params, &1))) do
      [] ->
        {:ok, params}
      missing ->
        {:error, "Missing required fields: #{Enum.join(missing, ", ")}"}
    end
  end
  
  defp create_typed_ontology(params) do
    ontology = TypedOntology.new()
    
    # Add default namespaces
    ontology = ontology
    |> TypedOntology.add_namespace(:rdf, "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    |> TypedOntology.add_namespace(:rdfs, "http://www.w3.org/2000/01/rdf-schema#")
    |> TypedOntology.add_namespace(:owl, "http://www.w3.org/2002/07/owl#")
    |> TypedOntology.add_namespace(:xsd, "http://www.w3.org/2001/XMLSchema#")
    
    # Add domain namespace
    ontology = TypedOntology.add_namespace(
      ontology,
      String.to_atom(params["name"]),
      params["domain"]
    )
    
    # Add initial classes if provided
    ontology = Enum.reduce(params["classes"] || [], ontology, fn class_def, acc ->
      TypedOntology.add_class(
        acc,
        class_def["name"],
        String.to_atom(params["name"]),
        description: class_def["description"]
      )
    end)
    
    {:ok, Map.put(ontology, :id, generate_ontology_id())}
  end
  
  defp prepare_for_turtle(ontology, options) do
    # Transform ontology structure for Turtle generation
    transformed = %{
      id: ontology.id,
      namespaces: ontology.namespaces,
      classes: transform_classes(ontology.classes),
      properties: transform_properties(ontology.properties),
      metadata: %{
        created_at: DateTime.utc_now(),
        version: "1.0.0",
        format: "typed_ontology"
      },
      data: generate_preview_data(ontology)
    }
    
    {:ok, transformed}
  end
  
  defp transform_classes(classes) do
    Enum.map(classes, fn class ->
      Map.merge(class, %{
        uri: generate_class_uri(class),
        turtle_ready: true
      })
    end)
  end
  
  defp transform_properties(properties) do
    Enum.map(properties, fn property ->
      Map.merge(property, %{
        uri: generate_property_uri(property),
        turtle_ready: true
      })
    end)
  end
  
  defp handle_namespace_action("add", payload, socket) do
    ontology = socket.assigns.ontology
    
    updated = TypedOntology.add_namespace(
      ontology,
      String.to_atom(payload["prefix"]),
      payload["uri"]
    )
    
    socket = assign(socket, :ontology, updated)
    {:reply, {:ok, %{message: "Namespace added"}}, socket}
  end
  
  defp handle_namespace_action("remove", payload, socket) do
    # Implementation for removing namespace
    {:reply, {:ok, %{message: "Namespace removed"}}, socket}
  end
  
  defp handle_class_action("add", payload, socket) do
    ontology = socket.assigns.ontology
    
    updated = TypedOntology.add_class(
      ontology,
      payload["name"],
      String.to_atom(payload["namespace"]),
      description: payload["description"]
    )
    
    socket = assign(socket, :ontology, updated)
    {:reply, {:ok, %{message: "Class added", class_count: length(updated.classes)}}, socket}
  end
  
  defp handle_class_action("update", payload, socket) do
    # Implementation for updating class
    {:reply, {:ok, %{message: "Class updated"}}, socket}
  end
  
  defp handle_property_action("add", payload, socket) do
    ontology = socket.assigns.ontology
    
    updated = TypedOntology.add_property(
      ontology,
      payload["name"],
      String.to_atom(payload["namespace"]),
      payload["domain"],
      payload["range"],
      description: payload["description"]
    )
    
    socket = assign(socket, :ontology, updated)
    {:reply, {:ok, %{message: "Property added", property_count: length(updated.properties)}}, socket}
  end
  
  defp ontology_stats(ontology) do
    %{
      namespaces: length(Keyword.keys(ontology.namespaces)),
      classes: length(ontology.classes),
      properties: length(ontology.properties),
      relationships: count_relationships(ontology)
    }
  end
  
  defp count_relationships(ontology) do
    # Count various relationship types in the ontology
    Enum.reduce(ontology.properties, 0, fn prop, acc ->
      if prop[:type] == :object_property, do: acc + 1, else: acc
    end)
  end
  
  defp generate_ontology_id do
    "ont_#{:crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower)}"
  end
  
  defp generate_class_uri(class) do
    namespace = class[:namespace] || :default
    "#{namespace}:#{class.name}"
  end
  
  defp generate_property_uri(property) do
    namespace = property[:namespace] || :default
    "#{namespace}:#{property.name}"
  end
  
  defp generate_preview_data(ontology) do
    # Generate a preview of the ontology data
    """
    # Typed Ontology Preview
    Namespaces: #{length(Keyword.keys(ontology.namespaces))}
    Classes: #{length(ontology.classes)}
    Properties: #{length(ontology.properties)}
    
    Sample classes:
    #{Enum.take(ontology.classes, 3) |> Enum.map(&("- #{&1.name}")) |> Enum.join("\n")}
    """
  end
end