defmodule CnsForge.TypedOntology do
  @moduledoc """
  🔄 SWARM 80/20: Type-safe ontology definitions for TTL generation
  Part 1 of pipeline: typer → turtle
  """
  
  # Type definitions for ontology concepts
  defmodule Types do
    @type namespace :: {atom(), String.t()}
    @type class :: %{
      name: String.t(),
      namespace: atom(),
      superclass: String.t() | nil,
      description: String.t() | nil
    }
    @type property :: %{
      name: String.t(),
      namespace: atom(),
      domain: String.t(),
      range: String.t(),
      type: :object | :datatype
    }
    @type relationship :: %{
      subject: String.t(),
      predicate: String.t(),
      object: String.t()
    }
  end
  
  # Define ontology structure
  defstruct namespaces: [],
            classes: [],
            properties: [],
            relationships: []
  
  @doc """
  Create a new typed ontology
  """
  def new do
    %__MODULE__{
      namespaces: default_namespaces(),
      classes: [],
      properties: [],
      relationships: []
    }
  end
  
  @doc """
  Add a namespace to the ontology
  """
  def add_namespace(%__MODULE__{} = ontology, prefix, uri) when is_atom(prefix) do
    namespace = {prefix, uri}
    %{ontology | namespaces: [namespace | ontology.namespaces]}
  end
  
  @doc """
  Add a typed class to the ontology
  """
  def add_class(%__MODULE__{} = ontology, name, namespace, opts \\ []) do
    class = %{
      name: name,
      namespace: namespace,
      superclass: Keyword.get(opts, :superclass),
      description: Keyword.get(opts, :description)
    }
    %{ontology | classes: [class | ontology.classes]}
  end
  
  @doc """
  Add a typed property to the ontology
  """
  def add_property(%__MODULE__{} = ontology, name, namespace, domain, range, type \\ :object) do
    property = %{
      name: name,
      namespace: namespace,
      domain: domain,
      range: range,
      type: type
    }
    %{ontology | properties: [property | ontology.properties]}
  end
  
  @doc """
  Add a relationship triple
  """
  def add_relationship(%__MODULE__{} = ontology, subject, predicate, object) do
    relationship = %{
      subject: subject,
      predicate: predicate,
      object: object
    }
    %{ontology | relationships: [relationship | ontology.relationships]}
  end
  
  # Default namespaces
  defp default_namespaces do
    [
      {:rdf, "http://www.w3.org/1999/02/22-rdf-syntax-ns#"},
      {:rdfs, "http://www.w3.org/2000/01/rdf-schema#"},
      {:owl, "http://www.w3.org/2002/07/owl#"},
      {:xsd, "http://www.w3.org/2001/XMLSchema#"}
    ]
  end
end