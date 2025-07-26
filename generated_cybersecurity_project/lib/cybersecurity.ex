defmodule Cybersecurity do
  @moduledoc """
  Cybersecurity - Generated from cybersecurity ontology
  
  This is a complete Ash.Reactor application generated from TTL ontology.
  It provides domain-driven resources, workflows, and business logic.
  """

  @doc """
  Execute a workflow with the given input (80/20: plain Elixir implementation)
  """
  def run_workflow(workflow_module, operation, data \\ %{}, context \\ %{}) do
    workflow_module.execute(operation, data, context)
  end

  @doc """
  List all available resources in the domain
  """
  def list_resources do
    Cybersecurity.Domain.resource_names()
  end

  @doc """
  Get domain info
  """
  def domain_info do
    %{
      name: "cybersecurity",
      module: Cybersecurity.Domain,
      resources: list_resources(),
      generated_at: DateTime.utc_now()
    }
  end
end
