defmodule Ultrathink do
  @moduledoc """
  Ultrathink - Generated from ultrathink ontology
  
  This is a complete Ash.Reactor application generated from TTL ontology.
  It provides domain-driven resources, workflows, and business logic.
  """

  @doc """
  Execute a reactor workflow with the given input
  """
  def run_workflow(workflow_module, input \\ %{}) do
    Reactor.run(workflow_module, input)
  end

  @doc """
  List all available resources in the domain
  """
  def list_resources do
    Ultrathink.Domain.resource_names()
  end

  @doc """
  Get domain info
  """
  def domain_info do
    %{
      name: "ultrathink",
      module: Ultrathink.Domain,
      resources: list_resources(),
      generated_at: DateTime.utc_now()
    }
  end
end
