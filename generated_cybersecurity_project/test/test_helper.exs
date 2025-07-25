ExUnit.start()

# Set up Ecto for testing
Ecto.Adapters.SQL.Sandbox.mode(Cybersecurity.Repo, :manual)

defmodule Cybersecurity.TestHelper do
  @moduledoc """
  Test helper functions for cybersecurity
  """

  def start_sandbox do
    Ecto.Adapters.SQL.Sandbox.start(Cybersecurity.Repo)
  end

  def stop_sandbox do
    Ecto.Adapters.SQL.Sandbox.stop(Cybersecurity.Repo)
  end

  def create_test_data(resource, attrs \\ %{}) do
    default_attrs = %{
      name: "Test " <> to_string(resource),
      description: "Generated test data",
      status: :active
    }
    
    attrs = Map.merge(default_attrs, attrs)
    Ash.create!(resource, attrs)
  end
end
