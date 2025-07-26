ExUnit.start()

# Set up Ecto for testing
Ecto.Adapters.SQL.Sandbox.mode(Ultrathink.Repo, :manual)

defmodule Ultrathink.TestHelper do
  @moduledoc """
  Test helper functions for ultrathink
  """

  def start_sandbox do
    Ecto.Adapters.SQL.Sandbox.start(Ultrathink.Repo)
  end

  def stop_sandbox do
    Ecto.Adapters.SQL.Sandbox.stop(Ultrathink.Repo)
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
