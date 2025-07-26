ExUnit.start()

# 80/20: No database setup needed for ETS-based resources

defmodule Cybersecurity.TestHelper do
  @moduledoc """
  Test helper functions for cybersecurity
  """

  def start_sandbox do
    # No-op for ETS-based resources
    :ok
  end

  def stop_sandbox do
    # No-op for ETS-based resources  
    :ok
  end

  def create_test_data(resource, attrs \\ %{}) do
    default_attrs = %{
      name: "Test " <> to_string(resource),
      description: "Generated test data",
      status: :active
    }
    
    attrs = Map.merge(default_attrs, attrs)
    resource.init_storage()
    resource.create(attrs)
  end
end
