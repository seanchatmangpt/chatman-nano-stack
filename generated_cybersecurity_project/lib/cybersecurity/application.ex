defmodule Cybersecurity.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Registry for dynamic processes (80/20: minimal supervision tree)
      {Registry, keys: :unique, name: Cybersecurity.Registry}
    ]

    opts = [strategy: :one_for_one, name: Cybersecurity.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
