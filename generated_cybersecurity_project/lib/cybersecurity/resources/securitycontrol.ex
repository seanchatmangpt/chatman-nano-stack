defmodule Cybersecurity.Resources.SecurityControl do
  @moduledoc """
  Security Control Resource
  Protective measure or safeguard
  """

  # 80/20: Plain Elixir struct instead of Ash.Resource for simplicity
  defstruct [:id, :name, :description, :status, :metadata, :inserted_at, :updated_at]
  
  @type t :: %__MODULE__{
    id: String.t(),
    name: String.t(),
    description: String.t() | nil,
    status: atom(),
    metadata: map(),
    inserted_at: DateTime.t(),
    updated_at: DateTime.t()
  }

  # Simple CRUD operations using ETS
  def create(attrs) do
    now = DateTime.utc_now()
    id = UUID.uuid4()
    
    record = struct(__MODULE__, Map.merge(attrs, %{
      id: id,
      status: :active,
      metadata: %{},
      inserted_at: now,
      updated_at: now
    }))
    
    :ets.insert(__MODULE__, {id, record})
    {:ok, record}
  end

  def get(id) do
    case :ets.lookup(__MODULE__, id) do
      [{^id, record}] -> {:ok, record}
      [] -> {:error, :not_found}
    end
  end

  def list do
    records = :ets.tab2list(__MODULE__)
    |> Enum.map(fn {_id, record} -> record end)
    {:ok, records}
  end

  def update(id, attrs) do
    case get(id) do
      {:ok, record} ->
        updated_record = struct(record, Map.merge(attrs, %{updated_at: DateTime.utc_now()}))
        :ets.insert(__MODULE__, {id, updated_record})
        {:ok, updated_record}
      error -> error
    end
  end

  def delete(id) do
    case get(id) do
      {:ok, _record} ->
        :ets.delete(__MODULE__, id)
        :ok
      error -> error
    end
  end

  # Ensure ETS table exists
  def init_storage do
    case :ets.whereis(__MODULE__) do
      :undefined -> 
        :ets.new(__MODULE__, [:named_table, :public, :set])
        :ok
      _ -> :ok
    end
  end
end
