defmodule Cybersecurity.Workflows.XSSWorkflow do
  @moduledoc """
  Workflow for XSS resource operations
  """

  # 80/20: Plain Elixir workflow for XSS resource operations
  
  @doc """
  Execute a xss resource action
  """
  def execute(action, resource_data \\ %{}, resource_id \\ nil) do
    with {:ok, validated_action} <- validate_action(action),
         {:ok, result} <- execute_resource_action(validated_action, resource_data, resource_id) do
      format_result(result, validated_action)
    end
  end
  
  defp validate_action(action) do
    if action in [:create, :read, :update, :delete, :list] do
      {:ok, action}
    else
      {:error, :invalid_action}
    end
  end
  
  defp execute_resource_action(action, data, id) do
    resource = Cybersecurity.Resources.XSS
    
    case action do
      :create ->
        resource.create(data)
        
      :read when not is_nil(id) ->
        resource.get(id)
        
      :list ->
        resource.list()
        
      :update when not is_nil(id) ->
        resource.update(id, data)
        
      :delete when not is_nil(id) ->
        resource.delete(id)
        
      _ ->
        {:error, :invalid_action_combination}
    end
  end
  
  defp format_result(result, action) do
    {:ok, %{
      resource: "xss",
      action: action,
      result: result,
      timestamp: DateTime.utc_now()
    }}
  end
end
