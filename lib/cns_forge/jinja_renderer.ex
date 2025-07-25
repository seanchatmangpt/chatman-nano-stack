defmodule CNSForge.JinjaRenderer do
  @moduledoc """
  Jinja template renderer for CNS Forge AOT compilation
  Renders Jinja2 templates for BitActor C code generation
  """
  
  alias CNSForge.TemplateCache
  require Logger
  
  @template_dir Path.join([File.cwd!(), "templates"])
  @custom_filters %{
    "c_identifier" => &c_identifier/1,
    "upper" => &String.upcase/1,
    "lower" => &String.downcase/1,
    "default" => &default_filter/2
  }
  
  @doc """
  Render a Jinja template with the given context
  """
  def render(template_path, context) do
    full_path = Path.join(@template_dir, template_path)
    
    with {:ok, template_content} <- read_template(full_path),
         {:ok, rendered} <- do_render(template_content, context) do
      {:ok, rendered}
    end
  end
  
  @doc """
  Render a template string directly
  """
  def render_string(template_string, context) do
    do_render(template_string, context)
  end
  
  @doc """
  Batch render multiple templates with shared context
  """
  def batch_render(templates, shared_context \\ %{}) do
    results = Enum.map(templates, fn {template_path, specific_context} ->
      context = Map.merge(shared_context, specific_context)
      {template_path, render(template_path, context)}
    end)
    
    errors = Enum.filter(results, fn {_, result} -> match?({:error, _}, result) end)
    
    if Enum.empty?(errors) do
      {:ok, Enum.map(results, fn {path, {:ok, content}} -> {path, content} end)}
    else
      {:error, {:batch_render_errors, errors}}
    end
  end
  
  # Private functions
  
  defp read_template(path) do
    case TemplateCache.get(path) do
      {:ok, cached} ->
        {:ok, cached}
        
      :miss ->
        case File.read(path) do
          {:ok, content} ->
            TemplateCache.put(path, content)
            {:ok, content}
            
          error ->
            Logger.error("Failed to read template #{path}: #{inspect(error)}")
            error
        end
    end
  end
  
  defp do_render(template_content, context) do
    try do
      rendered = template_content
      |> parse_template()
      |> evaluate_template(context)
      |> post_process()
      
      {:ok, rendered}
    rescue
      e in RuntimeError ->
        {:error, {:render_error, e.message}}
    end
  end
  
  defp parse_template(content) do
    # Parse Jinja syntax - simplified version
    content
    |> parse_variables()
    |> parse_blocks()
    |> parse_filters()
    |> parse_loops()
    |> parse_conditionals()
  end
  
  defp evaluate_template(parsed, context) do
    parsed
    |> replace_variables(context)
    |> evaluate_loops(context)
    |> evaluate_conditionals(context)
    |> apply_filters(context)
  end
  
  defp parse_variables(content) do
    # Parse {{ variable }} syntax
    Regex.replace(~r/\{\{\s*(\w+(?:\.\w+)*)\s*\}\}/, content, fn _, var ->
      "<jinja:var>#{var}</jinja:var>"
    end)
  end
  
  defp parse_blocks(content) do
    # Parse {% block %} syntax
    content
    |> String.replace(~r/\{%\s*for\s+(\w+)\s+in\s+(\w+)\s*%\}/, "<jinja:for var='\\1' in='\\2'>")
    |> String.replace(~r/\{%\s*endfor\s*%\}/, "</jinja:for>")
    |> String.replace(~r/\{%\s*if\s+(.+?)\s*%\}/, "<jinja:if condition='\\1'>")
    |> String.replace(~r/\{%\s*endif\s*%\}/, "</jinja:if>")
  end
  
  defp parse_filters(content) do
    # Parse {{ variable|filter }} syntax
    Regex.replace(~r/\{\{\s*(\w+(?:\.\w+)*)\s*\|\s*(\w+)(?:\(([^)]*)\))?\s*\}\}/, content, fn _, var, filter, args ->
      args_str = if args, do: " args='#{args}'", else: ""
      "<jinja:filtered var='#{var}' filter='#{filter}'#{args_str}/>"
    end)
  end
  
  defp parse_loops(content) do
    # Already handled in parse_blocks
    content
  end
  
  defp parse_conditionals(content) do
    # Already handled in parse_blocks
    content
  end
  
  defp replace_variables(content, context) do
    Regex.replace(~r/<jinja:var>([^<]+)<\/jinja:var>/, content, fn _, var ->
      value = get_nested_value(context, var)
      to_string(value)
    end)
  end
  
  defp evaluate_loops(content, context) do
    Regex.replace(~r/<jinja:for var='(\w+)' in='(\w+)'>(.+?)<\/jinja:for>/s, content, fn _, var, collection, body ->
      items = Map.get(context, String.to_atom(collection), [])
      
      Enum.map_join(items, "\n", fn item ->
        loop_context = Map.put(context, String.to_atom(var), item)
        body
        |> replace_variables(loop_context)
        |> apply_filters(loop_context)
      end)
    end)
  end
  
  defp evaluate_conditionals(content, context) do
    Regex.replace(~r/<jinja:if condition='([^']+)'>(.+?)<\/jinja:if>/s, content, fn _, condition, body ->
      if evaluate_condition(condition, context) do
        body
      else
        ""
      end
    end)
  end
  
  defp apply_filters(content, context) do
    Regex.replace(~r/<jinja:filtered var='([^']+)' filter='([^']+)'(?:\s+args='([^']+)')?\/>/,
                  content, fn _, var, filter, args ->
      value = get_nested_value(context, var)
      filter_fn = Map.get(@custom_filters, filter, &identity/1)
      
      if args do
        apply(filter_fn, [value, parse_filter_args(args)])
      else
        filter_fn.(value)
      end |> to_string()
    end)
  end
  
  defp post_process(content) do
    # Clean up any remaining jinja tags
    content
    |> String.replace(~r/<jinja:[^>]+>/, "")
    |> String.replace(~r/<\/jinja:[^>]+>/, "")
  end
  
  defp get_nested_value(context, path) when is_binary(path) do
    path
    |> String.split(".")
    |> Enum.map(&String.to_atom/1)
    |> get_nested_value(context)
  end
  
  defp get_nested_value(context, [key]), do: Map.get(context, key)
  defp get_nested_value(context, [key | rest]) do
    case Map.get(context, key) do
      nil -> nil
      value -> get_nested_value(value, rest)
    end
  end
  
  defp evaluate_condition(condition, context) do
    # Simple condition evaluation - would be more complex in production
    case condition do
      "true" -> true
      "false" -> false
      _ ->
        var = String.trim(condition)
        value = get_nested_value(context, var)
        truthy?(value)
    end
  end
  
  defp truthy?(nil), do: false
  defp truthy?(false), do: false
  defp truthy?(""), do: false
  defp truthy?([]), do: false
  defp truthy?(_), do: true
  
  defp c_identifier(name) when is_binary(name) do
    name
    |> String.downcase()
    |> String.replace(~r/[^a-z0-9_]/, "_")
    |> String.replace(~r/_+/, "_")
    |> String.trim("_")
  end
  defp c_identifier(name), do: c_identifier(to_string(name))
  
  defp default_filter(value, default) do
    if value in [nil, ""], do: default, else: value
  end
  
  defp parse_filter_args(args) do
    # Simple argument parsing - would be more robust in production
    args
  end
  
  defp identity(x), do: x
end

defmodule CNSForge.TemplateCache do
  @moduledoc """
  Simple template cache for performance
  """
  
  use GenServer
  
  def start_link(_) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end
  
  def get(path) do
    GenServer.call(__MODULE__, {:get, path})
  end
  
  def put(path, content) do
    GenServer.cast(__MODULE__, {:put, path, content})
  end
  
  # GenServer callbacks
  def init(_) do
    {:ok, %{}}
  end
  
  def handle_call({:get, path}, _from, cache) do
    case Map.get(cache, path) do
      nil -> {:reply, :miss, cache}
      content -> {:reply, {:ok, content}, cache}
    end
  end
  
  def handle_cast({:put, path, content}, cache) do
    {:noreply, Map.put(cache, path, content)}
  end
end