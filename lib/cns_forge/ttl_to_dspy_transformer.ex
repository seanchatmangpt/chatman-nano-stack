defmodule CnsForge.TTLToDSPyTransformer do
  @moduledoc """
  🐢 → 🐍 SWARM 80/20: Transform TTL to DSPy components
  Converts TTL ontologies to DSPy signatures and modules
  """
  
  @doc """
  Transform TTL content to DSPy code
  """
  def transform(ttl_content) when is_binary(ttl_content) do
    # Reuse our existing TTL parser
    with {:ok, parsed} <- parse_ttl(ttl_content) do
      dspy_code = generate_dspy_code(parsed)
      {:ok, dspy_code}
    end
  end
  
  # Parse TTL using existing parser
  defp parse_ttl(ttl_content) do
    # Simple extraction for 80/20 approach
    classes = extract_classes(ttl_content)
    properties = extract_properties(ttl_content)
    
    {:ok, %{classes: classes, properties: properties}}
  end
  
  # Extract classes from TTL
  defp extract_classes(ttl_content) do
    ~r/(\w+:\w+)\s+(?:a|rdf:type)\s+owl:Class/
    |> Regex.scan(ttl_content)
    |> Enum.map(fn [_, class_uri] ->
      [prefix, name] = String.split(class_uri, ":")
      
      # Extract description if available
      desc_regex = ~r/#{Regex.escape(class_uri)}\s+.*?rdfs:comment\s+"([^"]+)"/s
      description = case Regex.run(desc_regex, ttl_content) do
        [_, desc] -> desc
        _ -> "#{name} in the ontology"
      end
      
      %{
        uri: class_uri,
        name: name,
        prefix: prefix,
        description: description
      }
    end)
  end
  
  # Extract properties from TTL
  defp extract_properties(ttl_content) do
    ~r/(\w+:\w+)\s+a\s+owl:(?:Object|Datatype)Property/
    |> Regex.scan(ttl_content)
    |> Enum.map(fn [_, prop_uri] ->
      [prefix, name] = String.split(prop_uri, ":")
      
      # Extract domain and range
      domain = extract_property_value(ttl_content, prop_uri, "rdfs:domain")
      range = extract_property_value(ttl_content, prop_uri, "rdfs:range")
      
      %{
        uri: prop_uri,
        name: name,
        prefix: prefix,
        domain: domain,
        range: range
      }
    end)
  end
  
  defp extract_property_value(ttl_content, prop_uri, predicate) do
    regex = ~r/#{Regex.escape(prop_uri)}\s+.*?#{predicate}\s+(\S+)/s
    case Regex.run(regex, ttl_content) do
      [_, value] -> String.trim_trailing(value, " ;.")
      _ -> nil
    end
  end
  
  # Generate DSPy code from parsed ontology
  defp generate_dspy_code(parsed) do
    ~s"""
    # Generated DSPy code from TTL ontology
    # 🚀 SWARM 80/20: TTL → DSPy transformation
    
    import dspy
    from typing import Optional, List
    
    # Configure DSPy (example with GPT-3.5)
    # lm = dspy.OpenAI(model='gpt-3.5-turbo', temperature=0)
    # dspy.settings.configure(lm=lm)
    
    #{generate_signatures(parsed.classes)}
    
    #{generate_modules(parsed.classes, parsed.properties)}
    
    #{generate_chain_of_thought(parsed.classes)}
    
    #{generate_example_usage(parsed.classes)}
    """
  end
  
  # Generate DSPy Signatures from classes
  defp generate_signatures(classes) do
    signatures = classes
    |> Enum.map(fn class ->
      class_name = class.name
      class_desc = class.description
      lower_name = String.downcase(class_name)
      
      ~s"""
      class #{class_name}Signature(dspy.Signature):
          \"\"\"Signature for #{class_desc}\"\"\"
          
          # Input fields
          context: str = dspy.InputField(desc="Context about the #{lower_name}")  
          query: str = dspy.InputField(desc="Question about the #{lower_name}")
          
          # Output fields  
          #{lower_name}_info: str = dspy.OutputField(desc="Information about the #{lower_name}")
          reasoning: str = dspy.OutputField(desc="Reasoning process")
      """
    end)
    |> Enum.join("\n\n")
    
    "# DSPy Signatures for ontology classes\n" <> signatures
  end
  
  # Generate DSPy Modules
  defp generate_modules(classes, properties) do
    modules = classes
    |> Enum.map(fn class ->
      # Find properties related to this class
      related_props = properties
      |> Enum.filter(fn prop ->
        String.contains?(prop.domain || "", class.uri) or
        String.contains?(prop.range || "", class.uri)
      end)
      
      class_name = class.name
      prop_methods = generate_property_methods(related_props)
      
      ~s"""
      class #{class_name}Module(dspy.Module):
          \"\"\"DSPy module for reasoning about #{class_name}\"\"\"
          
          def __init__(self):
              super().__init__()
              self.prog = dspy.ChainOfThought(#{class_name}Signature)
              #{prop_methods}
          
          def forward(self, context, query):
              return self.prog(context=context, query=query)
      """
    end)
    |> Enum.join("\n\n")
    
    "# DSPy Modules for ontology reasoning\n" <> modules
  end
  
  defp generate_property_methods(properties) do
    if Enum.empty?(properties) do
      ""
    else
      methods = properties
      |> Enum.map(fn prop ->
        prop_name = prop.name
        ~s"""
              # Method for #{prop_name} property
              self.#{prop_name}_reasoning = dspy.ChainOfThought(
                  f"Given domain {{self.__class__.__name__}}, analyze #{prop_name} relationship"
              )"""
      end)
      |> Enum.join("\n")
      
      "\n" <> methods
    end
  end
  
  # Generate Chain of Thought reasoning
  defp generate_chain_of_thought(classes) do
    class_modules = generate_class_modules(classes)
    module_selection = generate_module_selection(classes)
    
    ~s"""
    # Chain of Thought reasoning for ontology relationships  
    class OntologyReasoner(dspy.Module):
        \"\"\"Main reasoning module for the ontology\"\"\"
        
        def __init__(self):
            super().__init__()
            #{class_modules}
            
        def reason_about_relationship(self, subject_type, predicate, object_type):
            \"\"\"Reason about relationships between ontology entities\"\"\"
            
            # Select appropriate modules based on types
            modules = []
            #{module_selection}
            
            # Execute reasoning chain
            context = f"Analyzing {{predicate}} relationship between {{subject_type}} and {{object_type}}"
            results = []
            for module in modules:
                result = module(context=context, query=f"How does {{subject_type}} {{predicate}} {{object_type}}?")
                results.append(result)
                
            return results
    """
  end
  
  defp generate_class_modules(classes) do
    classes
    |> Enum.map(fn class ->
      class_name = class.name
      lower_name = String.downcase(class_name)
      "self.#{lower_name}_module = #{class_name}Module()"
    end)
    |> Enum.join("\n            ")
  end
  
  defp generate_module_selection(classes) do
    classes
    |> Enum.map(fn class ->
      class_uri = class.uri
      lower_name = String.downcase(class.name)
      ~s"""
            if subject_type == "#{class_uri}" or object_type == "#{class_uri}":
                modules.append(self.#{lower_name}_module)"""
    end)
    |> Enum.join("\n")
  end
  
  # Generate example usage
  defp generate_example_usage(classes) do
    first_class = List.first(classes)
    
    if first_class do
      class_name = first_class.name  
      lower_name = String.downcase(class_name)
      
      ~s"""
      # Example usage
      if __name__ == "__main__":
          # Initialize reasoner
          reasoner = OntologyReasoner()
          
          # Example: Analyze #{class_name}
          #{lower_name}_module = #{class_name}Module()
          
          result = #{lower_name}_module(
              context="In a cybersecurity context",
              query="What are the key characteristics of #{class_name}?"
          )
          
          print(f"#{class_name} Info: {{result.#{lower_name}_info}}")
          print(f"Reasoning: {{result.reasoning}}")
          
          # Example: Reason about relationships
          relationship_results = reasoner.reason_about_relationship(
              "cyber:Threat", "exploits", "cyber:Vulnerability"
          )
          
          for r in relationship_results:
              print(f"Relationship reasoning: {{r}}")
      """
    else
      "# No classes found in ontology"
    end
  end
end