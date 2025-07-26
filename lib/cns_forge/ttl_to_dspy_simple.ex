defmodule CnsForge.TTLToDSPyTransformer do
  @moduledoc """
  🐢 → 🐍 SWARM 80/20: Simple TTL to DSPy transformation
  Generates valid Python DSPy code from TTL
  """
  
  def transform(ttl_content) when is_binary(ttl_content) do
    # Extract classes from TTL
    classes = extract_classes(ttl_content)
    
    # Generate DSPy Python code
    dspy_code = generate_dspy_code(classes)
    
    {:ok, dspy_code}
  end
  
  defp extract_classes(ttl_content) do
    ~r/(\w+):(\w+)\s+(?:a|rdf:type)\s+owl:Class/
    |> Regex.scan(ttl_content)
    |> Enum.map(fn [_, prefix, name] ->
      %{prefix: prefix, name: name, uri: "#{prefix}:#{name}"}
    end)
  end
  
  defp generate_dspy_code(classes) do
    signatures = generate_signatures(classes)
    modules = generate_modules(classes)
    reasoner = generate_reasoner(classes)
    example = generate_example(classes)
    
    header = """
# Generated DSPy code from TTL ontology
# 🚀 SWARM 80/20: TTL → DSPy transformation

import dspy
from typing import Optional, List

# Configure DSPy (example with GPT-3.5)
# lm = dspy.OpenAI(model='gpt-3.5-turbo', temperature=0)
# dspy.settings.configure(lm=lm)

"""
    
    header <> signatures <> "\n\n" <> modules <> "\n\n" <> reasoner <> "\n\n" <> example
  end
  
  defp generate_signatures(classes) do
    classes
    |> Enum.map(fn class ->
      """
class #{class.name}Signature(dspy.Signature):
    \"\"\"Signature for #{class.name} analysis\"\"\"
    
    # Input fields
    context = dspy.InputField(desc="Context about the #{String.downcase(class.name)}")
    query = dspy.InputField(desc="Question about the #{String.downcase(class.name)}")
    
    # Output fields  
    #{String.downcase(class.name)}_info = dspy.OutputField(desc="Information about the #{String.downcase(class.name)}")
    reasoning = dspy.OutputField(desc="Reasoning process")
"""
    end)
    |> Enum.join("\n\n")
  end
  
  defp generate_modules(classes) do
    classes
    |> Enum.map(fn class ->
      """
class #{class.name}Module(dspy.Module):
    \"\"\"DSPy module for reasoning about #{class.name}\"\"\"
    
    def __init__(self):
        super().__init__()
        self.prog = dspy.ChainOfThought(#{class.name}Signature)
    
    def forward(self, context, query):
        return self.prog(context=context, query=query)
"""
    end)
    |> Enum.join("\n\n")
  end
  
  defp generate_reasoner(classes) do
    module_inits = classes
    |> Enum.map(fn class ->
      "        self.#{String.downcase(class.name)}_module = #{class.name}Module()"
    end)
    |> Enum.join("\n")
    
    module_selections = classes
    |> Enum.map(fn class ->
      "        if subject_type == \"#{class.uri}\" or object_type == \"#{class.uri}\":\n" <>
      "            modules.append(self.#{String.downcase(class.name)}_module)"
    end)
    |> Enum.join("\n")
    
    # Build the reasoner class as a string
    "class OntologyReasoner(dspy.Module):\n" <>
    "    \"\"\"Main reasoning module for the ontology\"\"\"\n" <>
    "    \n" <>
    "    def __init__(self):\n" <>
    "        super().__init__()\n" <>
    module_inits <> "\n" <>
    "        \n" <>
    "    def reason_about_relationship(self, subject_type, predicate, object_type):\n" <>
    "        \"\"\"Reason about relationships between ontology entities\"\"\"\n" <>
    "        \n" <>
    "        # Select appropriate modules based on types\n" <>
    "        modules = []\n" <>
    module_selections <> "\n" <>
    "        \n" <>
    "        # Execute reasoning chain\n" <>
    "        context = f\"Analyzing {predicate} relationship between {subject_type} and {object_type}\"\n" <>
    "        results = []\n" <>
    "        for module in modules:\n" <>
    "            result = module(context=context, query=f\"How does {subject_type} {predicate} {object_type}?\")\n" <>
    "            results.append(result)\n" <>
    "            \n" <>
    "        return results"
  end
  
  defp generate_example(classes) do
    first_class = List.first(classes)
    
    if first_class do
      """
# Example usage
if __name__ == "__main__":
    # Initialize reasoner
    reasoner = OntologyReasoner()
    
    # Example: Analyze #{first_class.name}
    #{String.downcase(first_class.name)}_module = #{first_class.name}Module()
    
    result = #{String.downcase(first_class.name)}_module(
        context="In a cybersecurity context",
        query="What are the key characteristics of #{first_class.name}?"
    )
    
    print(f"#{first_class.name} Info: {{result.#{String.downcase(first_class.name)}_info}}")
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