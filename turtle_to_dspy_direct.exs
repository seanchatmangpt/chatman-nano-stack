#!/usr/bin/env elixir

# 🐢 → 🐍 Direct TTL to DSPy generation avoiding string interpolation issues

IO.puts("🐢 → 🐍 SWARM 80/20 PIPELINE: turtle → ttl2dspy")
IO.puts("=" <> String.duplicate("=", 45))

# Parse TTL to extract classes
ttl_input = """
@prefix cyber: <http://cybersecurity.org/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

cyber:Asset a owl:Class .
cyber:Threat a owl:Class .
cyber:Vulnerability a owl:Class .
cyber:SecurityControl a owl:Class .
"""

# Simple class extraction
classes = Regex.scan(~r/(\w+):(\w+)\s+a\s+owl:Class/, ttl_input)
|> Enum.map(fn [_, prefix, name] -> %{prefix: prefix, name: name} end)

IO.puts("\n📥 EXTRACTED CLASSES: #{length(classes)}")
Enum.each(classes, fn c -> IO.puts("  - #{c.name}") end)

# Generate DSPy Python code
dspy_file = "generated_ontology_dspy.py"
{:ok, file} = File.open(dspy_file, [:write, :utf8])

# Write header
IO.write(file, """
# Generated DSPy code from TTL ontology
# 🚀 SWARM 80/20: TTL → DSPy transformation

import dspy
from typing import Optional, List

# Configure DSPy (example with GPT-3.5)
# lm = dspy.OpenAI(model='gpt-3.5-turbo', temperature=0)
# dspy.settings.configure(lm=lm)

# DSPy Signatures for ontology classes
""")

# Write signatures
Enum.each(classes, fn class ->
  IO.write(file, """

class #{class.name}Signature(dspy.Signature):
    \"\"\"Signature for #{class.name} analysis\"\"\"
    
    context = dspy.InputField(desc="Context about the #{String.downcase(class.name)}")
    query = dspy.InputField(desc="Question about the #{String.downcase(class.name)}")
    
    #{String.downcase(class.name)}_info = dspy.OutputField(desc="Information about the #{String.downcase(class.name)}")
    reasoning = dspy.OutputField(desc="Reasoning process")
""")
end)

# Write modules
IO.write(file, "\n\n# DSPy Modules for ontology reasoning\n")

Enum.each(classes, fn class ->
  IO.write(file, """

class #{class.name}Module(dspy.Module):
    \"\"\"DSPy module for reasoning about #{class.name}\"\"\"
    
    def __init__(self):
        super().__init__()
        self.prog = dspy.ChainOfThought(#{class.name}Signature)
    
    def forward(self, context, query):
        return self.prog(context=context, query=query)
""")
end)

# Write reasoner
IO.write(file, """


class OntologyReasoner(dspy.Module):
    \"\"\"Main reasoning module for the ontology\"\"\"
    
    def __init__(self):
        super().__init__()
""")

Enum.each(classes, fn class ->
  IO.write(file, "        self.#{String.downcase(class.name)}_module = #{class.name}Module()\n")
end)

IO.write(file, """
        
    def reason_about_relationship(self, subject_type, predicate, object_type):
        \"\"\"Reason about relationships between ontology entities\"\"\"
        
        modules = []
""")

Enum.each(classes, fn class ->
  IO.write(file, """
        if subject_type == "#{class.prefix}:#{class.name}" or object_type == "#{class.prefix}:#{class.name}":
            modules.append(self.#{String.downcase(class.name)}_module)
""")
end)

IO.write(file, """
        
        context = f"Analyzing {predicate} relationship between {subject_type} and {object_type}"
        results = []
        for module in modules:
            result = module(context=context, query=f"How does {subject_type} {predicate} {object_type}?")
            results.append(result)
            
        return results
""")

# Write example
first_class = List.first(classes)
if first_class do
  IO.write(file, """

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
    
    print(f"#{first_class.name} Info: {result.#{String.downcase(first_class.name)}_info}")
    print(f"Reasoning: {result.reasoning}")
    
    # Example: Reason about relationships
    relationship_results = reasoner.reason_about_relationship(
        "cyber:Threat", "exploits", "cyber:Vulnerability"
    )
    
    for r in relationship_results:
        print(f"Relationship reasoning: {r}")
""")
end

File.close(file)

IO.puts("\n✅ DSPy CODE GENERATED!")
IO.puts("  File: #{dspy_file}")

# Show preview
{:ok, content} = File.read(dspy_file)
lines = String.split(content, "\n")

IO.puts("\n📝 PREVIEW (first 40 lines):")
IO.puts("=" <> String.duplicate("=", 45))
lines |> Enum.take(40) |> Enum.join("\n") |> IO.puts()
IO.puts("... (#{length(lines)} total lines)")

IO.puts("\n🎯 80/20 ACHIEVEMENT:")
IO.puts("  ✅ TTL parsed correctly")
IO.puts("  ✅ #{length(classes)} DSPy signatures generated")
IO.puts("  ✅ #{length(classes)} DSPy modules created")
IO.puts("  ✅ OntologyReasoner with relationship analysis")
IO.puts("  ✅ Ready for next stage: ttl2dspy → BitActor")