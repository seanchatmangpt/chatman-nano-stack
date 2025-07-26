defmodule CnsForge.Steps.ReactorGenerationStepTest do
  @moduledoc """
  🧪 UNIT TESTING for Reactor Generation Step
  
  Tests the reactor generation step to ensure:
  - Proper Reactor code generation
  - Correct step definitions
  - Valid return structure
  """
  
  use ExUnit.Case, async: true
  
  describe "Reactor Generation Step - Direct Testing" do
    test "generates main reactor for class processing" do
      parsed_data = %{
        classes: [
          %{name: "Person"},
          %{name: "Organization"}
        ]
      }
      resources = []  # Not used in current implementation
      
      assert {:ok, reactors} = CnsForge.TTLAshReactorTransformer.generate_ash_reactors(parsed_data, resources)
      
      assert length(reactors) == 1
      main_reactor = hd(reactors)
      assert main_reactor.name == "CnsForge.TTLMainReactor"
      assert is_binary(main_reactor.code)
    end
    
    test "reactor reflects correct class count" do
      test_cases = [
        {[], 0},
        {[%{name: "Single"}], 1},
        {[%{name: "First"}, %{name: "Second"}, %{name: "Third"}], 3}
      ]
      
      Enum.each(test_cases, fn {classes, expected_count} ->
        parsed_data = %{classes: classes}
        {:ok, [reactor]} = CnsForge.TTLAshReactorTransformer.generate_ash_reactors(parsed_data, [])
        
        assert String.contains?(reactor.code, "transformed_classes: #{expected_count}")
      end)
    end
    
    test "handles empty class list" do
      parsed_data = %{classes: []}
      
      {:ok, [reactor]} = CnsForge.TTLAshReactorTransformer.generate_ash_reactors(parsed_data, [])
      
      assert String.contains?(reactor.code, "transformed_classes: 0")
    end
  end
  
  describe "Generated Reactor Code Quality" do
    test "generated reactor code contains required Reactor components" do
      parsed_data = %{classes: [%{name: "TestClass"}]}
      
      {:ok, [reactor]} = CnsForge.TTLAshReactorTransformer.generate_ash_reactors(parsed_data, [])
      
      code = reactor.code
      
      # Check module definition
      assert String.contains?(code, "defmodule CnsForge.TTLMainReactor do")
      
      # Check Reactor usage
      assert String.contains?(code, "use Reactor")
      
      # Check input definition
      assert String.contains?(code, "input :ontology_data")
      
      # Check step definition
      assert String.contains?(code, "step :transform_classes do")
      assert String.contains?(code, "argument :data, input(:ontology_data)")
      
      # Check step implementation
      assert String.contains?(code, "run fn %{data: data}, _context ->")
      assert String.contains?(code, "{:ok, %{transformed_classes:")
      
      # Check return statement
      assert String.contains?(code, "return :transform_classes")
    end
    
    test "generated reactor code is syntactically valid Elixir" do
      parsed_data = %{classes: [%{name: "ValidClass"}]}
      
      {:ok, [reactor]} = CnsForge.TTLAshReactorTransformer.generate_ash_reactors(parsed_data, [])
      
      # Test that the generated code can be parsed as valid Elixir
      assert {:ok, _ast} = Code.string_to_quoted(reactor.code)
    end
    
    test "reactor structure is consistent regardless of class count" do
      test_counts = [0, 1, 5, 10]
      
      Enum.each(test_counts, fn count ->
        classes = Enum.map(1..count, fn i -> %{name: "Class#{i}"} end)
        parsed_data = %{classes: classes}
        
        {:ok, [reactor]} = CnsForge.TTLAshReactorTransformer.generate_ash_reactors(parsed_data, [])
        
        # All reactors should have the same basic structure
        assert String.contains?(reactor.code, "use Reactor")
        assert String.contains?(reactor.code, "input :ontology_data")
        assert String.contains?(reactor.code, "step :transform_classes")
        assert String.contains?(reactor.code, "return :transform_classes")
        assert String.contains?(reactor.code, "transformed_classes: #{count}")
      end)
    end
  end
  
  describe "Reactor Generation Integration" do
    test "reactor can be conceptually executed with test data" do
      parsed_data = %{classes: [%{name: "User"}, %{name: "Role"}]}
      
      {:ok, [reactor]} = CnsForge.TTLAshReactorTransformer.generate_ash_reactors(parsed_data, [])
      
      # While we can't actually execute the generated code without compilation,
      # we can verify the structure would work with test data
      assert String.contains?(reactor.code, "argument :data, input(:ontology_data)")
      
      # The step should accept data and return the expected structure
      assert String.contains?(reactor.code, "{:ok, %{transformed_classes: 2}}")
    end
    
    test "multiple reactors could theoretically be generated" do
      # Current implementation only generates one main reactor,
      # but test the structure supports multiple
      parsed_data = %{classes: [%{name: "TestClass"}]}
      
      {:ok, reactors} = CnsForge.TTLAshReactorTransformer.generate_ash_reactors(parsed_data, [])
      
      assert is_list(reactors)
      assert length(reactors) >= 1
      
      # Each reactor should have a name and code
      Enum.each(reactors, fn reactor ->
        assert Map.has_key?(reactor, :name)
        assert Map.has_key?(reactor, :code)
        assert is_binary(reactor.name)
        assert is_binary(reactor.code)
      end)
    end
  end
  
  describe "Error Handling in Reactor Generation" do
    test "handles missing classes gracefully" do
      # Test with missing classes key
      assert_raise(KeyError, fn ->
        CnsForge.TTLAshReactorTransformer.generate_ash_reactors(%{}, [])
      end)
    end
    
    test "handles malformed class data" do
      # Classes without name field
      parsed_data = %{classes: [%{uri: "test:NoName"}]}
      
      # Should not crash during generation but may produce invalid code
      assert {:ok, [reactor]} = CnsForge.TTLAshReactorTransformer.generate_ash_reactors(parsed_data, [])
      assert is_binary(reactor.code)
    end
  end
end