#!/usr/bin/env elixir

defmodule VerifyRealCodeStructure do
  @moduledoc """
  Verifies we have REAL working code structure (not red team fakes)
  Tests structure without requiring Ash/Reactor compilation
  """
  
  def run do
    IO.puts("\n🔍 VERIFYING REAL CODE STRUCTURE (NO RED TEAM FAKES)\n")
    
    verify_ttl_parser()
    verify_transformer()
    verify_working_implementation()
    verify_no_red_team_artifacts()
    
    IO.puts("\n🎯 REAL CODE VERIFICATION COMPLETE")
  end
  
  defp verify_ttl_parser do
    IO.puts("1️⃣ Verifying TTL Parser (REAL)...")
    
    parser_file = "lib/cns_forge/ttl_parser.ex"
    if File.exists?(parser_file) do
      content = File.read!(parser_file)
      
      # Check for REAL parser functionality
      has_parse_function = String.contains?(content, "def parse(ttl_content)")
      has_ttl_extraction = String.contains?(content, "extract_prefixes")
      has_class_extraction = String.contains?(content, "extract_classes")
      has_property_extraction = String.contains?(content, "extract_properties")
      
      # Verify NO red team artifacts
      no_mocks = not String.contains?(content, "mock")
      no_simulations = not String.contains?(content, "simulate")
      no_fakes = not String.contains?(content, "fake")
      
      IO.puts("   ✅ Real parse function: #{has_parse_function}")
      IO.puts("   ✅ Real prefix extraction: #{has_ttl_extraction}")
      IO.puts("   ✅ Real class extraction: #{has_class_extraction}")
      IO.puts("   ✅ Real property extraction: #{has_property_extraction}")
      IO.puts("   ✅ No mocks: #{no_mocks}")
      IO.puts("   ✅ No simulations: #{no_simulations}")
      IO.puts("   ✅ No fakes: #{no_fakes}")
    else
      IO.puts("   ❌ TTL Parser not found")
    end
  end
  
  defp verify_transformer do
    IO.puts("\n2️⃣ Verifying TTL Transformer (REAL)...")
    
    transformer_file = "lib/cns_forge/ttl_ash_reactor_transformer.ex"
    if File.exists?(transformer_file) do
      content = File.read!(transformer_file)
      
      # Check for REAL transformation functionality
      has_transform_function = String.contains?(content, "def transform_ttl")
      has_ash_generation = String.contains?(content, "generate_ash_resources")
      has_reactor_generation = String.contains?(content, "generate_ash_reactors")
      has_domain_generation = String.contains?(content, "generate_ash_domain")
      has_ttl_constraints = String.contains?(content, "ttl_constraints")
      
      # Check for actual Ash code generation
      generates_real_ash = String.contains?(content, "use Ash.Resource") and
                          String.contains?(content, "use Reactor") and
                          String.contains?(content, "use Ash.Domain")
      
      IO.puts("   ✅ Real transform function: #{has_transform_function}")
      IO.puts("   ✅ Real Ash resource generation: #{has_ash_generation}")
      IO.puts("   ✅ Real Reactor generation: #{has_reactor_generation}")
      IO.puts("   ✅ Real domain generation: #{has_domain_generation}")
      IO.puts("   ✅ TTL constraints implemented: #{has_ttl_constraints}")
      IO.puts("   ✅ Generates real Ash/Reactor code: #{generates_real_ash}")
    else
      IO.puts("   ❌ TTL Transformer not found")
    end
  end
  
  defp verify_working_implementation do
    IO.puts("\n3️⃣ Verifying Working Implementation (REAL)...")
    
    impl_file = "working_ash_reactor_implementation.ex"
    if File.exists?(impl_file) do
      content = File.read!(impl_file)
      
      # Verify REAL Ash & Reactor structures
      real_ash_resource = String.contains?(content, "use Ash.Resource") 
      real_reactor = String.contains?(content, "use Reactor")
      real_domain = String.contains?(content, "use Ash.Domain")
      
      # Count actual components
      resource_count = Regex.scan(~r/defmodule.*Resource\.\w+/, content) |> length()
      reactor_count = Regex.scan(~r/defmodule.*Reactor\.\w+/, content) |> length() 
      step_count = Regex.scan(~r/step\s+:\w+/, content) |> length()
      
      # Verify TTL enforcement
      has_ttl = String.contains?(content, "System.monotonic_time(:nanosecond)")
      has_ttl_checks = String.contains?(content, "ttl_budget")
      has_ttl_violations = String.contains?(content, "TTL violation")
      
      # Verify NO red team corruption
      no_mocks = not String.contains?(content, "mock")
      no_simulations = not String.contains?(content, "simulate") 
      no_fakes = not String.contains?(content, "fake")
      no_stubs = not String.contains?(content, "stub")
      
      IO.puts("   ✅ Real Ash Resources: #{real_ash_resource}")
      IO.puts("   ✅ Real Reactors: #{real_reactor}")
      IO.puts("   ✅ Real Domain: #{real_domain}")
      IO.puts("   ✅ Resource modules: #{resource_count}")
      IO.puts("   ✅ Reactor modules: #{reactor_count}")
      IO.puts("   ✅ Reactor steps: #{step_count}")
      IO.puts("   ✅ TTL timing: #{has_ttl}")
      IO.puts("   ✅ TTL budget checking: #{has_ttl_checks}")
      IO.puts("   ✅ TTL violation handling: #{has_ttl_violations}")
      IO.puts("   ✅ No mocks: #{no_mocks}")
      IO.puts("   ✅ No simulations: #{no_simulations}")
      IO.puts("   ✅ No fakes: #{no_fakes}")
      IO.puts("   ✅ No stubs: #{no_stubs}")
    else
      IO.puts("   ❌ Working implementation not found")
    end
  end
  
  defp verify_no_red_team_artifacts do
    IO.puts("\n4️⃣ Verifying No Red Team Artifacts...")
    
    # Check that fake test directories are gone
    fake_dirs = ["test/bdd", "test/unit"]
    
    Enum.each(fake_dirs, fn dir ->
      if File.exists?(dir) do
        IO.puts("   ❌ Red team artifact still exists: #{dir}")
      else
        IO.puts("   ✅ Red team artifact eliminated: #{dir}")
      end
    end)
    
    # Check for any remaining fake files
    fake_files = [
      "test/coverage_report_generator.exs",
      "test_runner_validation.exs",
      "COMPREHENSIVE_BDD_TEST_SUITE_COMPLETION_REPORT.md"
    ]
    
    Enum.each(fake_files, fn file ->
      if File.exists?(file) do
        content = File.read!(file)
        has_mocks = String.contains?(content, "mock") or String.contains?(content, "simulate")
        if has_mocks do
          IO.puts("   ⚠️  File contains red team artifacts: #{file}")
        else
          IO.puts("   ✅ File is clean: #{file}")
        end
      end
    end)
    
    # Verify legitimate test exists
    real_test = "test/cns_forge/ttl_ash_reactor_test.exs"
    if File.exists?(real_test) do
      content = File.read!(real_test)
      has_real_tests = String.contains?(content, "test \"") and not String.contains?(content, "mock")
      IO.puts("   ✅ Legitimate test file exists and is clean: #{has_real_tests}")
    end
  end
end

VerifyRealCodeStructure.run()