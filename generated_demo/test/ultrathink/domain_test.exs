defmodule Ultrathink.DomainTest do
  use ExUnit.Case, async: true
  
  alias Ultrathink.Domain

  describe "domain configuration" do
    test "has all expected resources" do
      resource_names = Domain.resource_names()
      
      expected_resources = [
        "bitactor", "intelligencenode", "coordinationreactor", "signal", "emergentbehavior"
      ]
      
      for resource <- expected_resources do
        assert resource in resource_names
      end
    end
    
    test "domain info returns correct structure" do
      info = Ultrathink.domain_info()
      
      assert info.name == "ultrathink"
      assert info.module == Ultrathink.Domain
      assert is_list(info.resources)
      assert %DateTime{} = info.generated_at
    end
  end
end
