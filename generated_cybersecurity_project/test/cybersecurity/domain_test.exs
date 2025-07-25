defmodule Cybersecurity.DomainTest do
  use ExUnit.Case, async: true
  
  alias Cybersecurity.Domain

  describe "domain configuration" do
    test "has all expected resources" do
      resource_names = Domain.resource_names()
      
      expected_resources = [
        "asset", "networkasset", "computeasset", "dataasset", "userasset", "applicationasset", "networkdevice", "router", "switch", "firewall", "loadbalancer", "vpn", "networksegment", "dmz", "internalnetwork", "publicnetwork", "threat", "malware", "virus", "worm", "trojan", "ransomware", "rootkit", "spyware", "botnet", "attack", "networkattack", "ddosattack", "maninthemiddleattack", "packetsniffing", "portscan", "webattack", "sqlinjection", "xss", "csrf", "socialengineering", "phishingattack", "spearphishing", "privilegeescalation", "lateralmovement", "dataexfiltration", "securityevent", "securityincident", "vulnerability", "zerodayvulnerability", "alert", "threatintelligence", "ioc", "securitycontrol", "preventivecontrol", "detectivecontrol", "correctivecontrol", "ids", "ips", "siem", "antivirus", "edr", "soar"
      ]
      
      for resource <- expected_resources do
        assert resource in resource_names
      end
    end
    
    test "domain info returns correct structure" do
      info = Cybersecurity.domain_info()
      
      assert info.name == "cybersecurity"
      assert info.module == Cybersecurity.Domain
      assert is_list(info.resources)
      assert %DateTime{} = info.generated_at
    end
  end
end
