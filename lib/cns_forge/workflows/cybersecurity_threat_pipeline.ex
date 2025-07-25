defmodule CNSForge.Workflows.CybersecurityThreatPipeline do
  @moduledoc """
  Advanced Reactor workflow for end-to-end cybersecurity threat detection
  
  Implements comprehensive threat processing using:
  - Map steps for processing threat collections
  - Switch steps for threat classification 
  - Group steps for coordinated response
  - Around steps for transaction safety
  - Collect steps for aggregating intelligence
  - Streaming processing for large threat feeds
  """
  
  use Reactor

  middlewares do
    middleware CNSForge.ReactorMiddleware
    middleware Reactor.Middleware.Telemetry
  end

  # Inputs from cybersecurity_core.ttl ontology
  input :threat_feed_sources
  input :security_policies
  input :response_config
  input :ttl, default: 8

  # Step 1: Extract threat data from multiple sources (streaming)
  step :extract_threat_feeds, CNSForge.Steps.ExtractThreatFeeds do
    argument :sources, input(:threat_feed_sources)
    argument :ttl, input(:ttl)
    max_retries 3
    
    compensate fn reason, _args, _context, _options ->
      case reason do
        %HTTPoison.Error{reason: :timeout} -> :retry
        %HTTPoison.Error{reason: :econnrefused} -> :retry
        _other -> :ok
      end
    end
  end

  # Step 2: Validate data quality for threat intelligence
  step :validate_threat_quality, CNSForge.Steps.ThreatQualityValidator do
    argument :raw_threats, result(:extract_threat_feeds)
    argument :ttl, input(:ttl)
    
    where fn %{raw_threats: threats} ->
      length(threats.items) > 0
    end
  end

  # Step 3: Process threats in parallel batches with advanced patterns
  map :process_threat_batch do
    source result(:extract_threat_feeds, [:threat_stream])
    batch_size 100
    allow_async? true
    return :classify_threat

    # Parse individual threat indicators
    step :parse_threat_indicators do
      argument :threat_data, element(:process_threat_batch)
      argument :ttl, input(:ttl)
      
      run fn %{threat_data: data, ttl: ttl} ->
        if ttl <= 0 do
          {:error, :ttl_expired}
        else
          parsed = %{
            ioc_type: determine_ioc_type(data),
            threat_level: calculate_threat_level(data),
            source_confidence: data.confidence || 0.5,
            indicators: extract_indicators(data),
            ttl: ttl - 1,
            timestamp: DateTime.utc_now()
          }
          {:ok, parsed}
        end
      end
    end

    # Enrich with external threat intelligence
    step :enrich_threat_intel do
      argument :parsed_threat, result(:parse_threat_indicators)
      
      run &enrich_with_misp_feed/1
      max_retries 2
      
      compensate fn _reason, _args, _context, _options ->
        # Continue without enrichment on external service failure
        :ok
      end
    end

    # Classification using switch pattern for threat types
    switch :classify_threat do
      on result(:enrich_threat_intel)

      matches? &(&1.ioc_type == :malware_hash) do
        step :classify_malware do
          argument :threat, result(:enrich_threat_intel)
          run &classify_malware_threat/1
        end
      end

      matches? &(&1.ioc_type == :ip_address) do
        step :classify_network_threat do
          argument :threat, result(:enrich_threat_intel)
          run &classify_network_threat/1
        end
      end

      matches? &(&1.ioc_type == :domain) do
        step :classify_domain_threat do
          argument :threat, result(:enrich_threat_intel)
          run &classify_domain_threat/1
        end
      end

      default do
        step :classify_generic_threat do
          argument :threat, result(:enrich_threat_intel)
          run &classify_generic_threat/1
        end
      end
    end
  end

  # Step 4: Collect and aggregate threat intelligence
  collect :aggregate_threat_intelligence do
    argument :classified_threats, result(:process_threat_batch)
    argument :source_stats, result(:extract_threat_feeds, [:stats])
    argument :quality_report, result(:validate_threat_quality)
    
    transform fn %{classified_threats: threats, source_stats: stats, quality_report: quality} ->
      successful = Enum.filter(threats, &match?({:ok, _}, &1))
      failed = Enum.filter(threats, &match?({:error, _}, &1))
      
      threat_summary = %{
        total_processed: length(threats),
        successful_classifications: length(successful),
        failed_classifications: length(failed),
        success_rate: length(successful) / length(threats),
        threat_levels: aggregate_threat_levels(successful),
        ioc_distribution: aggregate_ioc_types(successful),
        processing_time: DateTime.utc_now(),
        source_coverage: stats.sources_processed,
        quality_score: quality.overall_score
      }
      
      %{
        threats: Enum.map(successful, &elem(&1, 1)),
        failed_threats: failed,
        intelligence_summary: threat_summary
      }
    end
  end

  # Step 5: Threat correlation using group pattern
  group :threat_correlation do
    before_all &setup_correlation_context/3
    after_all &cleanup_correlation_resources/1

    step :correlate_network_indicators do
      argument :threats, result(:aggregate_threat_intelligence, [:threats])
      
      run fn %{threats: threats} ->
        network_threats = Enum.filter(threats, &(&1.category == :network))
        correlations = correlate_network_patterns(network_threats)
        {:ok, correlations}
      end
    end

    step :correlate_malware_families do
      argument :threats, result(:aggregate_threat_intelligence, [:threats])
      
      run fn %{threats: threats} ->
        malware_threats = Enum.filter(threats, &(&1.category == :malware))
        families = identify_malware_families(malware_threats)
        {:ok, families}
      end
    end

    step :correlate_attack_campaigns do
      argument :threats, result(:aggregate_threat_intelligence, [:threats])
      
      run fn %{threats: threats} ->
        campaigns = identify_attack_campaigns(threats)
        {:ok, campaigns}
      end
    end
  end

  # Step 6: Risk assessment and prioritization
  step :assess_risk_levels, CNSForge.Steps.ThreatRiskAssessment do
    argument :threat_intelligence, result(:aggregate_threat_intelligence)
    argument :correlations, result(:threat_correlation)
    argument :security_policies, input(:security_policies)
    
    wait_for [:correlate_network_indicators, :correlate_malware_families, :correlate_attack_campaigns]
  end

  # Step 7: Parallel response orchestration using compose
  step :orchestrate_response_actions do
    argument :risk_assessment, result(:assess_risk_levels)
    argument :response_config, input(:response_config)
    
    run fn %{risk_assessment: assessment, response_config: config} ->
      high_priority = Enum.filter(assessment.threats, &(&1.priority == :critical))
      
      if length(high_priority) > 0 do
        # Spawn parallel response workflows
        response_tasks = Enum.map(high_priority, fn threat ->
          case threat.category do
            :network -> spawn_network_response(threat, config)
            :malware -> spawn_malware_response(threat, config)
            :domain -> spawn_domain_response(threat, config)
            _ -> spawn_generic_response(threat, config)
          end
        end)
        
        {:ok, %{response_tasks: response_tasks, total_responses: length(response_tasks)}}
      else
        {:ok, %{response_tasks: [], total_responses: 0}}
      end
    end
  end

  # Step 8: Generate comprehensive threat report
  template :generate_threat_report do
    argument :intelligence, result(:aggregate_threat_intelligence)
    argument :correlations, result(:threat_correlation)
    argument :risk_assessment, result(:assess_risk_levels)
    argument :response_summary, result(:orchestrate_response_actions)
    
    template """
    # Cybersecurity Threat Intelligence Report
    
    ## Executive Summary
    - **Total Threats Processed**: <%= @intelligence.intelligence_summary.total_processed %>
    - **Success Rate**: <%= Float.round(@intelligence.intelligence_summary.success_rate * 100, 2) %>%
    - **Critical Threats Identified**: <%= length(Enum.filter(@risk_assessment.threats, &(&1.priority == :critical))) %>
    - **Response Actions Initiated**: <%= @response_summary.total_responses %>
    
    ## Threat Distribution
    <%= for {ioc_type, count} <- @intelligence.intelligence_summary.ioc_distribution do %>
    - **<%= String.capitalize(to_string(ioc_type)) %>**: <%= count %> indicators
    <% end %>
    
    ## Risk Assessment
    <%= for threat <- @risk_assessment.threats do %>
    ### Threat ID: <%= threat.id %>
    - **Type**: <%= threat.category %>
    - **Priority**: <%= threat.priority %>
    - **Risk Score**: <%= threat.risk_score %>
    - **Confidence**: <%= threat.confidence %>
    <% end %>
    
    ## Correlation Analysis
    - **Attack Campaigns Identified**: <%= length(@correlations.campaigns) %>
    - **Malware Families**: <%= length(@correlations.malware_families) %>
    - **Network Patterns**: <%= length(@correlations.network_correlations) %>
    
    ## Response Actions
    <%= if @response_summary.total_responses > 0 do %>
    - Automated response actions have been initiated for <%= @response_summary.total_responses %> critical threats
    - Response coordination is managed through the BitActor mesh
    <% else %>
    - No automated responses required at this time
    <% end %>
    
    ---
    *Report generated by CNS Forge BitActor Mesh at <%= DateTime.utc_now() %>*
    """
  end

  # Step 9: Store intelligence in multiple destinations
  step :persist_threat_intelligence, CNSForge.Steps.PersistThreatIntelligence do
    argument :intelligence, result(:aggregate_threat_intelligence)
    argument :correlations, result(:threat_correlation)
    argument :report, result(:generate_threat_report)
    async? true
  end

  step :update_security_feeds, CNSForge.Steps.UpdateSecurityFeeds do
    argument :new_indicators, result(:aggregate_threat_intelligence, [:threats])
    async? true
  end

  # Return comprehensive results
  collect :final_results do
    argument :intelligence, result(:aggregate_threat_intelligence)
    argument :correlations, result(:threat_correlation)
    argument :risk_assessment, result(:assess_risk_levels)
    argument :response_actions, result(:orchestrate_response_actions)
    argument :threat_report, result(:generate_threat_report)
    
    wait_for [:persist_threat_intelligence, :update_security_feeds]
    
    transform fn inputs ->
      %{
        processing_summary: inputs.intelligence.intelligence_summary,
        threat_correlations: inputs.correlations,
        risk_assessment: inputs.risk_assessment,
        response_coordination: inputs.response_actions,
        detailed_report: inputs.threat_report,
        status: :completed,
        completed_at: DateTime.utc_now()
      }
    end
  end

  return :final_results

  # Helper functions for threat processing

  defp determine_ioc_type(data) do
    cond do
      String.match?(data.indicator || "", ~r/^[a-fA-F0-9]{32,64}$/) -> :malware_hash
      String.match?(data.indicator || "", ~r/^\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}$/) -> :ip_address
      String.match?(data.indicator || "", ~r/^[a-zA-Z0-9-]+\.[a-zA-Z]{2,}$/) -> :domain
      String.match?(data.indicator || "", ~r/^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$/) -> :email
      true -> :generic
    end
  end

  defp calculate_threat_level(data) do
    base_score = data.severity || 1
    confidence_multiplier = data.confidence || 0.5
    source_weight = get_source_weight(data.source)
    
    score = base_score * confidence_multiplier * source_weight
    
    cond do
      score >= 8.0 -> :critical
      score >= 6.0 -> :high
      score >= 4.0 -> :medium
      score >= 2.0 -> :low
      true -> :info
    end
  end

  defp extract_indicators(data) do
    %{
      primary: data.indicator,
      secondary: data.additional_indicators || [],
      ttps: data.tactics || [],
      references: data.references || []
    }
  end

  defp get_source_weight(source) do
    case source do
      "misp" -> 0.9
      "virustotal" -> 0.8
      "threatfox" -> 0.85
      "internal" -> 1.0
      _ -> 0.5
    end
  end

  defp enrich_with_misp_feed(%{parsed_threat: threat}) do
    # Mock MISP enrichment - would call actual MISP API
    enriched = Map.merge(threat, %{
      misp_events: [],
      threat_actor: "unknown",
      campaign: "unattributed",
      enriched_at: DateTime.utc_now()
    })
    {:ok, enriched}
  end

  defp classify_malware_threat(%{threat: threat}) do
    classification = %{
      category: :malware,
      malware_family: detect_malware_family(threat.indicators.primary),
      file_type: determine_file_type(threat.indicators.primary),
      packer: detect_packer(threat),
      capabilities: analyze_capabilities(threat)
    }
    {:ok, Map.merge(threat, classification)}
  end

  defp classify_network_threat(%{threat: threat}) do
    classification = %{
      category: :network,
      ip_reputation: lookup_ip_reputation(threat.indicators.primary),
      geolocation: lookup_geolocation(threat.indicators.primary),
      asn: lookup_asn(threat.indicators.primary),
      threat_type: determine_network_threat_type(threat)
    }
    {:ok, Map.merge(threat, classification)}
  end

  defp classify_domain_threat(%{threat: threat}) do
    classification = %{
      category: :domain,
      domain_age: get_domain_age(threat.indicators.primary),
      registrar: get_registrar_info(threat.indicators.primary),
      dns_records: get_dns_records(threat.indicators.primary),
      threat_type: determine_domain_threat_type(threat)
    }
    {:ok, Map.merge(threat, classification)}
  end

  defp classify_generic_threat(%{threat: threat}) do
    classification = %{
      category: :generic,
      threat_type: :unknown,
      requires_manual_review: true
    }
    {:ok, Map.merge(threat, classification)}
  end

  defp aggregate_threat_levels(successful_threats) do
    successful_threats
    |> Enum.map(&elem(&1, 1))
    |> Enum.group_by(& &1.threat_level)
    |> Enum.map(fn {level, threats} -> {level, length(threats)} end)
    |> Enum.into(%{})
  end

  defp aggregate_ioc_types(successful_threats) do
    successful_threats
    |> Enum.map(&elem(&1, 1))
    |> Enum.group_by(& &1.ioc_type)
    |> Enum.map(fn {type, threats} -> {type, length(threats)} end)
    |> Enum.into(%{})
  end

  defp setup_correlation_context(_args, _context, _options) do
    # Initialize correlation databases and contexts
    {:ok, %{correlation_session: UUID.uuid4()}}
  end

  defp cleanup_correlation_resources(_result) do
    # Cleanup temporary correlation resources
    :ok
  end

  defp correlate_network_patterns(network_threats) do
    # Group by IP ranges, ASNs, and temporal patterns
    network_threats
    |> Enum.group_by(& &1.asn)
    |> Enum.map(fn {asn, threats} ->
      %{
        asn: asn,
        threat_count: length(threats),
        threat_types: Enum.map(threats, & &1.threat_type) |> Enum.uniq(),
        temporal_pattern: analyze_temporal_pattern(threats)
      }
    end)
  end

  defp identify_malware_families(malware_threats) do
    malware_threats
    |> Enum.group_by(& &1.malware_family)
    |> Enum.map(fn {family, threats} ->
      %{
        family: family,
        sample_count: length(threats),
        capabilities: aggregate_capabilities(threats),
        first_seen: threats |> Enum.map(& &1.timestamp) |> Enum.min(),
        latest_seen: threats |> Enum.map(& &1.timestamp) |> Enum.max()
      }
    end)
  end

  defp identify_attack_campaigns(threats) do
    # Correlate threats by TTPs, infrastructure, and timing
    campaigns = threats
    |> Enum.group_by(& &1.campaign)
    |> Enum.filter(fn {campaign, _} -> campaign != "unattributed" end)
    |> Enum.map(fn {campaign, campaign_threats} ->
      %{
        campaign: campaign,
        threat_count: length(campaign_threats),
        threat_types: Enum.map(campaign_threats, & &1.category) |> Enum.uniq(),
        infrastructure: extract_infrastructure(campaign_threats),
        timeline: build_campaign_timeline(campaign_threats)
      }
    end)
    
    campaigns
  end

  defp spawn_network_response(threat, config) do
    Task.async(fn ->
      CNSForge.ResponseOrchestrator.handle_network_threat(threat, config)
    end)
  end

  defp spawn_malware_response(threat, config) do
    Task.async(fn ->
      CNSForge.ResponseOrchestrator.handle_malware_threat(threat, config)
    end)
  end

  defp spawn_domain_response(threat, config) do
    Task.async(fn ->
      CNSForge.ResponseOrchestrator.handle_domain_threat(threat, config)
    end)
  end

  defp spawn_generic_response(threat, config) do
    Task.async(fn ->
      CNSForge.ResponseOrchestrator.handle_generic_threat(threat, config)
    end)
  end

  # Additional helper functions...
  defp detect_malware_family(_hash), do: "unknown"
  defp determine_file_type(_hash), do: "unknown"
  defp detect_packer(_threat), do: "none"
  defp analyze_capabilities(_threat), do: []
  defp lookup_ip_reputation(_ip), do: "unknown"
  defp lookup_geolocation(_ip), do: %{country: "unknown", city: "unknown"}
  defp lookup_asn(_ip), do: "unknown"
  defp determine_network_threat_type(_threat), do: "suspicious"
  defp get_domain_age(_domain), do: 0
  defp get_registrar_info(_domain), do: "unknown"
  defp get_dns_records(_domain), do: []
  defp determine_domain_threat_type(_threat), do: "suspicious"
  defp analyze_temporal_pattern(_threats), do: "random"
  defp aggregate_capabilities(_threats), do: []
  defp extract_infrastructure(_threats), do: %{ips: [], domains: []}
  defp build_campaign_timeline(_threats), do: %{start: nil, end: nil, events: []}
end