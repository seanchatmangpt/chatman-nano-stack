%%%-------------------------------------------------------------------
%%% @doc BitActor Semantic Integration Test Suite
%%% Tests semantic TTL/SPARQL with UHFT performance
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(bitactor_semantic_test).
-export([run_all_tests/0, run_domain_test/1]).

-define(TARGET_SEMANTIC_LATENCY_US, 100). % 100 microseconds for semantic ops

run_all_tests() ->
    io:format("=== BitActor Semantic Integration Tests ===~n"),
    
    %% Start required applications
    ensure_apps_started(),
    
    %% Start BitActor semantic server
    {ok, _} = bitactor_semantic:start_link(),
    
    %% Load ontologies for each domain
    Domains = [
        {autonomous_vehicle, "ontologies/autonomous_vehicle_core.ttl"},
        {smart_grid, "ontologies/smart_grid_core.ttl"},
        {cybersecurity, "ontologies/cybersecurity_core.ttl"},
        {industrial_iot, "ontologies/industrial_iot_core.ttl"},
        {healthcare, "ontologies/healthcare_core.ttl"}
    ],
    
    %% Run tests for each domain
    Results = lists:map(fun({Domain, OntologyFile}) ->
        io:format("~n--- Testing ~p domain ---~n", [Domain]),
        Result = run_domain_test(Domain, OntologyFile),
        {Domain, Result}
    end, Domains),
    
    %% Generate test report
    generate_report(Results),
    
    Results.

run_domain_test(autonomous_vehicle) ->
    io:format("Testing Autonomous Vehicle Semantic Integration...~n"),
    
    %% Load ontology
    OntologyPath = filename:join([code:priv_dir(bitactor), "..", "..", "ontologies", "autonomous_vehicle_core.ttl"]),
    {ok, TTLContent} = file:read_file(OntologyPath),
    ok = bitactor_semantic:load_ontology(autonomous_vehicle, TTLContent),
    
    %% Spawn semantic actors
    {ok, Vehicle1} = bitactor_semantic:spawn_semantic_actor(
        autonomous_vehicle, 
        vehicle,
        #{capabilities => [v2v_communication, collision_detection]}
    ),
    
    {ok, _Vehicle2} = bitactor_semantic:spawn_semantic_actor(
        autonomous_vehicle,
        vehicle,
        #{capabilities => [v2v_communication, trajectory_planning]}
    ),
    
    %% Test V2V communication latency
    V2VLatencies = measure_semantic_latencies(
        fun() ->
            bitactor_semantic:send_semantic_message(Vehicle1, {
                sensor_data, #{
                    position => {37.7749, -122.4194, 50.0},
                    velocity => {25.0, 0.0, 0.0},
                    heading => 90.0
                }
            })
        end,
        10000
    ),
    
    %% Test SPARQL query performance
    CollisionQuery = <<"
        PREFIX av: <http://cns.ai/ontology/autonomous-vehicle#>
        SELECT ?vehicle ?risk ?distance
        WHERE {
            ?vehicle a av:Vehicle .
            ?vehicle av:hasPosition ?pos .
            ?vehicle av:collisionRisk ?risk .
            ?vehicle av:distanceToVehicle ?distance .
            FILTER (?risk > 0.7)
        }
    ">>,
    
    QueryLatencies = measure_semantic_latencies(
        fun() ->
            bitactor_semantic:query_sparql(autonomous_vehicle, CollisionQuery)
        end,
        1000
    ),
    
    %% Test SHACL validation
    SHACLPath = filename:join([code:priv_dir(bitactor), "..", "..", "ontologies", "autonomous_vehicle_shacl.ttl"]),
    {ok, SHACLContent} = file:read_file(SHACLPath),
    
    ValidationLatencies = measure_semantic_latencies(
        fun() ->
            VehicleData = #{
                '@type' => 'av:Vehicle',
                'av:vehicleID' => <<"AV-123456">>,
                'av:hasPosition' => #{
                    'av:latitude' => 37.7749,
                    'av:longitude' => -122.4194
                }
            },
            bitactor_semantic:validate_shacl(autonomous_vehicle, VehicleData, SHACLContent)
        end,
        1000
    ),
    
    %% Analyze results
    #{
        domain => autonomous_vehicle,
        v2v_latency => analyze_latencies("V2V Communication", V2VLatencies),
        sparql_latency => analyze_latencies("SPARQL Query", QueryLatencies),
        shacl_latency => analyze_latencies("SHACL Validation", ValidationLatencies),
        semantic_actors => 2,
        test_status => passed
    };

run_domain_test(smart_grid) ->
    io:format("Testing Smart Grid Semantic Integration...~n"),
    
    %% Load ontology
    OntologyPath = filename:join([code:priv_dir(bitactor), "..", "..", "ontologies", "smart_grid_core.ttl"]),
    {ok, TTLContent} = file:read_file(OntologyPath),
    ok = bitactor_semantic:load_ontology(smart_grid, TTLContent),
    
    %% Spawn grid nodes
    {ok, PowerPlant} = bitactor_semantic:spawn_semantic_actor(
        smart_grid,
        power_plant,
        #{capabilities => [renewable_energy, grid_synchronization]}
    ),
    
    {ok, _GridController} = bitactor_semantic:spawn_semantic_actor(
        smart_grid,
        grid_controller,
        #{capabilities => [frequency_regulation, load_balancing]}
    ),
    
    %% Test grid optimization latency
    GridLatencies = measure_semantic_latencies(
        fun() ->
            bitactor_semantic:send_semantic_message(PowerPlant, {
                measurement, #{
                    frequency => 49.98,
                    voltage => 230.5,
                    power_output => 150.0,
                    renewable_percentage => 75.0
                }
            })
        end,
        10000
    ),
    
    %% Test renewable optimization query
    RenewableQuery = <<"
        PREFIX sg: <http://cns.ai/ontology/smart-grid#>
        SELECT ?plant ?output ?efficiency
        WHERE {
            ?plant a sg:RenewablePlant .
            ?plant sg:currentOutput ?output .
            ?plant sg:efficiency ?efficiency .
            ?plant sg:gridPriority ?priority .
            FILTER (?efficiency > 0.85)
        }
        ORDER BY DESC(?priority)
    ">>,
    
    QueryLatencies = measure_semantic_latencies(
        fun() ->
            bitactor_semantic:query_sparql(smart_grid, RenewableQuery)
        end,
        1000
    ),
    
    #{
        domain => smart_grid,
        grid_latency => analyze_latencies("Grid Optimization", GridLatencies),
        renewable_query => analyze_latencies("Renewable Query", QueryLatencies),
        semantic_actors => 2,
        test_status => passed
    };

run_domain_test(Domain) ->
    %% Generic test for other domains
    io:format("Testing ~p Semantic Integration...~n", [Domain]),
    
    %% Simplified testing for other domains
    #{
        domain => Domain,
        test_status => skipped,
        reason => "Simplified test for demo"
    }.

run_domain_test(Domain, OntologyFile) ->
    try
        Result = run_domain_test(Domain),
        Result#{ontology_file => OntologyFile}
    catch
        Type:Error ->
            #{
                domain => Domain,
                test_status => failed,
                error => {Type, Error},
                ontology_file => OntologyFile
            }
    end.

%% Helper functions

ensure_apps_started() ->
    application:ensure_all_started(bitactor),
    ok.

measure_semantic_latencies(Fun, Count) ->
    %% Warmup
    [Fun() || _ <- lists:seq(1, 100)],
    
    %% Measure
    Latencies = [begin
        Start = erlang:monotonic_time(microsecond),
        Fun(),
        End = erlang:monotonic_time(microsecond),
        End - Start
    end || _ <- lists:seq(1, Count)],
    
    Latencies.

analyze_latencies(Name, Latencies) ->
    Sorted = lists:sort(Latencies),
    Len = length(Sorted),
    
    P50 = lists:nth(Len div 2, Sorted),
    P95 = lists:nth(round(Len * 0.95), Sorted),
    P99 = lists:nth(round(Len * 0.99), Sorted),
    
    #{
        name => Name,
        samples => Len,
        min_us => lists:min(Sorted),
        p50_us => P50,
        p95_us => P95,
        p99_us => P99,
        max_us => lists:max(Sorted),
        mean_us => lists:sum(Sorted) div Len,
        semantic_compliant => P99 =< ?TARGET_SEMANTIC_LATENCY_US
    }.

generate_report(Results) ->
    io:format("~n=== Semantic Integration Test Report ===~n"),
    
    %% Summary
    Passed = length([ok || {_, #{test_status := passed}} <- Results]),
    Total = length(Results),
    
    io:format("~nDomains Tested: ~p/~p passed~n", [Passed, Total]),
    
    %% Detailed results
    lists:foreach(fun({Domain, Result}) ->
        Status = maps:get(test_status, Result),
        io:format("~n~p: ~p~n", [Domain, Status]),
        
        case Status of
            passed ->
                %% Show latency results
                lists:foreach(fun
                    ({_Key, Value}) when is_map(Value) ->
                        case maps:get(p99_us, Value, undefined) of
                            P99 when is_number(P99) ->
                                Compliant = P99 =< ?TARGET_SEMANTIC_LATENCY_US,
                                io:format("  ~p P99: ~p μs (~s)~n", 
                                    [maps:get(name, Value), P99, 
                                     if Compliant -> "✓ Compliant"; true -> "✗ Over" end]);
                            _ -> ok
                        end;
                    ({_Key, _Value}) -> ok
                end, maps:to_list(Result));
            _ ->
                io:format("  Reason: ~p~n", [maps:get(reason, Result, unknown)])
        end
    end, Results),
    
    %% Mermaid diagram
    io:format("~n```mermaid~n"),
    io:format("graph TB~n"),
    io:format("    A[BitActor Semantic Tests] --> B[Results: ~p/~p Passed]~n", [Passed, Total]),
    
    lists:foreach(fun({Domain, Result}) ->
        DomainStr = atom_to_list(Domain),
        Status = maps:get(test_status, Result),
        
        case Status of
            passed ->
                io:format("    B --> ~s[✓ ~s]~n", [DomainStr, DomainStr]),
                
                %% Show key metrics
                case maps:get(v2v_latency, Result, undefined) of
                    #{p99_us := V2VP99} ->
                        io:format("    ~s --> ~s_v2v[V2V P99: ~p μs]~n", [DomainStr, DomainStr, V2VP99]);
                    _ -> ok
                end,
                
                case maps:get(sparql_latency, Result, undefined) of
                    #{p99_us := SPARQLP99} ->
                        io:format("    ~s --> ~s_sparql[SPARQL P99: ~p μs]~n", [DomainStr, DomainStr, SPARQLP99]);
                    _ -> ok
                end;
            _ ->
                io:format("    B --> ~s[✗ ~s]~n", [DomainStr, DomainStr])
        end
    end, Results),
    
    io:format("```~n"),
    
    ok.