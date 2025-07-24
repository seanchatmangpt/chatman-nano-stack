%%%-------------------------------------------------------------------
%%% @doc BitActor Test Runner - Properly Starts Application
%%% Ensures proper initialization for UHFT tests
%%% @copyright 2025 CNS - Chatman Nano Stack
%%%-------------------------------------------------------------------
-module(bitactor_test_runner).
-export([run_uhft_tests/0, run_with_app/1]).

%% Run UHFT tests with proper application startup
run_uhft_tests() ->
    io:format("=== BitActor Test Runner - Starting Application ===~n"),
    
    %% Ensure BitActor application is started
    case ensure_bitactor_started() of
        ok ->
            io:format("✓ BitActor application started successfully~n"),
            %% Run the comprehensive tests
            Result = uhft_comprehensive_test:run_all_tests(),
            io:format("~n=== Test Runner Complete ===~n"),
            Result;
        {error, Reason} ->
            io:format("✗ Failed to start BitActor application: ~p~n", [Reason]),
            #{
                test_status => failed,
                reason => {app_start_failed, Reason},
                tests_run => 0,
                tests_passed => 0
            }
    end.

%% Generic function to run any function with BitActor app started
run_with_app(Fun) when is_function(Fun, 0) ->
    case ensure_bitactor_started() of
        ok ->
            try
                Fun()
            catch
                Type:Error ->
                    io:format("Error running function: ~p:~p~n", [Type, Error]),
                    {error, {Type, Error}}
            end;
        {error, Reason} ->
            {error, {app_start_failed, Reason}}
    end.

%% Internal function to ensure BitActor is started
ensure_bitactor_started() ->
    %% Use application:ensure_all_started for automatic dependency resolution
    case application:ensure_all_started(bitactor) of
        {ok, _Started} ->
            %% Wait a moment for initialization
            timer:sleep(100),
            %% Verify server is running
            case whereis(bitactor_server) of
                undefined ->
                    %% Try to start server manually
                    case bitactor_server:start_link() of
                        {ok, _Pid} ->
                            timer:sleep(100),
                            ok;
                        Error ->
                            {error, {server_start_failed, Error}}
                    end;
                _Pid ->
                    ok
            end;
        {error, Reason} ->
            {error, Reason}
    end.