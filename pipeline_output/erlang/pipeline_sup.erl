-module(pipeline_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        {datastream_worker, {pipeline_datastream, start_link, []}, permanent, 5000, worker, [pipeline_datastream]},
        {processor_worker, {pipeline_processor, start_link, []}, permanent, 5000, worker, [pipeline_processor]},
        {pattern_worker, {pipeline_pattern, start_link, []}, permanent, 5000, worker, [pipeline_pattern]},
        {alert_worker, {pipeline_alert, start_link, []}, permanent, 5000, worker, [pipeline_alert]}
    ],
    {ok, {{one_for_one, 5, 10}, Children}}.
