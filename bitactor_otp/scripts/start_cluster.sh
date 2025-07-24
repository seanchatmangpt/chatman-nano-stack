#!/bin/bash
# BitActor Cluster Startup Script
# Deploys distributed BitActor nodes with semantic integration
# Copyright (C) 2025 CNS - Chatman Nano Stack

set -e

# Configuration
CLUSTER_NAME=${CLUSTER_NAME:-"bitactor_cluster"}
NODE_COUNT=${NODE_COUNT:-3}
BASE_PORT=${BASE_PORT:-9001}
LOG_DIR="logs"
COOKIE=${COOKIE:-"bitactor_ultrathink_swarm"}

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo_colored() {
    echo -e "${1}${2}${NC}"
}

# Create log directory
mkdir -p $LOG_DIR

echo_colored $BLUE "=== BitActor Cluster Deployment ==="
echo_colored $BLUE "Cluster: $CLUSTER_NAME"
echo_colored $BLUE "Nodes: $NODE_COUNT"
echo_colored $BLUE "Base Port: $BASE_PORT"
echo ""

# Function to start a single node
start_node() {
    local node_id=$1
    local port=$((BASE_PORT + node_id))
    local node_name="${CLUSTER_NAME}_node${node_id}@localhost"
    local log_file="$LOG_DIR/node${node_id}.log"
    
    echo_colored $YELLOW "Starting node: $node_name (port: $port)"
    
    # Start Erlang node with BitActor
    erl \
        -name "$node_name" \
        -setcookie "$COOKIE" \
        -pa _build/default/lib/*/ebin \
        -kernel inet_dist_listen_min $port inet_dist_listen_max $port \
        -s bitactor_app start \
        -eval "
            timer:sleep(2000),
            {ok, _} = bitactor_cluster:start_link(),
            io:format('Node ~p started successfully~n', [node()]),
            case $node_id of
                1 -> 
                    io:format('Master node ready for cluster formation~n');
                _ ->
                    timer:sleep(3000),
                    MasterNode = '${CLUSTER_NAME}_node1@localhost',
                    case bitactor_cluster:join_cluster(MasterNode) of
                        ok -> 
                            io:format('Joined cluster successfully~n');
                        Error -> 
                            io:format('Failed to join cluster: ~p~n', [Error])
                    end
            end
        " \
        -detached \
        -heart \
        -env ERL_CRASH_DUMP "$LOG_DIR/node${node_id}_crash.dump" \
        > "$log_file" 2>&1 &
    
    local pid=$!
    echo "$pid" > "$LOG_DIR/node${node_id}.pid"
    
    echo_colored $GREEN "✓ Node $node_id started (PID: $pid)"
}

# Function to check node status
check_node() {
    local node_id=$1
    local node_name="${CLUSTER_NAME}_node${node_id}@localhost"
    
    # Check if Erlang node is running
    if epmd -names | grep -q "node${node_id}"; then
        echo_colored $GREEN "✓ Node $node_id: Running"
        
        # Check BitActor cluster status
        erl -name "checker@localhost" -setcookie "$COOKIE" -noshell -eval "
            case rpc:call('$node_name', bitactor_cluster, get_cluster_status, [], 5000) of
                {badrpc, _} -> 
                    io:format('  Status: BitActor cluster not responding~n');
                Status when is_map(Status) ->
                    NodeCount = maps:get(node_count, Status, 0),
                    ClusterID = maps:get(cluster_id, Status, unknown),
                    io:format('  Cluster ID: ~p~n', [ClusterID]),
                    io:format('  Cluster Size: ~p nodes~n', [NodeCount])
            end,
            init:stop().
        " 2>/dev/null || echo_colored $YELLOW "  Status: Cluster service not ready"
    else
        echo_colored $RED "✗ Node $node_id: Not running"
    fi
}

# Function to stop a node
stop_node() {
    local node_id=$1
    local node_name="${CLUSTER_NAME}_node${node_id}@localhost"
    local pid_file="$LOG_DIR/node${node_id}.pid"
    
    echo_colored $YELLOW "Stopping node: $node_name"
    
    # Try graceful shutdown first
    if epmd -names | grep -q "node${node_id}"; then
        erl -name "stopper@localhost" -setcookie "$COOKIE" -noshell -eval "
            rpc:call('$node_name', init, stop, []),
            init:stop().
        " 2>/dev/null || true
        
        sleep 2
    fi
    
    # Force kill if still running
    if [ -f "$pid_file" ]; then
        local pid=$(cat "$pid_file")
        if ps -p "$pid" > /dev/null 2>&1; then
            kill "$pid" 2>/dev/null || true
            sleep 1
            kill -9 "$pid" 2>/dev/null || true
        fi
        rm -f "$pid_file"
    fi
    
    echo_colored $GREEN "✓ Node $node_id stopped"
}

# Function to deploy semantic ontologies to cluster
deploy_semantics() {
    echo_colored $BLUE "Deploying semantic ontologies to cluster..."
    
    local master_node="${CLUSTER_NAME}_node1@localhost"
    
    # Load ontologies on master node
    erl -name "semantic_deployer@localhost" -setcookie "$COOKIE" -noshell -eval "
        Domains = [autonomous_vehicle, smart_grid, cybersecurity, industrial_iot, healthcare],
        lists:foreach(fun(Domain) ->
            OntologyFile = filename:join([\"priv\", \"ontologies\", atom_to_list(Domain) ++ \"_core.ttl\"]),
            case file:read_file(OntologyFile) of
                {ok, TTLContent} ->
                    case rpc:call('$master_node', bitactor_semantic, load_ontology, [Domain, TTLContent], 10000) of
                        ok ->
                            io:format('✓ Loaded ~p ontology~n', [Domain]);
                        Error ->
                            io:format('✗ Failed to load ~p: ~p~n', [Domain, Error])
                    end;
                {error, enoent} ->
                    io:format('⚠ Ontology file not found: ~s~n', [OntologyFile]);
                Error ->
                    io:format('✗ Error reading ~s: ~p~n', [OntologyFile, Error])
            end
        end, Domains),
        init:stop().
    " 2>/dev/null || echo_colored $RED "Failed to deploy semantics"
}

# Function to run cluster benchmarks
benchmark_cluster() {
    echo_colored $BLUE "Running cluster benchmarks..."
    
    local master_node="${CLUSTER_NAME}_node1@localhost"
    
    erl -name "benchmark@localhost" -setcookie "$COOKIE" -noshell -eval "
        % Spawn distributed actors
        Domains = [autonomous_vehicle, smart_grid, cybersecurity],
        lists:foreach(fun(Domain) ->
            case rpc:call('$master_node', bitactor_cluster, spawn_distributed_actor, [Domain, test_actor, #{}], 10000) of
                {ok, ActorRef, Node} ->
                    io:format('✓ Spawned ~p actor on ~p: ~p~n', [Domain, Node, ActorRef]);
                Error ->
                    io:format('✗ Failed to spawn ~p actor: ~p~n', [Domain, Error])
            end
        end, Domains),
        
        % Get cluster status
        case rpc:call('$master_node', bitactor_cluster, get_cluster_status, [], 10000) of
            Status when is_map(Status) ->
                io:format('~n=== Cluster Status ===~n'),
                io:format('Cluster ID: ~p~n', [maps:get(cluster_id, Status)]),
                io:format('Nodes: ~p~n', [maps:get(cluster_nodes, Status)]),
                io:format('Node Count: ~p~n', [maps:get(node_count, Status)]);
            Error ->
                io:format('Error getting cluster status: ~p~n', [Error])
        end,
        init:stop().
    " 2>/dev/null || echo_colored $RED "Benchmarks failed"
}

# Main command handling
case "${1:-start}" in
    "start")
        echo_colored $BLUE "Starting BitActor cluster..."
        
        # Compile first
        make compile
        
        # Start nodes sequentially
        for i in $(seq 1 $NODE_COUNT); do
            start_node $i
            sleep 2
        done
        
        echo_colored $GREEN "Cluster startup initiated. Waiting for nodes to join..."
        sleep 8
        
        # Deploy semantic ontologies
        deploy_semantics
        
        echo_colored $GREEN "BitActor cluster deployment complete!"
        echo ""
        echo_colored $BLUE "Use '$0 status' to check cluster health"
        echo_colored $BLUE "Use '$0 benchmark' to run performance tests"
        echo_colored $BLUE "Use '$0 stop' to shutdown the cluster"
        ;;
        
    "status")
        echo_colored $BLUE "=== BitActor Cluster Status ==="
        for i in $(seq 1 $NODE_COUNT); do
            check_node $i
        done
        
        # Show EPMD registered nodes
        echo ""
        echo_colored $BLUE "=== EPMD Registered Nodes ==="
        epmd -names | grep -E "node[0-9]+" || echo "No BitActor nodes found"
        ;;
        
    "stop")
        echo_colored $BLUE "Stopping BitActor cluster..."
        for i in $(seq 1 $NODE_COUNT); do
            stop_node $i
        done
        echo_colored $GREEN "Cluster stopped"
        ;;
        
    "benchmark")
        benchmark_cluster
        ;;
        
    "logs")
        node_id=${2:-1}
        if [ -f "$LOG_DIR/node${node_id}.log" ]; then
            tail -f "$LOG_DIR/node${node_id}.log"
        else
            echo_colored $RED "Log file not found: $LOG_DIR/node${node_id}.log"
        fi
        ;;
        
    "shell")
        node_id=${2:-1}
        node_name="${CLUSTER_NAME}_node${node_id}@localhost"
        echo_colored $BLUE "Connecting to $node_name..."
        erl -name "shell@localhost" -setcookie "$COOKIE" -remsh "$node_name"
        ;;
        
    "help"|"-h"|"--help")
        echo_colored $BLUE "BitActor Cluster Management"
        echo ""
        echo "Usage: $0 [command] [options]"
        echo ""
        echo "Commands:"
        echo "  start     - Start the cluster (default)"
        echo "  status    - Check cluster status"
        echo "  stop      - Stop the cluster"
        echo "  benchmark - Run cluster benchmarks"
        echo "  logs [N]  - Show logs for node N (default: 1)"
        echo "  shell [N] - Connect to node N shell (default: 1)"
        echo "  help      - Show this help"
        echo ""
        echo "Environment Variables:"
        echo "  CLUSTER_NAME - Cluster name (default: bitactor_cluster)"
        echo "  NODE_COUNT   - Number of nodes (default: 3)"
        echo "  BASE_PORT    - Base port number (default: 9001)"
        echo "  COOKIE       - Erlang cookie (default: bitactor_ultrathink_swarm)"
        ;;
        
    *)
        echo_colored $RED "Unknown command: $1"
        echo "Use '$0 help' for usage information"
        exit 1
        ;;
esac