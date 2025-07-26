#!/usr/bin/env python3
"""
CNS Aegis Fabric TTL-Driven Template Engine
Generates all system components from TTL ontology using Jinja2 templates
"""

import rdflib
from rdflib import Graph, Namespace, URIRef, Literal
from jinja2 import Environment, FileSystemLoader, Template
import json
import yaml
import os
from pathlib import Path
from typing import Dict, List, Any, Optional
import subprocess

class AegisTTLTemplateEngine:
    """TTL-driven code generation engine for Aegis Fabric"""
    
    def __init__(self, ttl_file: str, template_dir: str = "templates/"):
        self.graph = Graph()
        self.graph.parse(ttl_file, format="turtle")
        
        # Define namespaces
        self.AEGIS = Namespace("http://cns.io/aegis/fabric#")
        self.CNS = Namespace("http://cns.io/core#")
        self.SEC = Namespace("http://cns.io/security#")
        self.PERF = Namespace("http://cns.io/performance#")
        self.DEPLOY = Namespace("http://cns.io/deployment#")
        self.GOSSIP = Namespace("http://cns.io/gossip#")
        
        # Bind namespaces
        self.graph.bind("aegis", self.AEGIS)
        self.graph.bind("cns", self.CNS)
        self.graph.bind("sec", self.SEC)
        self.graph.bind("perf", self.PERF)
        self.graph.bind("deploy", self.DEPLOY)
        self.graph.bind("gossip", self.GOSSIP)
        
        # Setup Jinja2
        self.template_dir = Path(template_dir)
        self.template_dir.mkdir(exist_ok=True)
        self.env = Environment(
            loader=FileSystemLoader(str(self.template_dir)),
            trim_blocks=True,
            lstrip_blocks=True,
            autoescape=False
        )
        
    def extract_bitactor_config(self) -> Dict[str, Any]:
        """Extract BitActor configuration from TTL"""
        config = {}
        
        # Query BitActor properties
        for s, p, o in self.graph.triples((None, self.AEGIS.sourceFile, None)):
            if "bitactor_production.c" in str(o):
                bitactor_uri = s
                
                # Extract all properties
                for prop, value in self.graph.predicate_objects(bitactor_uri):
                    prop_name = str(prop).split('#')[-1]
                    config[prop_name] = str(value)
        
        # Extract performance requirements
        perf_config = {}
        for s, p, o in self.graph.triples((None, self.PERF.maxLatencyMicroseconds, None)):
            perf_config['max_latency_us'] = int(o)
        for s, p, o in self.graph.triples((None, self.PERF.minThroughputOpsPerSec, None)):
            perf_config['min_throughput_ops'] = int(o)
        for s, p, o in self.graph.triples((None, self.PERF.maxCpuUtilizationPercent, None)):
            perf_config['max_cpu_percent'] = int(o)
        
        config['performance'] = perf_config
        
        # Extract security settings
        sec_config = {}
        for s, p, o in self.graph.triples((None, self.SEC.memoryProtection, None)):
            sec_config['memory_protection'] = str(o)
        for s, p, o in self.graph.triples((None, self.SEC.stackProtection, None)):
            sec_config['stack_protection'] = str(o)
        for s, p, o in self.graph.triples((None, self.SEC.aslrEnabled, None)):
            sec_config['aslr_enabled'] = str(o).lower() == 'true'
        
        config['security'] = sec_config
        
        return config
    
    def extract_gossip_protocol_config(self) -> Dict[str, Any]:
        """Extract Gossip Protocol configuration from TTL"""
        config = {}
        
        # Query Gossip Protocol properties
        for s, p, o in self.graph.triples((None, self.GOSSIP.fanOut, None)):
            config['fan_out'] = int(o)
        for s, p, o in self.graph.triples((None, self.GOSSIP.maxHops, None)):
            config['max_hops'] = int(o)
        for s, p, o in self.graph.triples((None, self.GOSSIP.convergenceTimeMs, None)):
            config['convergence_time_ms'] = int(o)
        for s, p, o in self.graph.triples((None, self.GOSSIP.messageFormat, None)):
            config['message_format'] = str(o)
        for s, p, o in self.graph.triples((None, self.GOSSIP.compressionAlgorithm, None)):
            config['compression_algorithm'] = str(o)
        for s, p, o in self.graph.triples((None, self.GOSSIP.encryptionAlgorithm, None)):
            config['encryption_algorithm'] = str(o)
        for s, p, o in self.graph.triples((None, self.GOSSIP.integrityCheck, None)):
            config['integrity_check'] = str(o)
        
        return config
    
    def extract_deployment_config(self) -> Dict[str, Any]:
        """Extract Kubernetes deployment configuration from TTL"""
        config = {}
        
        # Query deployment properties
        for s, p, o in self.graph.triples((None, self.DEPLOY.namespace, None)):
            config['namespace'] = str(o)
        for s, p, o in self.graph.triples((None, self.DEPLOY.replicaCount, None)):
            config['replica_count'] = int(o)
        for s, p, o in self.graph.triples((None, self.DEPLOY.antiAffinity, None)):
            config['anti_affinity'] = str(o)
        for s, p, o in self.graph.triples((None, self.DEPLOY.podDisruptionBudget, None)):
            config['pod_disruption_budget'] = int(o)
        
        # Extract network policy
        network_config = {}
        for s, p, o in self.graph.triples((None, self.DEPLOY.ingressPolicy, None)):
            network_config['ingress_policy'] = str(o)
        for s, p, o in self.graph.triples((None, self.DEPLOY.egressPolicy, None)):
            network_config['egress_policy'] = str(o)
        for s, p, o in self.graph.triples((None, self.DEPLOY.gossipPort, None)):
            network_config['gossip_port'] = int(o)
        for s, p, o in self.graph.triples((None, self.DEPLOY.metricsPort, None)):
            network_config['metrics_port'] = int(o)
        
        config['network_policy'] = network_config
        
        # Extract instance configuration
        instance_config = {}
        
        # Query for ProductionFabric instance
        production_fabric = None
        for s, p, o in self.graph.triples((None, self.AEGIS.instanceCount, None)):
            instance_config['instance_count'] = int(o)
        for s, p, o in self.graph.triples((None, self.AEGIS.cpuRequest, None)):
            instance_config['cpu_request'] = str(o)
        for s, p, o in self.graph.triples((None, self.AEGIS.cpuLimit, None)):
            instance_config['cpu_limit'] = str(o)
        for s, p, o in self.graph.triples((None, self.AEGIS.memoryRequest, None)):
            instance_config['memory_request'] = str(o)
        for s, p, o in self.graph.triples((None, self.AEGIS.memoryLimit, None)):
            instance_config['memory_limit'] = str(o)
        
        config['resources'] = instance_config
        
        return config
    
    def extract_validation_config(self) -> Dict[str, Any]:
        """Extract validation configuration from TTL"""
        config = {}
        
        for s, p, o in self.graph.triples((None, self.AEGIS.primaryValidator, None)):
            config['primary_validator'] = str(o)
        for s, p, o in self.graph.triples((None, self.AEGIS.penetrationTester, None)):
            config['penetration_tester'] = str(o)
        for s, p, o in self.graph.triples((None, self.AEGIS.serviceMeshTester, None)):
            config['service_mesh_tester'] = str(o)
        for s, p, o in self.graph.triples((None, self.AEGIS.chaosEngine, None)):
            config['chaos_engine'] = str(o)
        for s, p, o in self.graph.triples((None, self.AEGIS.requiredPassRate, None)):
            config['required_pass_rate'] = float(o)
        
        return config
    
    def generate_all_components(self, output_dir: str = "generated/"):
        """Generate all system components from TTL"""
        output_path = Path(output_dir)
        output_path.mkdir(exist_ok=True)
        
        # Extract all configurations
        bitactor_config = self.extract_bitactor_config()
        gossip_config = self.extract_gossip_protocol_config()
        deployment_config = self.extract_deployment_config()
        validation_config = self.extract_validation_config()
        
        # Create master configuration
        master_config = {
            'bitactor': bitactor_config,
            'gossip_protocol': gossip_config,
            'deployment': deployment_config,
            'validation': validation_config
        }
        
        # Save master configuration
        with open(output_path / "aegis_config.json", 'w') as f:
            json.dump(master_config, f, indent=2)
        
        # Generate components
        self._generate_bitactor_c_code(bitactor_config, output_path)
        self._generate_erlang_service_mesh(gossip_config, output_path)
        self._generate_kubernetes_manifests(deployment_config, output_path)
        self._generate_terraform_configuration(deployment_config, output_path)
        self._generate_validation_suite(validation_config, output_path)
        self._generate_dockerfile(bitactor_config, output_path)
        
        print(f"✅ Generated all components in {output_path}")
        return master_config
    
    def _generate_bitactor_c_code(self, config: Dict[str, Any], output_path: Path):
        """Generate BitActor C code from configuration"""
        # Create the Jinja2 template for BitActor C code
        template_content = '''/* 
 * Generated BitActor Implementation
 * Source: TTL Ontology
 * Performance Target: {{ performance.min_throughput_ops }} ops/sec, < {{ performance.max_latency_us }}μs latency
 */

#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <pthread.h>
#include <time.h>

{% if security.memory_protection == "NX_DEP" %}
#define _GNU_SOURCE
#include <sys/mman.h>
{% endif %}

{% if security.stack_protection == "CANARY" %}
#define STACK_CANARY 0xDEADBEEF
#endif

// Performance constants from TTL
#define MAX_LATENCY_US {{ performance.max_latency_us }}
#define MIN_THROUGHPUT_OPS {{ performance.min_throughput_ops }}
#define MAX_CPU_PERCENT {{ performance.max_cpu_percent }}

// BitActor structure
typedef struct {
    uint64_t id;
    uint8_t* memory_pool;
    size_t pool_size;
    pthread_mutex_t lock;
    {% if security.stack_protection == "CANARY" %}
    uint32_t canary;
    {% endif %}
} bitactor_t;

// Threat signature structure
typedef struct {
    uint32_t signature_id;
    uint8_t pattern[64];
    uint8_t pattern_len;
    uint8_t severity;
} threat_signature_t;

// Initialize BitActor with security hardening
int bitactor_init(bitactor_t* actor, uint64_t id, size_t pool_size) {
    actor->id = id;
    actor->pool_size = pool_size;
    
    {% if security.memory_protection == "NX_DEP" %}
    // Allocate memory with NX protection
    actor->memory_pool = mmap(NULL, pool_size, 
                             PROT_READ | PROT_WRITE,
                             MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (actor->memory_pool == MAP_FAILED) {
        return -1;
    }
    {% else %}
    actor->memory_pool = malloc(pool_size);
    if (!actor->memory_pool) {
        return -1;
    }
    {% endif %}
    
    pthread_mutex_init(&actor->lock, NULL);
    
    {% if security.stack_protection == "CANARY" %}
    actor->canary = STACK_CANARY;
    {% endif %}
    
    return 0;
}

// High-performance threat detection
bool bitactor_detect_threat(bitactor_t* actor, const uint8_t* data, size_t len) {
    {% if security.stack_protection == "CANARY" %}
    if (actor->canary != STACK_CANARY) {
        // Stack corruption detected
        abort();
    }
    {% endif %}
    
    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start);
    
    // Ultra-fast pattern matching using SIMD when available
    bool threat_detected = false;
    
    // TODO: Implement actual threat detection logic
    // This is a placeholder for the real implementation
    
    clock_gettime(CLOCK_MONOTONIC, &end);
    uint64_t latency_ns = (end.tv_sec - start.tv_sec) * 1000000000 + 
                         (end.tv_nsec - start.tv_nsec);
    
    if (latency_ns > MAX_LATENCY_US * 1000) {
        // Log performance violation
    }
    
    return threat_detected;
}

// Cleanup
void bitactor_destroy(bitactor_t* actor) {
    if (actor->memory_pool) {
        {% if security.memory_protection == "NX_DEP" %}
        munmap(actor->memory_pool, actor->pool_size);
        {% else %}
        free(actor->memory_pool);
        {% endif %}
    }
    pthread_mutex_destroy(&actor->lock);
}
'''
        
        # Save template
        template_path = self.template_dir / "bitactor.c.j2"
        with open(template_path, 'w') as f:
            f.write(template_content)
        
        # Render template
        template = self.env.get_template("bitactor.c.j2")
        rendered = template.render(**config)
        
        # Save generated code
        with open(output_path / "bitactor_generated.c", 'w') as f:
            f.write(rendered)
    
    def _generate_erlang_service_mesh(self, config: Dict[str, Any], output_path: Path):
        """Generate Erlang/OTP service mesh code"""
        template_content = '''%%%-------------------------------------------------------------------
%%% @doc
%%% Generated Aegis Fabric Service Mesh with Gossip Protocol
%%% Source: TTL Ontology
%%% @end
%%%-------------------------------------------------------------------
-module(aegis_gossip_mesh).

-behaviour(gen_server).

%% API
-export([start_link/1, broadcast_threat/2, join_cluster/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(FAN_OUT, {{ fan_out }}).
-define(MAX_HOPS, {{ max_hops }}).
-define(CONVERGENCE_TIME_MS, {{ convergence_time_ms }}).

-record(state, {
    node_id :: binary(),
    peers :: [node()],
    seen_messages :: gb_sets:set(),
    threat_signatures :: map()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(NodeId) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [NodeId], []).

broadcast_threat(ThreatSignature, Priority) ->
    gen_server:cast(?SERVER, {broadcast_threat, ThreatSignature, Priority}).

join_cluster(SeedNodes) ->
    gen_server:call(?SERVER, {join_cluster, SeedNodes}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([NodeId]) ->
    process_flag(trap_exit, true),
    
    % Initialize state
    State = #state{
        node_id = NodeId,
        peers = [],
        seen_messages = gb_sets:new(),
        threat_signatures = #{}
    },
    
    % Start gossip timer
    erlang:send_after(?CONVERGENCE_TIME_MS, self(), gossip_tick),
    
    {ok, State}.

handle_call({join_cluster, SeedNodes}, _From, State) ->
    % Connect to seed nodes
    NewPeers = lists:foldl(fun(Node, Acc) ->
        case net_adm:ping(Node) of
            pong -> [Node | Acc];
            pang -> Acc
        end
    end, State#state.peers, SeedNodes),
    
    {reply, ok, State#state{peers = NewPeers}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({broadcast_threat, ThreatSignature, Priority}, State) ->
    MessageId = crypto:strong_rand_bytes(16),
    Message = #{
        id => MessageId,
        signature => ThreatSignature,
        priority => Priority,
        hops => 0,
        origin => State#state.node_id,
        timestamp => erlang:system_time(millisecond)
    },
    
    % Add to seen messages
    NewSeen = gb_sets:add(MessageId, State#state.seen_messages),
    
    % Gossip to random peers
    gossip_to_peers(Message, State#state.peers),
    
    {noreply, State#state{seen_messages = NewSeen}};

handle_cast({gossip_message, Message}, State) ->
    MessageId = maps:get(id, Message),
    
    case gb_sets:is_member(MessageId, State#state.seen_messages) of
        true ->
            % Already seen this message
            {noreply, State};
        false ->
            % New message
            Hops = maps:get(hops, Message),
            if
                Hops < ?MAX_HOPS ->
                    % Forward to other peers
                    NewMessage = Message#{hops => Hops + 1},
                    gossip_to_peers(NewMessage, State#state.peers),
                    
                    % Process threat signature
                    process_threat_signature(Message, State);
                true ->
                    % Max hops reached
                    {noreply, State}
            end
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(gossip_tick, State) ->
    % Periodic gossip maintenance
    erlang:send_after(?CONVERGENCE_TIME_MS, self(), gossip_tick),
    
    % Clean old messages from seen set
    % TODO: Implement message expiry
    
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

gossip_to_peers(Message, Peers) ->
    % Select random subset of peers
    SelectedPeers = select_random_peers(Peers, ?FAN_OUT),
    
    % Send message to selected peers
    lists:foreach(fun(Peer) ->
        gen_server:cast({?SERVER, Peer}, {gossip_message, Message})
    end, SelectedPeers).

select_random_peers(Peers, Count) ->
    Shuffled = [X || {_, X} <- lists:sort([{rand:uniform(), P} || P <- Peers])],
    lists:sublist(Shuffled, Count).

process_threat_signature(Message, State) ->
    ThreatSignature = maps:get(signature, Message),
    
    % Update local threat database
    NewSignatures = maps:put(
        maps:get(id, Message),
        ThreatSignature,
        State#state.threat_signatures
    ),
    
    % Notify BitActor NIFs
    notify_bitactors(ThreatSignature),
    
    {noreply, State#state{threat_signatures = NewSignatures}}.

notify_bitactors(ThreatSignature) ->
    % TODO: Call BitActor NIF to update threat signatures
    ok.
'''
        
        # Save template
        template_path = self.template_dir / "aegis_gossip_mesh.erl.j2"
        with open(template_path, 'w') as f:
            f.write(template_content)
        
        # Render template
        template = self.env.get_template("aegis_gossip_mesh.erl.j2")
        rendered = template.render(**config)
        
        # Save generated code
        with open(output_path / "aegis_gossip_mesh.erl", 'w') as f:
            f.write(rendered)
    
    def _generate_kubernetes_manifests(self, config: Dict[str, Any], output_path: Path):
        """Generate Kubernetes manifests from configuration"""
        # Deployment manifest template
        deployment_template = '''apiVersion: apps/v1
kind: Deployment
metadata:
  name: aegis-fabric
  namespace: {{ namespace }}
  labels:
    app: aegis-fabric
    version: v1.0.0
spec:
  replicas: {{ replica_count }}
  selector:
    matchLabels:
      app: aegis-fabric
  template:
    metadata:
      labels:
        app: aegis-fabric
      annotations:
        prometheus.io/scrape: "true"
        prometheus.io/port: "{{ network_policy.metrics_port }}"
    spec:
      {% if anti_affinity == "REQUIRED" %}
      affinity:
        podAntiAffinity:
          requiredDuringSchedulingIgnoredDuringExecution:
          - labelSelector:
              matchExpressions:
              - key: app
                operator: In
                values:
                - aegis-fabric
            topologyKey: kubernetes.io/hostname
      {% endif %}
      
      securityContext:
        runAsNonRoot: true
        runAsUser: 1000
        fsGroup: 2000
        
      containers:
      - name: bitactor
        image: aegis-fabric:latest
        ports:
        - containerPort: {{ network_policy.gossip_port }}
          name: gossip
          protocol: TCP
        - containerPort: {{ network_policy.metrics_port }}
          name: metrics
          protocol: TCP
          
        resources:
          requests:
            cpu: {{ resources.cpu_request }}
            memory: {{ resources.memory_request }}
          limits:
            cpu: {{ resources.cpu_limit }}
            memory: {{ resources.memory_limit }}
            
        securityContext:
          allowPrivilegeEscalation: false
          readOnlyRootFilesystem: true
          runAsNonRoot: true
          capabilities:
            drop:
            - ALL
            
        livenessProbe:
          httpGet:
            path: /health
            port: metrics
          initialDelaySeconds: 30
          periodSeconds: 10
          
        readinessProbe:
          httpGet:
            path: /ready
            port: metrics
          initialDelaySeconds: 10
          periodSeconds: 5
          
        env:
        - name: NODE_NAME
          valueFrom:
            fieldRef:
              fieldPath: spec.nodeName
        - name: POD_NAME
          valueFrom:
            fieldRef:
              fieldPath: metadata.name
        - name: POD_NAMESPACE
          valueFrom:
            fieldRef:
              fieldPath: metadata.namespace
              
        volumeMounts:
        - name: tmp
          mountPath: /tmp
        - name: cache
          mountPath: /app/cache
          
      volumes:
      - name: tmp
        emptyDir: {}
      - name: cache
        emptyDir:
          sizeLimit: 1Gi
---
apiVersion: v1
kind: Service
metadata:
  name: aegis-fabric
  namespace: {{ namespace }}
spec:
  selector:
    app: aegis-fabric
  ports:
  - name: gossip
    port: {{ network_policy.gossip_port }}
    targetPort: gossip
  - name: metrics
    port: {{ network_policy.metrics_port }}
    targetPort: metrics
  clusterIP: None  # Headless service for gossip
---
apiVersion: policy/v1
kind: PodDisruptionBudget
metadata:
  name: aegis-fabric-pdb
  namespace: {{ namespace }}
spec:
  minAvailable: {{ pod_disruption_budget }}
  selector:
    matchLabels:
      app: aegis-fabric
---
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: aegis-fabric-netpol
  namespace: {{ namespace }}
spec:
  podSelector:
    matchLabels:
      app: aegis-fabric
  policyTypes:
  - Ingress
  - Egress
  
  {% if network_policy.ingress_policy == "DENY_ALL" %}
  ingress: []
  {% else %}
  ingress:
  - from:
    - podSelector:
        matchLabels:
          app: aegis-fabric
    ports:
    - protocol: TCP
      port: {{ network_policy.gossip_port }}
  - from:
    - namespaceSelector:
        matchLabels:
          name: monitoring
    ports:
    - protocol: TCP
      port: {{ network_policy.metrics_port }}
  {% endif %}
  
  {% if network_policy.egress_policy == "ALLOW_GOSSIP_ONLY" %}
  egress:
  - to:
    - podSelector:
        matchLabels:
          app: aegis-fabric
    ports:
    - protocol: TCP
      port: {{ network_policy.gossip_port }}
  - to:
    - namespaceSelector: {}
    ports:
    - protocol: UDP
      port: 53  # DNS
  {% endif %}
'''
        
        # Save template
        template_path = self.template_dir / "k8s_deployment.yaml.j2"
        with open(template_path, 'w') as f:
            f.write(deployment_template)
        
        # Render template
        template = self.env.get_template("k8s_deployment.yaml.j2")
        rendered = template.render(**config)
        
        # Save generated manifest
        with open(output_path / "aegis_fabric_deployment.yaml", 'w') as f:
            f.write(rendered)
    
    def _generate_terraform_configuration(self, config: Dict[str, Any], output_path: Path):
        """Generate Terraform configuration"""
        terraform_template = '''# Generated Terraform configuration for Aegis Fabric
# Source: TTL Ontology

terraform {
  required_version = ">= 1.0"
  
  required_providers {
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = "~> 2.23"
    }
    helm = {
      source  = "hashicorp/helm"
      version = "~> 2.11"
    }
  }
}

variable "kubeconfig_path" {
  description = "Path to kubeconfig file"
  type        = string
  default     = "~/.kube/config"
}

provider "kubernetes" {
  config_path = var.kubeconfig_path
}

# Create namespace
resource "kubernetes_namespace" "aegis_fabric" {
  metadata {
    name = "{{ namespace }}"
    
    labels = {
      app         = "aegis-fabric"
      environment = "production"
      managed_by  = "terraform"
    }
  }
}

# Deploy Aegis Fabric
resource "kubernetes_manifest" "aegis_deployment" {
  manifest = yamldecode(file("${path.module}/aegis_fabric_deployment.yaml"))
  
  depends_on = [kubernetes_namespace.aegis_fabric]
}

# Create security policies
resource "kubernetes_network_policy" "aegis_security" {
  metadata {
    name      = "aegis-fabric-security"
    namespace = kubernetes_namespace.aegis_fabric.metadata[0].name
  }
  
  spec {
    pod_selector {
      match_labels = {
        app = "aegis-fabric"
      }
    }
    
    {% if network_policy.ingress_policy == "DENY_ALL" %}
    policy_types = ["Ingress", "Egress"]
    {% endif %}
  }
}

# RBAC configuration
resource "kubernetes_cluster_role" "aegis_role" {
  metadata {
    name = "aegis-fabric-role"
  }
  
  rule {
    api_groups = [""]
    resources  = ["pods", "services"]
    verbs      = ["get", "list", "watch"]
  }
}

resource "kubernetes_cluster_role_binding" "aegis_binding" {
  metadata {
    name = "aegis-fabric-binding"
  }
  
  role_ref {
    api_group = "rbac.authorization.k8s.io"
    kind      = "ClusterRole"
    name      = kubernetes_cluster_role.aegis_role.metadata[0].name
  }
  
  subject {
    kind      = "ServiceAccount"
    name      = "default"
    namespace = kubernetes_namespace.aegis_fabric.metadata[0].name
  }
}

# Monitoring setup
resource "kubernetes_service_monitor" "aegis_metrics" {
  metadata {
    name      = "aegis-fabric-metrics"
    namespace = kubernetes_namespace.aegis_fabric.metadata[0].name
    
    labels = {
      app = "aegis-fabric"
    }
  }
  
  spec {
    selector {
      match_labels = {
        app = "aegis-fabric"
      }
    }
    
    endpoints {
      port     = "metrics"
      interval = "30s"
      path     = "/metrics"
    }
  }
}

output "namespace" {
  value = kubernetes_namespace.aegis_fabric.metadata[0].name
}

output "deployment_status" {
  value = "Aegis Fabric deployed successfully"
}
'''
        
        # Save template
        template_path = self.template_dir / "terraform_aegis.tf.j2"
        with open(template_path, 'w') as f:
            f.write(terraform_template)
        
        # Render template
        template = self.env.get_template("terraform_aegis.tf.j2")
        rendered = template.render(**config)
        
        # Save generated Terraform config
        with open(output_path / "aegis_fabric.tf", 'w') as f:
            f.write(rendered)
    
    def _generate_validation_suite(self, config: Dict[str, Any], output_path: Path):
        """Generate validation test suite"""
        validation_template = '''#!/usr/bin/env python3
"""
Generated Aegis Fabric Validation Suite
Source: TTL Ontology
Required Pass Rate: {{ required_pass_rate }}%
"""

import subprocess
import json
import time
from pathlib import Path
from typing import Dict, List, Any

class AegisFabricValidator:
    """Comprehensive validation suite for Aegis Fabric"""
    
    def __init__(self):
        self.validators = {
            'primary': '{{ primary_validator }}',
            'penetration': '{{ penetration_tester }}',
            'service_mesh': '{{ service_mesh_tester }}',
            'chaos': '{{ chaos_engine }}'
        }
        self.required_pass_rate = {{ required_pass_rate }}
        self.results = {}
    
    def run_validation_gauntlet(self) -> bool:
        """Execute complete validation suite"""
        print("🎯 EXECUTING AEGIS FABRIC VALIDATION GAUNTLET")
        print("=" * 60)
        
        start_time = time.time()
        
        # Run each validator
        for name, validator_script in self.validators.items():
            print(f"\\n🔍 Running {name} validation...")
            
            try:
                result = subprocess.run(
                    ['python3', validator_script],
                    capture_output=True,
                    text=True,
                    timeout=3600  # 1 hour timeout
                )
                
                success = result.returncode == 0
                self.results[name] = {
                    'success': success,
                    'output': result.stdout,
                    'error': result.stderr if not success else None
                }
                
                print(f"  {'✅ PASS' if success else '❌ FAIL'}")
                
            except subprocess.TimeoutExpired:
                self.results[name] = {
                    'success': False,
                    'error': 'Timeout exceeded'
                }
                print("  ❌ TIMEOUT")
            except Exception as e:
                self.results[name] = {
                    'success': False,
                    'error': str(e)
                }
                print(f"  ❌ ERROR: {e}")
        
        # Calculate pass rate
        total_tests = len(self.results)
        passed_tests = sum(1 for r in self.results.values() if r['success'])
        pass_rate = (passed_tests / total_tests * 100) if total_tests > 0 else 0
        
        duration = time.time() - start_time
        
        # Generate report
        report = {
            'timestamp': time.time(),
            'duration_seconds': duration,
            'total_tests': total_tests,
            'passed_tests': passed_tests,
            'failed_tests': total_tests - passed_tests,
            'pass_rate': pass_rate,
            'required_pass_rate': self.required_pass_rate,
            'validation_passed': pass_rate >= self.required_pass_rate,
            'detailed_results': self.results
        }
        
        # Save report
        with open('aegis_fabric_validation_report.json', 'w') as f:
            json.dump(report, f, indent=2)
        
        print(f"\\n📊 VALIDATION SUMMARY")
        print(f"Total Tests: {total_tests}")
        print(f"Passed: {passed_tests}")
        print(f"Failed: {total_tests - passed_tests}")
        print(f"Pass Rate: {pass_rate:.1f}%")
        print(f"Required: {self.required_pass_rate}%")
        print(f"\\nValidation {'PASSED ✅' if report['validation_passed'] else 'FAILED ❌'}")
        
        return report['validation_passed']

if __name__ == "__main__":
    validator = AegisFabricValidator()
    success = validator.run_validation_gauntlet()
    exit(0 if success else 1)
'''
        
        # Save template
        template_path = self.template_dir / "validation_suite.py.j2"
        with open(template_path, 'w') as f:
            f.write(validation_template)
        
        # Render template
        template = self.env.get_template("validation_suite.py.j2")
        rendered = template.render(**config)
        
        # Save generated validation suite
        with open(output_path / "aegis_fabric_validator.py", 'w') as f:
            f.write(rendered)
        
        # Make it executable
        os.chmod(output_path / "aegis_fabric_validator.py", 0o755)
    
    def _generate_dockerfile(self, config: Dict[str, Any], output_path: Path):
        """Generate Dockerfile for Aegis Fabric"""
        dockerfile_template = '''# Generated Dockerfile for Aegis Fabric
# Source: TTL Ontology

FROM alpine:3.18 AS builder

# Install build dependencies
RUN apk add --no-cache \
    gcc \
    musl-dev \
    make \
    cmake \
    erlang \
    erlang-dev \
    elixir

# Copy source files
WORKDIR /build
COPY bitactor_generated.c .
COPY aegis_gossip_mesh.erl .

# Build BitActor
RUN gcc -O3 -march=native -mtune=native \
    {% if security.memory_protection == "NX_DEP" %}-Wl,-z,relro -Wl,-z,now{% endif %} \
    {% if security.stack_protection == "CANARY" %}-fstack-protector-strong{% endif %} \
    {% if security.aslr_enabled %}-fPIE -pie{% endif %} \
    -o bitactor bitactor_generated.c -lpthread

# Compile Erlang modules
RUN erlc +native +"{hipe, [o3]}" aegis_gossip_mesh.erl

# Runtime image
FROM alpine:3.18

# Install runtime dependencies
RUN apk add --no-cache \
    libstdc++ \
    erlang \
    {% if security.memory_protection == "NX_DEP" %}libcap{% endif %}

# Create non-root user
RUN addgroup -g 1000 aegis && \
    adduser -D -u 1000 -G aegis aegis

# Copy binaries
COPY --from=builder /build/bitactor /app/bitactor
COPY --from=builder /build/*.beam /app/

# Set permissions
RUN chown -R aegis:aegis /app && \
    chmod 755 /app/bitactor

{% if security.memory_protection == "NX_DEP" %}
# Apply security capabilities
RUN setcap 'cap_sys_nice+ep' /app/bitactor
{% endif %}

# Switch to non-root user
USER aegis

WORKDIR /app

# Expose ports
EXPOSE {{ deployment.network_policy.gossip_port }} {{ deployment.network_policy.metrics_port }}

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=10s --retries=3 \
    CMD ["/app/bitactor", "--health-check"]

# Entry point
ENTRYPOINT ["/app/bitactor"]
CMD ["--node-name", "${POD_NAME}", "--cluster", "aegis-fabric"]
'''
        
        # Save template
        template_path = self.template_dir / "Dockerfile.j2"
        with open(template_path, 'w') as f:
            f.write(dockerfile_template)
        
        # Render template with full config
        template = self.env.get_template("Dockerfile.j2")
        full_config = {'security': config.get('security', {}), 'deployment': {'network_policy': {}}}
        
        # Get deployment config from parent
        deployment_config = self.extract_deployment_config()
        full_config['deployment'] = deployment_config
        full_config['security'] = config.get('security', {})
        
        rendered = template.render(**full_config)
        
        # Save generated Dockerfile
        with open(output_path / "Dockerfile.aegis", 'w') as f:
            f.write(rendered)

def main():
    """Generate all Aegis Fabric components from TTL"""
    
    # Initialize template engine
    engine = AegisTTLTemplateEngine("aegis_fabric_ontology.ttl")
    
    # Generate all components
    config = engine.generate_all_components()
    
    print("✅ Aegis Fabric generation complete!")
    print(f"📁 Generated files in: generated/")
    print(f"📋 Configuration saved to: generated/aegis_config.json")
    
    return 0

if __name__ == "__main__":
    import sys
    sys.exit(main())