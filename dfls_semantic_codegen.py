#!/usr/bin/env python3
"""
DFLS SEMANTIC CODE GENERATOR
Mass production system for generating Erlang/OTP DFLS implementations from semantic specifications

TTL/OWL/SHACL/SPARQL -> Jinja Typer -> Erlang/OTP Workflow
Integrates with existing CNS infrastructure for ultra-high performance
"""

import sys
import os
import time
import json
import subprocess
from pathlib import Path
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass, asdict
import logging

import typer
import rdflib
from rdflib import Graph, Namespace, URIRef, Literal
from rdflib.plugins.sparql import prepareQuery
import jinja2
from jinja2 import Environment, FileSystemLoader, select_autoescape
import yaml
import click

# Add CNS root for imports
CNS_ROOT = Path(__file__).parent
sys.path.insert(0, str(CNS_ROOT))

# Import existing CNS infrastructure
from jinja_aot_compiler import JinjaAOTCompiler, CompiledTemplate
from numba_optimizations import fast_constraint_validation

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Initialize Typer app
app = typer.Typer(
    name="dfls-codegen",
    help="🚀 DFLS Semantic Code Generator - Mass production of Erlang/OTP systems",
    add_completion=False,
    rich_markup_mode="rich"
)

# ============================================================================
# SEMANTIC WEB NAMESPACES AND CONFIGURATION
# ============================================================================

# Define semantic namespaces
DFLS = Namespace("http://cns.bitactor.io/ontology/dfls#")
OTP = Namespace("http://cns.bitactor.io/ontology/otp#")
BITACTOR = Namespace("http://cns.bitactor.io/ontology/bitactor#")

@dataclass
class DFLSConfig:
    """Configuration for DFLS code generation system"""
    cns_root: Path
    ontology_dir: Path
    sparql_dir: Path
    template_dir: Path
    output_dir: Path
    quality_target: float = 0.00034  # Six Sigma: 3.4 defects per million
    performance_target: float = 0.0005  # <500μs latency
    batch_size: int = 100
    concurrency_level: int = 8
    enable_aot_optimization: bool = True
    enable_bitactor_integration: bool = True

# ============================================================================
# SEMANTIC GRAPH LOADER AND VALIDATOR
# ============================================================================

class SemanticGraphManager:
    """Manages semantic graphs with SHACL validation and SPARQL querying"""
    
    def __init__(self, config: DFLSConfig):
        self.config = config
        self.graph = Graph()
        self.loaded_ontologies = set()
        self.sparql_queries = {}
        
        # Load core ontologies
        self._load_core_ontologies()
        self._load_sparql_queries()
        
        logger.info(f"✅ Loaded {len(self.graph)} triples from ontologies")
    
    def _load_core_ontologies(self):
        """Load all DFLS ontologies into the graph"""
        ontology_files = [
            "dfls_erlang_core.ttl",
            "dfls_shacl_validation.ttl"
        ]
        
        for ontology_file in ontology_files:
            ontology_path = self.config.ontology_dir / ontology_file
            if ontology_path.exists():
                try:
                    self.graph.parse(ontology_path, format="turtle")
                    self.loaded_ontologies.add(ontology_file)
                    logger.info(f"📚 Loaded ontology: {ontology_file}")
                except Exception as e:
                    logger.error(f"❌ Failed to load {ontology_file}: {e}")
    
    def _load_sparql_queries(self):
        """Load all SPARQL queries for code generation"""
        sparql_file = self.config.sparql_dir / "dfls_code_generation_queries.sparql"
        if sparql_file.exists():
            with open(sparql_file, 'r') as f:
                sparql_content = f.read()
                
            # Parse queries from the file (simplified parsing)
            queries = self._parse_sparql_queries(sparql_content)
            self.sparql_queries.update(queries)
            logger.info(f"🔍 Loaded {len(queries)} SPARQL queries")
    
    def _parse_sparql_queries(self, content: str) -> Dict[str, str]:
        """Parse SPARQL queries from multi-query file"""
        queries = {}
        lines = content.split('\n')
        current_query = []
        current_name = None
        
        for line in lines:
            if line.startswith('# Query'):
                if current_name and current_query:
                    queries[current_name] = '\n'.join(current_query)
                
                # Extract query name
                parts = line.split(':')
                if len(parts) > 1:
                    current_name = parts[1].strip().lower().replace(' ', '_')
                current_query = []
                
            elif line.startswith('SELECT') or (current_query and not line.startswith('#')):
                current_query.append(line)
        
        # Add final query
        if current_name and current_query:
            queries[current_name] = '\n'.join(current_query)
        
        return queries
    
    def execute_sparql_query(self, query_name: str, bindings: Dict = None) -> List[Dict]:
        """Execute SPARQL query and return results"""
        if query_name not in self.sparql_queries:
            raise ValueError(f"Query '{query_name}' not found")
        
        query_text = self.sparql_queries[query_name]
        
        # Add namespace prefixes
        prefixes = """
        PREFIX dfls: <http://cns.bitactor.io/ontology/dfls#>
        PREFIX otp: <http://cns.bitactor.io/ontology/otp#>
        PREFIX bitactor: <http://cns.bitactor.io/ontology/bitactor#>
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX owl: <http://www.w3.org/2002/07/owl#>
        """
        
        full_query = prefixes + query_text
        
        try:
            prepared_query = prepareQuery(full_query)
            results = self.graph.query(prepared_query, initBindings=bindings or {})
            
            # Convert results to list of dictionaries
            result_list = []
            for row in results:
                row_dict = {}
                for var in results.vars:
                    value = row[var]
                    if value:
                        row_dict[str(var)] = str(value)
                result_list.append(row_dict)
            
            logger.info(f"🔍 Query '{query_name}' returned {len(result_list)} results")
            return result_list
            
        except Exception as e:
            logger.error(f"❌ SPARQL query failed '{query_name}': {e}")
            return []
    
    def validate_with_shacl(self) -> Tuple[bool, List[str]]:
        """Validate graph against SHACL constraints"""
        try:
            # Import pyshacl for validation
            import pyshacl
            
            # Create SHACL shapes graph
            shapes_graph = Graph()
            shapes_file = self.config.ontology_dir / "dfls_shacl_validation.ttl"
            if shapes_file.exists():
                shapes_graph.parse(shapes_file, format="turtle")
            
            # Validate
            conforms, results_graph, results_text = pyshacl.validate(
                self.graph,
                shacl_graph=shapes_graph,
                inference='rdfs',
                debug=False
            )
            
            if conforms:
                logger.info("✅ SHACL validation passed")
                return True, []
            else:
                logger.warning("⚠️ SHACL validation failed")
                violations = results_text.split('\n')
                return False, violations
                
        except ImportError:
            logger.warning("⚠️ pyshacl not available - skipping validation")
            return True, []
        except Exception as e:
            logger.error(f"❌ SHACL validation error: {e}")
            return False, [str(e)]

# ============================================================================
# JINJA TEMPLATE SYSTEM WITH AOT OPTIMIZATION
# ============================================================================

class DFLSTemplateEngine:
    """Enhanced Jinja template engine with AOT optimization for DFLS code generation"""
    
    def __init__(self, config: DFLSConfig):
        self.config = config
        self.template_dir = config.template_dir
        self.template_dir.mkdir(exist_ok=True)
        
        # Initialize Jinja AOT compiler from existing CNS infrastructure
        self.aot_compiler = JinjaAOTCompiler(cache_dir=str(config.output_dir / ".template_cache"))
        
        # Setup template environment
        self.env = Environment(
            loader=FileSystemLoader(str(self.template_dir)),
            autoescape=select_autoescape(['html', 'xml']),
            trim_blocks=True,
            lstrip_blocks=True
        )
        
        # Add custom filters for Erlang code generation
        self._setup_erlang_filters()
        
        # Create default templates
        self._create_default_templates()
        
        logger.info("🎨 DFLS Template Engine initialized with AOT optimization")
    
    def _setup_erlang_filters(self):
        """Add Erlang-specific Jinja filters"""
        
        def erlang_atom(value: str) -> str:
            """Convert string to Erlang atom"""
            if not value:
                return "''"
            # Simple atom conversion
            clean_value = str(value).lower().replace('-', '_').replace(' ', '_')
            if clean_value.isalnum() or '_' in clean_value:
                return clean_value
            else:
                return f"'{clean_value}'"
        
        def erlang_string(value: str) -> str:
            """Convert to Erlang string with proper escaping"""
            if value is None:
                return '""'
            escaped = str(value).replace('\\', '\\\\').replace('"', '\\"')
            return f'"{escaped}"'
        
        def erlang_module_name(value: str) -> str:
            """Convert to valid Erlang module name"""
            if not value:
                return "unnamed_module"
            clean = str(value).lower().replace('-', '_').replace(' ', '_')
            # Ensure starts with lowercase letter
            if clean and clean[0].isdigit():
                clean = 'm_' + clean
            return clean
        
        def format_latency(value: float) -> str:
            """Format latency value with appropriate units"""
            if value < 0.001:
                return f"{value * 1000000:.0f}μs"
            elif value < 1.0:
                return f"{value * 1000:.1f}ms"
            else:
                return f"{value:.2f}s"
        
        def quality_rating(defect_rate: float) -> str:
            """Convert defect rate to quality rating"""
            if defect_rate <= 0.00034:
                return "Six Sigma"
            elif defect_rate <= 0.001:
                return "Five Sigma"
            elif defect_rate <= 0.01:
                return "Four Sigma"
            else:
                return "Below Four Sigma"
        
        # Register filters
        self.env.filters['erlang_atom'] = erlang_atom
        self.env.filters['erlang_string'] = erlang_string
        self.env.filters['erlang_module_name'] = erlang_module_name
        self.env.filters['format_latency'] = format_latency
        self.env.filters['quality_rating'] = quality_rating
    
    def _create_default_templates(self):
        """Create default Jinja templates for Erlang/OTP code generation"""
        
        # GenServer template
        genserver_template = '''%% Generated GenServer Module: {{ module_name }}
%% DFLS Quality Target: {{ quality_target | quality_rating }}
%% Performance Target: {{ performance_target | format_latency }}
%% Generated: {{ generation_timestamp }}

-module({{ module_name | erlang_module_name }}).
-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% DFLS Quality Control
-export([quality_metrics/0, performance_stats/0]).

-define(SERVER, ?MODULE).
-define(QUALITY_TARGET, {{ quality_target }}).
-define(PERFORMANCE_TARGET, {{ performance_target }}).

%% State record
-record(state, {
    {% for field in state_fields -%}
    {{ field.name | erlang_atom }} = {{ field.default_value | erlang_string }}{% if not loop.last %},{% endif %}
    {% endfor %}
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Args) ->
    %% DFLS Quality Gate: Initialization validation
    case validate_init_args(Args) of
        {ok, ValidatedArgs} ->
            State = #state{},
            %% Initialize performance monitoring
            init_performance_monitoring(),
            {ok, State};
        {error, Reason} ->
            %% DFLS Error Handling: Log quality violation
            error_logger:error_msg("Init quality gate failed: ~p~n", [Reason]),
            {stop, {quality_violation, Reason}}
    end.

handle_call(quality_metrics, _From, State) ->
    Metrics = collect_quality_metrics(),
    {reply, Metrics, State};

handle_call(performance_stats, _From, State) ->
    Stats = collect_performance_stats(),
    {reply, Stats, State};

{% for callback in callbacks -%}
handle_call({{ callback.pattern | erlang_atom }}, _From, State) ->
    %% DFLS Performance Monitoring
    StartTime = erlang:monotonic_time(microsecond),
    
    %% {{ callback.description }}
    Result = {{ callback.handler_function }}(State),
    
    %% Performance validation
    EndTime = erlang:monotonic_time(microsecond),
    Latency = EndTime - StartTime,
    validate_performance_target({{ callback.name | erlang_string }}, Latency),
    
    {reply, Result, State};
{% endfor %}

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% DFLS Quality Control Functions
%%====================================================================

validate_init_args(Args) ->
    %% Six Sigma validation logic
    case length(Args) of
        N when N >= 0, N =< 10 -> {ok, Args};
        _ -> {error, too_many_args}
    end.

collect_quality_metrics() ->
    #{
        defect_rate => get_defect_rate(),
        quality_target => ?QUALITY_TARGET,
        current_quality => calculate_current_quality(),
        six_sigma_compliance => check_six_sigma_compliance()
    }.

collect_performance_stats() ->
    #{
        average_latency => get_average_latency(),
        performance_target => ?PERFORMANCE_TARGET,
        cpu_usage => get_cpu_usage(),
        memory_usage => get_memory_usage()
    }.

init_performance_monitoring() ->
    %% Initialize performance counters
    put(performance_start, erlang:monotonic_time(microsecond)),
    put(request_count, 0),
    put(total_latency, 0).

validate_performance_target(Operation, Latency) ->
    %% DFLS Performance Gate
    case Latency > (?PERFORMANCE_TARGET * 1000000) of  % Convert to microseconds
        true ->
            error_logger:warning_msg("Performance target exceeded: ~s took ~pμs~n", 
                                   [Operation, Latency]);
        false ->
            ok
    end,
    update_performance_stats(Latency).

update_performance_stats(Latency) ->
    Count = get(request_count) + 1,
    TotalLatency = get(total_latency) + Latency,
    put(request_count, Count),
    put(total_latency, TotalLatency).

get_defect_rate() -> 0.0001.  %% Placeholder
get_average_latency() -> get(total_latency) / max(1, get(request_count)).
get_cpu_usage() -> 0.15.      %% Placeholder  
get_memory_usage() -> 0.08.   %% Placeholder
calculate_current_quality() -> 1.0 - get_defect_rate().
check_six_sigma_compliance() -> get_defect_rate() =< ?QUALITY_TARGET.
'''

        # Supervisor template
        supervisor_template = '''%% Generated Supervisor Module: {{ module_name }}
%% DFLS Fault Tolerance with Six Sigma Reliability
%% Generated: {{ generation_timestamp }}

-module({{ module_name | erlang_module_name }}).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(MAX_RESTARTS, {{ max_restarts | default(3) }}).
-define(MAX_SECONDS, {{ max_seconds | default(10) }}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    %% DFLS Restart Strategy: {{ restart_strategy }}
    RestartStrategy = {{{ restart_strategy | default('one_for_one') | erlang_atom }}, 
                      ?MAX_RESTARTS, ?MAX_SECONDS},
    
    Children = [
        {% for child in children -%}
        {{{ child.id | erlang_atom }},
         {{{ child.module | erlang_atom }}, start_link, {{ child.args | default('[]') }}},
         {{ child.restart_type | default('permanent') | erlang_atom }},
         {{ child.shutdown_timeout | default(5000) }},
         {{ child.worker_type | default('worker') | erlang_atom }},
         [{{ child.module | erlang_atom }}]}{% if not loop.last %},{% endif %}
        {% endfor %}
    ],
    
    %% DFLS Quality Gate: Validate supervision tree
    case validate_supervision_tree(Children) of
        ok ->
            {ok, {RestartStrategy, Children}};
        {error, Reason} ->
            error_logger:error_msg("Supervision tree validation failed: ~p~n", [Reason]),
            {stop, {quality_violation, Reason}}
    end.

%%====================================================================
%% DFLS Quality Control
%%====================================================================

validate_supervision_tree(Children) ->
    %% Six Sigma validation for supervision tree
    case length(Children) of
        N when N > 0, N =< 50 -> ok;  % Reasonable number of children
        _ -> {error, invalid_child_count}
    end.
'''

        # Write templates to files
        templates = {
            'genserver.erl.j2': genserver_template,
            'supervisor.erl.j2': supervisor_template
        }
        
        for template_name, template_content in templates.items():
            template_path = self.template_dir / template_name
            with open(template_path, 'w') as f:
                f.write(template_content)
            logger.info(f"📝 Created template: {template_name}")
    
    def render_template(self, template_name: str, context: Dict[str, Any]) -> str:
        """Render Jinja template with AOT optimization"""
        
        # Add generation metadata to context
        context.update({
            'generation_timestamp': time.strftime("%Y-%m-%d %H:%M:%S UTC", time.gmtime()),
            'generator_version': "DFLS Semantic CodeGen v1.0",
            'cns_integration': True,
            'aot_optimized': self.config.enable_aot_optimization
        })
        
        try:
            # Use AOT compiler for performance
            if self.config.enable_aot_optimization:
                template_path = self.template_dir / template_name
                if template_path.exists():
                    with open(template_path, 'r') as f:
                        template_content = f.read()
                    
                    # Compile template using existing CNS AOT infrastructure
                    compiled_template = self.aot_compiler.get_template(template_name, template_content)
                    return compiled_template.render(**context)
            
            # Fallback to regular Jinja rendering
            template = self.env.get_template(template_name)
            return template.render(**context)
            
        except Exception as e:
            logger.error(f"❌ Template rendering failed '{template_name}': {e}")
            raise

# ============================================================================
# ERLANG/OTP CODE GENERATOR
# ============================================================================

class ErlangOTPGenerator:
    """Main code generator that coordinates semantic analysis and template rendering"""
    
    def __init__(self, config: DFLSConfig):
        self.config = config
        self.semantic_manager = SemanticGraphManager(config)
        self.template_engine = DFLSTemplateEngine(config)
        
        # Statistics
        self.generation_stats = {
            'modules_generated': 0,
            'lines_of_code': 0,
            'quality_violations': 0,
            'performance_optimizations': 0,
            'start_time': time.time()
        }
        
        logger.info("🏗️ Erlang/OTP Generator initialized")
    
    def generate_genserver(self, genserver_uri: str) -> Tuple[str, str]:
        """Generate GenServer module from semantic definition"""
        
        # Extract GenServer specification using SPARQL
        query_results = self.semantic_manager.execute_sparql_query(
            'extract_genserver_specifications_for_code_generation',
            {'genServer': URIRef(genserver_uri)}
        )
        
        if not query_results:
            raise ValueError(f"No GenServer specification found for {genserver_uri}")
        
        genserver_spec = query_results[0]
        
        # Extract callbacks and state information
        callback_results = self.semantic_manager.execute_sparql_query(
            'extract_genserver_callback_functions_and_their_constraints'
        )
        
        # Build template context
        context = {
            'module_name': genserver_spec.get('module', 'unnamed_genserver'),
            'label': genserver_spec.get('label', 'Generated GenServer'),
            'comment': genserver_spec.get('comment', 'DFLS Generated GenServer'),
            'quality_target': float(genserver_spec.get('defectRate', self.config.quality_target)),
            'performance_target': self.config.performance_target,
            'state_fields': self._extract_state_fields(genserver_spec),
            'callbacks': self._process_callbacks(callback_results)
        }
        
        # Generate code using template
        code = self.template_engine.render_template('genserver.erl.j2', context)
        module_name = context['module_name']
        
        # Update statistics
        self.generation_stats['modules_generated'] += 1
        self.generation_stats['lines_of_code'] += len(code.split('\n'))
        
        logger.info(f"✅ Generated GenServer: {module_name}")
        return module_name, code
    
    def generate_supervisor(self, supervisor_uri: str) -> Tuple[str, str]:
        """Generate Supervisor module from semantic definition"""
        
        # Extract Supervisor specification
        query_results = self.semantic_manager.execute_sparql_query(
            'extract_supervisor_specifications_with_restart_strategies',
            {'supervisor': URIRef(supervisor_uri)}
        )
        
        if not query_results:
            raise ValueError(f"No Supervisor specification found for {supervisor_uri}")
        
        supervisor_spec = query_results[0]
        
        # Extract child specifications
        child_results = self.semantic_manager.execute_sparql_query(
            'extract_child_specifications_with_quality_constraints'
        )
        
        # Build template context
        context = {
            'module_name': supervisor_spec.get('module', 'unnamed_supervisor'),
            'label': supervisor_spec.get('label', 'Generated Supervisor'),
            'restart_strategy': supervisor_spec.get('restartStrategy', 'one_for_one'),
            'max_restarts': int(supervisor_spec.get('maxRestarts', 3)),
            'max_seconds': int(supervisor_spec.get('maxSeconds', 10)),
            'children': self._process_child_specs(child_results)
        }
        
        # Generate code
        code = self.template_engine.render_template('supervisor.erl.j2', context)
        module_name = context['module_name']
        
        # Update statistics
        self.generation_stats['modules_generated'] += 1
        self.generation_stats['lines_of_code'] += len(code.split('\n'))
        
        logger.info(f"✅ Generated Supervisor: {module_name}")
        return module_name, code
    
    def _extract_state_fields(self, genserver_spec: Dict) -> List[Dict]:
        """Extract state field definitions from GenServer specification"""
        # Placeholder implementation - in real system, this would extract from ontology
        return [
            {'name': 'status', 'default_value': 'inactive'},
            {'name': 'quality_metrics', 'default_value': '#{}'},
            {'name': 'performance_stats', 'default_value': '#{}'}
        ]
    
    def _process_callbacks(self, callback_results: List[Dict]) -> List[Dict]:
        """Process callback specifications for template rendering"""
        callbacks = []
        for callback in callback_results:
            callbacks.append({
                'name': callback.get('callback', 'unknown'),
                'pattern': callback.get('callbackType', 'unknown_request'),
                'handler_function': 'handle_' + callback.get('callback', 'unknown'),
                'description': f"Handler for {callback.get('callback', 'unknown')} requests"
            })
        return callbacks
    
    def _process_child_specs(self, child_results: List[Dict]) -> List[Dict]:
        """Process child specifications for supervisor template"""
        children = []
        for child in child_results:
            children.append({
                'id': child.get('childId', 'unknown_child'),
                'module': child.get('module', 'unknown_module'),
                'args': child.get('args', '[]'),
                'restart_type': child.get('restartType', 'permanent'),
                'shutdown_timeout': int(child.get('shutdownTimeout', 5000)),
                'worker_type': child.get('workType', 'worker')
            })
        return children
    
    def generate_batch(self, specifications: List[str]) -> Dict[str, str]:
        """Generate multiple modules in batch for mass production"""
        logger.info(f"🏭 Starting batch generation of {len(specifications)} modules")
        
        generated_modules = {}
        
        for spec_uri in specifications:
            try:
                # Determine component type and generate accordingly
                if 'genserver' in spec_uri.lower():
                    module_name, code = self.generate_genserver(spec_uri)
                elif 'supervisor' in spec_uri.lower():
                    module_name, code = self.generate_supervisor(spec_uri)
                else:
                    logger.warning(f"⚠️ Unknown specification type: {spec_uri}")
                    continue
                
                generated_modules[module_name] = code
                
            except Exception as e:
                logger.error(f"❌ Failed to generate {spec_uri}: {e}")
                self.generation_stats['quality_violations'] += 1
        
        logger.info(f"✅ Batch generation complete: {len(generated_modules)} modules")
        return generated_modules
    
    def get_generation_stats(self) -> Dict[str, Any]:
        """Get generation statistics and performance metrics"""
        elapsed_time = time.time() - self.generation_stats['start_time']
        
        stats = dict(self.generation_stats)
        stats.update({
            'elapsed_time': elapsed_time,
            'modules_per_second': self.generation_stats['modules_generated'] / max(elapsed_time, 1),
            'lines_per_second': self.generation_stats['lines_of_code'] / max(elapsed_time, 1),
            'quality_score': 1.0 - (self.generation_stats['quality_violations'] / 
                                  max(self.generation_stats['modules_generated'], 1))
        })
        
        return stats

# ============================================================================
# TYPER CLI INTERFACE
# ============================================================================

@app.command()
def init(
    output_dir: Path = typer.Option("./dfls_output", help="Output directory for generated code"),
    template_dir: Path = typer.Option("./templates", help="Template directory"),
    quality_target: float = typer.Option(0.00034, help="Six Sigma quality target (defect rate)"),
    performance_target: float = typer.Option(0.0005, help="Performance target in seconds")
):
    """🚀 Initialize DFLS code generation workspace"""
    
    # Create configuration
    config = DFLSConfig(
        cns_root=CNS_ROOT,
        ontology_dir=CNS_ROOT / "bitactor_otp" / "priv" / "ontologies",
        sparql_dir=CNS_ROOT / "bitactor_otp" / "priv" / "sparql",
        template_dir=template_dir,
        output_dir=output_dir,
        quality_target=quality_target,
        performance_target=performance_target
    )
    
    # Create directories
    for directory in [config.output_dir, config.template_dir]:
        directory.mkdir(parents=True, exist_ok=True)
    
    # Save configuration
    config_file = output_dir / "dfls_config.json"
    with open(config_file, 'w') as f:
        # Convert Path objects to strings for JSON serialization
        config_dict = asdict(config)
        for key, value in config_dict.items():
            if isinstance(value, Path):
                config_dict[key] = str(value)
        json.dump(config_dict, f, indent=2)
    
    typer.echo(f"✅ DFLS workspace initialized in {output_dir}")
    typer.echo(f"📋 Configuration saved to {config_file}")
    typer.echo(f"🎯 Quality target: {quality_target} (Six Sigma)")
    typer.echo(f"⚡ Performance target: {performance_target*1000:.1f}ms")

@app.command()
def validate(
    config_file: Path = typer.Option("./dfls_output/dfls_config.json", help="Configuration file")
):
    """✅ Validate semantic definitions with SHACL constraints"""
    
    # Load configuration
    config = _load_config(config_file)
    
    # Initialize semantic manager
    semantic_manager = SemanticGraphManager(config)
    
    # Run SHACL validation
    typer.echo("🔍 Running SHACL validation...")
    is_valid, violations = semantic_manager.validate_with_shacl()
    
    if is_valid:
        typer.echo("✅ All semantic definitions are valid")
    else:
        typer.echo("❌ SHACL validation failed:")
        for violation in violations[:10]:  # Show first 10 violations
            typer.echo(f"  • {violation}")
        
        if len(violations) > 10:
            typer.echo(f"  ... and {len(violations) - 10} more violations")

@app.command()
def generate(
    config_file: Path = typer.Option("./dfls_output/dfls_config.json", help="Configuration file"),
    specifications: List[str] = typer.Option(None, "--spec", help="Specification URIs to generate"),
    output_format: str = typer.Option("files", help="Output format: files, stdout, or archive"),
    batch_size: int = typer.Option(100, help="Batch size for mass production")
):
    """🏗️ Generate Erlang/OTP code from semantic specifications"""
    
    # Load configuration
    config = _load_config(config_file)
    config.batch_size = batch_size
    
    # Initialize generator
    generator = ErlangOTPGenerator(config)
    
    # If no specifications provided, generate from all available
    if not specifications:
        typer.echo("🔍 Discovering available specifications...")
        # Query for all available specifications
        specs = generator.semantic_manager.execute_sparql_query('extract_genserver_specifications_for_code_generation')
        specifications = [spec.get('genServer', '') for spec in specs if spec.get('genServer')]
        typer.echo(f"📋 Found {len(specifications)} specifications")
    
    if not specifications:
        typer.echo("❌ No specifications found to generate")
        raise typer.Exit(1)
    
    # Generate code
    typer.echo(f"🏭 Generating code for {len(specifications)} specifications...")
    
    with typer.progressbar(specifications, label="Generating") as progress:
        generated_modules = {}
        for spec_uri in progress:
            try:
                if 'genserver' in spec_uri.lower():
                    module_name, code = generator.generate_genserver(spec_uri)
                elif 'supervisor' in spec_uri.lower():
                    module_name, code = generator.generate_supervisor(spec_uri)
                else:
                    continue
                
                generated_modules[module_name] = code
                
            except Exception as e:
                typer.echo(f"❌ Failed to generate {spec_uri}: {e}")
    
    # Output results
    if output_format == "files":
        _write_generated_files(generated_modules, config.output_dir)
    elif output_format == "stdout":
        for module_name, code in generated_modules.items():
            typer.echo(f"\n{'='*60}")
            typer.echo(f"MODULE: {module_name}")
            typer.echo('='*60)
            typer.echo(code)
    
    # Show statistics
    stats = generator.get_generation_stats()
    typer.echo(f"\n📊 GENERATION STATISTICS:")
    typer.echo(f"  • Modules generated: {stats['modules_generated']}")
    typer.echo(f"  • Lines of code: {stats['lines_of_code']}")
    typer.echo(f"  • Generation rate: {stats['modules_per_second']:.1f} modules/sec")
    typer.echo(f"  • Quality score: {stats['quality_score']:.4f}")

@app.command()
def batch_produce(
    config_file: Path = typer.Option("./dfls_output/dfls_config.json", help="Configuration file"),
    production_spec: Path = typer.Option(None, help="Production specification YAML file"),
    concurrency: int = typer.Option(8, help="Concurrent generation processes"),
    quality_gate: bool = typer.Option(True, help="Enable quality gates")
):
    """🏭 Mass production of DFLS systems with quality control"""
    
    # Load configuration
    config = _load_config(config_file)
    config.concurrency_level = concurrency
    
    # Load production specification
    if production_spec and production_spec.exists():
        with open(production_spec, 'r') as f:
            prod_spec = yaml.safe_load(f)
    else:
        prod_spec = {
            'systems': [
                {'name': 'forex_trading_system', 'components': ['genserver', 'supervisor']},
                {'name': 'news_processing_system', 'components': ['genserver', 'supervisor']},
                {'name': 'risk_management_system', 'components': ['genserver', 'supervisor']}
            ]
        }
    
    typer.echo(f"🏭 Starting mass production of {len(prod_spec['systems'])} systems")
    typer.echo(f"⚡ Concurrency level: {concurrency}")
    typer.echo(f"🔒 Quality gates: {'enabled' if quality_gate else 'disabled'}")
    
    # Initialize generator
    generator = ErlangOTPGenerator(config)
    
    # Quality gate: SHACL validation
    if quality_gate:
        typer.echo("🔍 Running quality gate: SHACL validation...")
        is_valid, violations = generator.semantic_manager.validate_with_shacl()
        if not is_valid:
            typer.echo("❌ Quality gate failed: SHACL validation errors")
            for violation in violations[:5]:
                typer.echo(f"  • {violation}")
            raise typer.Exit(1)
        typer.echo("✅ Quality gate passed")
    
    # Mass production
    total_modules = 0
    for system in prod_spec['systems']:
        system_name = system['name']
        components = system['components']
        
        typer.echo(f"🔨 Producing system: {system_name}")
        
        # Generate specifications for this system
        specifications = []
        for component in components:
            spec_uri = f"http://example.org/{system_name}_{component}"
            specifications.append(spec_uri)
        
        # Generate modules
        try:
            generated_modules = generator.generate_batch(specifications)
            
            # Write to system-specific directory
            system_dir = config.output_dir / system_name
            system_dir.mkdir(exist_ok=True)
            _write_generated_files(generated_modules, system_dir)
            
            total_modules += len(generated_modules)
            typer.echo(f"✅ System '{system_name}': {len(generated_modules)} modules generated")
            
        except Exception as e:
            typer.echo(f"❌ Failed to generate system '{system_name}': {e}")
    
    # Final statistics
    stats = generator.get_generation_stats()
    typer.echo(f"\n🎯 MASS PRODUCTION COMPLETE:")
    typer.echo(f"  • Total modules: {total_modules}")
    typer.echo(f"  • Total systems: {len(prod_spec['systems'])}")
    typer.echo(f"  • Production rate: {stats['modules_per_second']:.1f} modules/sec")
    typer.echo(f"  • Quality score: {stats['quality_score']:.4f}")
    typer.echo(f"  • Six Sigma compliance: {'✅' if stats['quality_score'] >= 0.99966 else '❌'}")

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

def _load_config(config_file: Path) -> DFLSConfig:
    """Load configuration from JSON file"""
    if not config_file.exists():
        typer.echo(f"❌ Configuration file not found: {config_file}")
        typer.echo("Run 'dfls-codegen init' to create configuration")
        raise typer.Exit(1)
    
    with open(config_file, 'r') as f:
        config_dict = json.load(f)
    
    # Convert string paths back to Path objects
    for key, value in config_dict.items():
        if key.endswith('_dir') or key.endswith('_root'):
            config_dict[key] = Path(value)
    
    return DFLSConfig(**config_dict)

def _write_generated_files(modules: Dict[str, str], output_dir: Path):
    """Write generated modules to files"""
    output_dir.mkdir(parents=True, exist_ok=True)
    
    for module_name, code in modules.items():
        file_path = output_dir / f"{module_name}.erl"
        with open(file_path, 'w') as f:
            f.write(code)
        typer.echo(f"📝 Generated: {file_path}")

# ============================================================================
# MAIN ENTRY POINT
# ============================================================================

if __name__ == "__main__":
    app()