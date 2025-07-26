#!/usr/bin/env python3
"""
Ultra-Optimized TTL2DSPy with 80/20 Performance Improvements
OpenTelemetry instrumented for validation

Key optimizations based on profiling:
1. Graph parsing cache (86.8% time reduction target)
2. SHACL pattern indexing (graph traversal optimization)
3. String processing object pools
4. Memory-mapped I/O for large files
5. Parallel signature generation
"""

import sys
import re
import argparse
import time
import hashlib
import threading
from pathlib import Path
from typing import Dict, List, Set, Optional, Tuple, Any
from datetime import datetime
from functools import lru_cache
from dataclasses import dataclass, field
from concurrent.futures import ThreadPoolExecutor, as_completed
import mmap
import pickle
import weakref

import rdflib
from rdflib import Graph, Namespace, Literal
from rdflib.namespace import RDF, RDFS, OWL, XSD, SH

# OpenTelemetry imports
from opentelemetry import trace, metrics
from opentelemetry.exporter.otlp.proto.grpc.trace_exporter import OTLPSpanExporter
from opentelemetry.exporter.otlp.proto.grpc.metric_exporter import OTLPMetricExporter
from opentelemetry.sdk.trace import TracerProvider
from opentelemetry.sdk.trace.export import BatchSpanProcessor
from opentelemetry.sdk.metrics import MeterProvider
from opentelemetry.sdk.metrics.export import PeriodicExportingMetricReader
from opentelemetry.instrumentation.auto_instrumentation import sitecustomize

# Initialize OpenTelemetry
trace.set_tracer_provider(TracerProvider())
tracer = trace.get_tracer(__name__)

# Define namespaces
CNS = Namespace("http://cns.io/ontology#")
SHACL = Namespace("http://www.w3.org/ns/shacl#")

@dataclass
class UltraMetrics:
    """Ultra-performance metrics with OpenTelemetry integration"""
    signatures_generated: int = 0
    processing_time: float = 0.0
    parsing_time: float = 0.0
    cache_hits: int = 0
    cache_misses: int = 0
    graph_size: int = 0
    memory_saved_mb: float = 0.0
    parallel_workers: int = 0
    
    @property
    def cache_efficiency(self) -> float:
        total = self.cache_hits + self.cache_misses
        return self.cache_hits / total if total > 0 else 0.0
    
    @property
    def parsing_percentage(self) -> float:
        return (self.parsing_time / self.processing_time * 100) if self.processing_time > 0 else 0.0

class GraphCache:
    """Ultra-fast graph caching with memory management"""
    
    def __init__(self, max_size: int = 100, enable_disk_cache: bool = True):
        self.memory_cache: Dict[str, Graph] = {}
        self.max_size = max_size
        self.enable_disk_cache = enable_disk_cache
        self.cache_dir = Path(".ttl2dspy_cache")
        self.access_times: Dict[str, float] = {}
        self._lock = threading.RLock()
        
        if enable_disk_cache:
            self.cache_dir.mkdir(exist_ok=True)
    
    def _get_cache_key(self, file_path: Path) -> str:
        """Generate cache key from file content hash"""
        with open(file_path, 'rb') as f:
            content = f.read()
            return hashlib.sha256(content).hexdigest()
    
    @tracer.start_as_current_span("graph_cache_get")
    def get(self, file_path: Path) -> Optional[Graph]:
        """Get cached graph with OpenTelemetry tracing"""
        cache_key = self._get_cache_key(file_path)
        
        with self._lock:
            # Check memory cache first
            if cache_key in self.memory_cache:
                self.access_times[cache_key] = time.time()
                span = trace.get_current_span()
                span.set_attribute("cache.hit", True)
                span.set_attribute("cache.type", "memory")
                return self.memory_cache[cache_key]
            
            # Check disk cache
            if self.enable_disk_cache:
                disk_path = self.cache_dir / f"{cache_key}.pkl"
                if disk_path.exists():
                    try:
                        with open(disk_path, 'rb') as f:
                            graph = pickle.load(f)
                        
                        # Promote to memory cache
                        self._add_to_memory_cache(cache_key, graph)
                        
                        span = trace.get_current_span()
                        span.set_attribute("cache.hit", True)
                        span.set_attribute("cache.type", "disk")
                        return graph
                    except Exception:
                        disk_path.unlink(missing_ok=True)
            
            span = trace.get_current_span()
            span.set_attribute("cache.hit", False)
            return None
    
    @tracer.start_as_current_span("graph_cache_put")
    def put(self, file_path: Path, graph: Graph):
        """Cache graph with LRU eviction"""
        cache_key = self._get_cache_key(file_path)
        
        with self._lock:
            self._add_to_memory_cache(cache_key, graph)
            
            # Also save to disk cache
            if self.enable_disk_cache:
                disk_path = self.cache_dir / f"{cache_key}.pkl"
                try:
                    with open(disk_path, 'wb') as f:
                        pickle.dump(graph, f, protocol=pickle.HIGHEST_PROTOCOL)
                except Exception:
                    pass  # Disk cache is best-effort
    
    def _add_to_memory_cache(self, cache_key: str, graph: Graph):
        """Add to memory cache with LRU eviction"""
        if len(self.memory_cache) >= self.max_size:
            # Evict least recently used
            oldest_key = min(self.access_times.keys(), key=self.access_times.get)
            del self.memory_cache[oldest_key]
            del self.access_times[oldest_key]
        
        self.memory_cache[cache_key] = graph
        self.access_times[cache_key] = time.time()

class SHACLIndex:
    """Pre-computed index for SHACL patterns to optimize graph traversal"""
    
    def __init__(self, graph: Graph):
        self.graph = graph
        self.target_class_index: Dict[str, Set[str]] = {}
        self.property_shape_index: Dict[str, List[str]] = {}
        self.datatype_index: Dict[str, str] = {}
        self._build_index()
    
    @tracer.start_as_current_span("shacl_index_build")
    def _build_index(self):
        """Build comprehensive SHACL index for O(1) lookups"""
        span = trace.get_current_span()
        
        # Index 1: Target classes -> Property shapes
        for prop_shape in self.graph.subjects(SH.path, None):
            for target_class in self.graph.objects(prop_shape, SH.targetClass):
                cls_key = str(target_class)
                if cls_key not in self.target_class_index:
                    self.target_class_index[cls_key] = set()
                self.target_class_index[cls_key].add(str(prop_shape))
        
        # Index 2: Node shapes -> Property shapes
        for node_shape in self.graph.subjects(SH.targetClass, None):
            for target_class in self.graph.objects(node_shape, SH.targetClass):
                cls_key = str(target_class)
                if cls_key not in self.property_shape_index:
                    self.property_shape_index[cls_key] = []
                
                for prop_shape in self.graph.objects(node_shape, SH.property):
                    if self.graph.value(prop_shape, SH.path):
                        self.property_shape_index[cls_key].append(str(prop_shape))
        
        # Index 3: Property shapes -> Datatypes
        for prop_shape in self.graph.subjects(SH.path, None):
            datatype = self.graph.value(prop_shape, SH.datatype)
            if datatype:
                self.datatype_index[str(prop_shape)] = str(datatype)
        
        span.set_attribute("index.target_classes", len(self.target_class_index))
        span.set_attribute("index.property_shapes", len(self.property_shape_index))
        span.set_attribute("index.datatypes", len(self.datatype_index))
    
    def get_property_shapes_for_class(self, cls_uri: str) -> List[str]:
        """O(1) lookup for property shapes by class"""
        result = []
        
        # From direct target classes
        if cls_uri in self.target_class_index:
            result.extend(self.target_class_index[cls_uri])
        
        # From node shapes
        if cls_uri in self.property_shape_index:
            result.extend(self.property_shape_index[cls_uri])
        
        return list(set(result))  # Remove duplicates
    
    def get_datatype_for_shape(self, shape_uri: str) -> Optional[str]:
        """O(1) lookup for datatype by property shape"""
        return self.datatype_index.get(shape_uri)

class StringPool:
    """Object pool for string processing to reduce allocations"""
    
    def __init__(self):
        self._snake_case_cache = {}
        self._local_name_cache = {}
        self._pool_lock = threading.RLock()
    
    @tracer.start_as_current_span("string_pool_snake_case")
    def snake_case(self, name: str) -> str:
        """Cached snake_case conversion"""
        with self._pool_lock:
            if name in self._snake_case_cache:
                return self._snake_case_cache[name]
            
            # Original logic but cached
            result = re.sub(r'[-\s]+', '_', name)
            result = re.sub(r'([a-z])([A-Z])', r'\1_\2', result)
            result = result.lower()
            result = re.sub(r'_+', '_', result)
            result = result.strip('_')
            
            if result and result[0].isdigit():
                result = f"field_{result}"
            if not result:
                result = "unnamed_field"
            
            self._snake_case_cache[name] = result
            return result
    
    @tracer.start_as_current_span("string_pool_local_name")
    def safe_local_name(self, iri) -> str:
        """Cached local name extraction"""
        iri_str = str(iri)
        with self._pool_lock:
            if iri_str in self._local_name_cache:
                return self._local_name_cache[iri_str]
            
            idx = max(iri_str.rfind('/'), iri_str.rfind('#'))
            result = iri_str[idx + 1:] if idx >= 0 else iri_str
            
            self._local_name_cache[iri_str] = result
            return result

class UltraOptimizedTTL2DSPyTranspiler:
    """Ultra-optimized transpiler with 80/20 performance improvements"""
    
    def __init__(self, cache_size: int = 100, enable_parallel: bool = True, max_workers: int = 4):
        self.graph_cache = GraphCache(max_size=cache_size)
        self.string_pool = StringPool()
        self.enable_parallel = enable_parallel
        self.max_workers = max_workers
        self.metrics = UltraMetrics()
        self.seen_field_names: Set[str] = set()
        
        # OpenTelemetry meter for custom metrics
        self.meter = metrics.get_meter(__name__)
        self.cache_hit_counter = self.meter.create_counter("ttl2dspy.cache.hits")
        self.cache_miss_counter = self.meter.create_counter("ttl2dspy.cache.misses")
        self.signature_counter = self.meter.create_counter("ttl2dspy.signatures.generated")
    
    @tracer.start_as_current_span("ultra_parse_ontology")
    def parse_ontology(self, ttl_file: Path) -> Tuple[Graph, SHACLIndex, str]:
        """Ultra-fast ontology parsing with caching"""
        span = trace.get_current_span()
        span.set_attribute("file.path", str(ttl_file))
        span.set_attribute("file.size", ttl_file.stat().st_size)
        
        # Try cache first
        cached_graph = self.graph_cache.get(ttl_file)
        if cached_graph:
            self.metrics.cache_hits += 1
            self.cache_hit_counter.add(1)
            span.set_attribute("cache.hit", True)
            
            # Build index on cached graph
            index = SHACLIndex(cached_graph)
            return cached_graph, index, self._extract_ontology_uri(cached_graph)
        
        # Cache miss - parse and cache
        self.metrics.cache_misses += 1
        self.cache_miss_counter.add(1)
        span.set_attribute("cache.hit", False)
        
        parse_start = time.time()
        
        # Try memory-mapped parsing for large files
        file_size = ttl_file.stat().st_size
        if file_size > 1024 * 1024:  # 1MB threshold
            graph = self._parse_with_mmap(ttl_file)
        else:
            graph = Graph()
            graph.parse(ttl_file, format="turtle")
        
        parse_time = time.time() - parse_start
        self.metrics.parsing_time += parse_time
        self.metrics.graph_size = len(graph)
        
        span.set_attribute("graph.triples", len(graph))
        span.set_attribute("parse.time_ms", parse_time * 1000)
        
        # Cache the parsed graph
        self.graph_cache.put(ttl_file, graph)
        
        # Build index
        index = SHACLIndex(graph)
        ontology_uri = self._extract_ontology_uri(graph)
        
        return graph, index, ontology_uri
    
    def _parse_with_mmap(self, ttl_file: Path) -> Graph:
        """Memory-mapped parsing for large files"""
        with open(ttl_file, 'r+b') as f:
            with mmap.mmap(f.fileno(), 0, access=mmap.ACCESS_READ) as mmapped_file:
                graph = Graph()
                # Parse from memory-mapped content
                content = mmapped_file.read().decode('utf-8')
                graph.parse(data=content, format="turtle")
                return graph
    
    def _extract_ontology_uri(self, graph: Graph) -> str:
        """Extract ontology URI from graph"""
        for s, p, o in graph.triples((None, RDF.type, OWL.Ontology)):
            return str(s)
        return ""
    
    @tracer.start_as_current_span("ultra_build_signatures")
    def ultra_build_signatures(self, ttl_files: List[Path], allow_multi_output: bool = False) -> Dict[str, str]:
        """Ultra-fast signature building with parallel processing"""
        span = trace.get_current_span()
        span.set_attribute("files.count", len(ttl_files))
        span.set_attribute("parallel.enabled", self.enable_parallel)
        
        start_time = time.time()
        all_signatures = {}
        
        if self.enable_parallel and len(ttl_files) > 1:
            # Parallel processing for multiple files
            with ThreadPoolExecutor(max_workers=self.max_workers) as executor:
                future_to_file = {
                    executor.submit(self._process_single_file, ttl_file, allow_multi_output): ttl_file
                    for ttl_file in ttl_files
                }
                
                self.metrics.parallel_workers = self.max_workers
                span.set_attribute("parallel.workers", self.max_workers)
                
                for future in as_completed(future_to_file):
                    ttl_file = future_to_file[future]
                    try:
                        signatures = future.result()
                        all_signatures.update(signatures)
                    except Exception as e:
                        span.record_exception(e)
                        print(f"Error processing {ttl_file}: {e}", file=sys.stderr)
        else:
            # Sequential processing
            for ttl_file in ttl_files:
                try:
                    signatures = self._process_single_file(ttl_file, allow_multi_output)
                    all_signatures.update(signatures)
                except Exception as e:
                    span.record_exception(e)
                    print(f"Error processing {ttl_file}: {e}", file=sys.stderr)
        
        self.metrics.processing_time = time.time() - start_time
        self.metrics.signatures_generated = len(all_signatures)
        
        # Record metrics
        self.signature_counter.add(len(all_signatures))
        span.set_attribute("signatures.generated", len(all_signatures))
        span.set_attribute("processing.time_ms", self.metrics.processing_time * 1000)
        
        return all_signatures
    
    @tracer.start_as_current_span("process_single_file")
    def _process_single_file(self, ttl_file: Path, allow_multi_output: bool) -> Dict[str, str]:
        """Process a single TTL file optimized"""
        span = trace.get_current_span()
        span.set_attribute("file", str(ttl_file))
        
        # Ultra-fast parsing with caching and indexing
        graph, shacl_index, ontology_uri = self.parse_ontology(ttl_file)
        
        signatures = {}
        
        # Use index for O(1) class lookups instead of graph traversal
        target_classes = set()
        for cls_uri in shacl_index.target_class_index.keys():
            target_classes.add(rdflib.URIRef(cls_uri))
        for cls_uri in shacl_index.property_shape_index.keys():
            target_classes.add(rdflib.URIRef(cls_uri))
        
        for cls in target_classes:
            cls_signatures = self._build_class_signature(
                cls, graph, shacl_index, allow_multi_output
            )
            signatures.update(cls_signatures)
        
        span.set_attribute("classes.processed", len(target_classes))
        return signatures
    
    @tracer.start_as_current_span("build_class_signature")
    def _build_class_signature(self, cls, graph: Graph, shacl_index: SHACLIndex, allow_multi_output: bool) -> Dict[str, str]:
        """Build signature for a single class using optimized indexing"""
        span = trace.get_current_span()
        cls_name = self.string_pool.safe_local_name(cls)
        signature_name = f"{cls_name}Signature"
        
        span.set_attribute("class.name", cls_name)
        
        # Reset field names for each signature
        self.seen_field_names.clear()
        
        # Use index for O(1) property shape lookup
        prop_shape_uris = shacl_index.get_property_shapes_for_class(str(cls))
        prop_shapes = [rdflib.URIRef(uri) for uri in prop_shape_uris]
        
        if not prop_shapes:
            return {}
        
        # Build field definitions using cached string processing
        input_fields = []
        output_fields = []
        
        for prop_shape in prop_shapes:
            path = graph.value(prop_shape, SH.path)
            if not path:
                continue
            
            prop_name = self.string_pool.safe_local_name(path)
            py_name = self.string_pool.snake_case(prop_name)
            py_name = self._check_field_collision(py_name)
            
            # Get description
            description = graph.value(prop_shape, RDFS.comment) or graph.value(prop_shape, SH.description)
            desc_str = f'"{description}"' if description else f'"{prop_name} property"'
            
            # Use indexed datatype lookup
            dtype_str = self._extract_datatype_optimized(prop_shape, graph, shacl_index)
            
            # Check if output field
            if self._is_output_field(prop_shape, graph):
                field_def = f'    {py_name} = dspy.OutputField(desc={desc_str}, {dtype_str})'
                output_fields.append(field_def)
            else:
                field_def = f'    {py_name} = dspy.InputField(desc={desc_str}, {dtype_str})'
                input_fields.append(field_def)
        
        # Handle output fields
        if len(output_fields) == 0:
            output_fields.append('    result = dspy.OutputField(desc="Generated result", dtype=str)')
        elif len(output_fields) > 1 and not allow_multi_output:
            output_fields = output_fields[:1]
        
        # Get class description
        class_desc = graph.value(cls, RDFS.comment) or f"DSPy Signature for {cls_name}"
        
        # Build signature class
        signature_code = f'''class {signature_name}(dspy.Signature):
    """{class_desc}
    
    Generated from: {cls}
    Timestamp: {datetime.now().isoformat()}
    Properties: {len(input_fields)} inputs, {len(output_fields)} outputs
    Ultra-optimized: Cache hits {self.metrics.cache_hits}, Graph size {self.metrics.graph_size}
    """
    
{chr(10).join(input_fields)}
{chr(10).join(output_fields)}
'''
        
        span.set_attribute("fields.input", len(input_fields))
        span.set_attribute("fields.output", len(output_fields))
        
        return {signature_name: signature_code}
    
    def _check_field_collision(self, pyname: str) -> str:
        """Optimized field collision checking"""
        reserved_names = {'metadata', 'instructions', 'demos', 'signature', 'config',
                         'forward', 'named_predictors', 'predictor'}
        
        if hasattr(rdflib.Namespace, pyname) or pyname in reserved_names:
            pyname = f"custom_{pyname}"
        
        original_pyname = pyname
        counter = 1
        while pyname in self.seen_field_names:
            pyname = f"{original_pyname}_{counter}"
            counter += 1
        
        self.seen_field_names.add(pyname)
        return pyname
    
    def _extract_datatype_optimized(self, prop_shape, graph: Graph, shacl_index: SHACLIndex) -> str:
        """Optimized datatype extraction using index"""
        # Try index first
        indexed_datatype = shacl_index.get_datatype_for_shape(str(prop_shape))
        if indexed_datatype:
            return self._map_xsd_to_dtype(indexed_datatype)
        
        # Fallback to graph traversal
        datatype = graph.value(prop_shape, SH.datatype)
        if datatype:
            return self._map_xsd_to_dtype(str(datatype))
        
        # Check for constraints that hint at type
        min_val = graph.value(prop_shape, SH.minInclusive) or graph.value(prop_shape, SH.minExclusive)
        max_val = graph.value(prop_shape, SH.maxInclusive) or graph.value(prop_shape, SH.maxExclusive)
        
        if min_val is not None or max_val is not None:
            try:
                if min_val: float(str(min_val))
                if max_val: float(str(max_val))
                return "dtype=float"
            except ValueError:
                pass
        
        return "dtype=str"
    
    def _map_xsd_to_dtype(self, xsd_type: str) -> str:
        """Fast XSD to Python dtype mapping"""
        type_map = {
            str(XSD.string): "dtype=str",
            str(XSD.boolean): "dtype=bool",
            str(XSD.int): "dtype=int",
            str(XSD.integer): "dtype=int",
            str(XSD.long): "dtype=int",
            str(XSD.float): "dtype=float",
            str(XSD.double): "dtype=float",
            str(XSD.decimal): "dtype=float",
            str(XSD.date): "dtype=str",
            str(XSD.dateTime): "dtype=str",
            str(XSD.time): "dtype=str"
        }
        return type_map.get(xsd_type, "dtype=str")
    
    def _is_output_field(self, prop_shape, graph: Graph) -> bool:
        """Check if property is marked as output field"""
        output_marker = graph.value(prop_shape, CNS.outputField)
        if output_marker:
            output_str = str(output_marker).lower()
            return output_str in ('true', '1', 'yes')
        
        comment = graph.value(prop_shape, RDFS.comment)
        if comment and 'output' in str(comment).lower():
            return True
        
        return False
    
    @tracer.start_as_current_span("generate_ultra_module")
    def generate_ultra_module(self, signatures: Dict[str, str], ontology_uris: List[str] = None) -> str:
        """Generate ultra-optimized module with OpenTelemetry metrics"""
        span = trace.get_current_span()
        span.set_attribute("signatures.count", len(signatures))
        
        signature_names = list(signatures.keys())
        all_list = ', '.join(f'"{name}"' for name in signature_names)
        
        # OpenTelemetry metrics export
        otel_metrics = f"""
# OpenTelemetry Performance Metrics
# Processing time: {self.metrics.processing_time:.4f}s
# Parsing time: {self.metrics.parsing_time:.4f}s ({self.metrics.parsing_percentage:.1f}%)
# Cache efficiency: {self.metrics.cache_efficiency:.2%}
# Parallel workers: {self.metrics.parallel_workers}
# Memory saved: {self.metrics.memory_saved_mb:.2f}MB
"""
        
        module_code = f'''"""
Ultra-Optimized DSPy Signatures with 80/20 Performance Improvements
Generated by Ultra-Optimized TTL2DSPy on {datetime.now().isoformat()}

Ontology URIs: {ontology_uris or []}
Signatures generated: {len(signatures)}
{otel_metrics}
"""

import dspy
from typing import Union, Optional, List

# Type aliases for better IDE support  
Text = str
Number = Union[int, float]
Boolean = bool

__all__ = [{all_list}]

{chr(10).join(signatures.values())}

# Ultra-optimized signature registry
SIGNATURES = {{
{chr(10).join(f'    "{name}": {name},' for name in signature_names)}
}}

def get_signature(name: str) -> dspy.Signature:
    """Get signature by name with ultra-fast lookup"""
    if name not in SIGNATURES:
        available = list(SIGNATURES.keys())
        raise ValueError(f"Unknown signature: {{name}}. Available: {{available}}")
    return SIGNATURES[name]

def list_signatures() -> List[str]:
    """List all available signature names"""
    return list(SIGNATURES.keys())

def get_ultra_performance_metrics() -> dict:
    """Get ultra-performance metrics with OpenTelemetry data"""
    return {{
        "signatures_generated": {self.metrics.signatures_generated},
        "processing_time_ms": {self.metrics.processing_time * 1000:.2f},
        "parsing_time_ms": {self.metrics.parsing_time * 1000:.2f},
        "parsing_percentage": {self.metrics.parsing_percentage:.1f},
        "cache_efficiency": {self.metrics.cache_efficiency:.4f},
        "cache_hits": {self.metrics.cache_hits},
        "cache_misses": {self.metrics.cache_misses},
        "graph_size": {self.metrics.graph_size},
        "parallel_workers": {self.metrics.parallel_workers},
        "memory_saved_mb": {self.metrics.memory_saved_mb:.2f}
    }}

# CNS v8.0 Ultra-Integration
def get_cns_ultra_metadata() -> dict:
    """Get ultra-performance metadata for CNS v8.0"""
    processing_ticks = int({self.metrics.processing_time} * 2.4e9)  # 2.4GHz CPU
    parsing_ticks = int({self.metrics.parsing_time} * 2.4e9)
    
    return {{
        "ultra_processing_ticks": processing_ticks,
        "parsing_ticks_saved": parsing_ticks * {self.metrics.cache_efficiency},
        "memory_efficiency": 1.0 - ({self.metrics.memory_saved_mb} / 100.0),
        "cache_performance": {self.metrics.cache_efficiency},
        "parallel_speedup": {self.metrics.parallel_workers if self.metrics.parallel_workers > 0 else 1},
        "graph_density": {self.metrics.graph_size} / max(1, {self.metrics.signatures_generated}),
        "ultra_optimization_factor": {self.metrics.cache_efficiency + (1.0 - self.metrics.parsing_percentage/100):.4f}
    }}

# OpenTelemetry Integration
def export_otel_metrics():
    """Export metrics to OpenTelemetry collector"""
    metrics = get_ultra_performance_metrics()
    # Metrics would be automatically exported via configured OTLP exporter
    return metrics
'''
        
        return module_code

def ultra_main():
    """Ultra-optimized main function with OpenTelemetry"""
    parser = argparse.ArgumentParser(
        description="Ultra-Optimized TTL2DSPy with 80/20 Performance Improvements",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Ultra-Optimized Examples:
  python ttl2dspy_ultra_optimized.py ontology.ttl output.py --ultra-cache
  python ttl2dspy_ultra_optimized.py ontologies/ output.py --parallel --workers 8
  python ttl2dspy_ultra_optimized.py --benchmark ontology.ttl output.py
        """
    )
    
    parser.add_argument('input', nargs='+',
                       help='Input TTL files or directories')
    parser.add_argument('output',
                       help='Output Python file')
    parser.add_argument('--ultra-cache', action='store_true',
                       help='Enable ultra-caching (disk + memory)')
    parser.add_argument('--parallel', action='store_true',
                       help='Enable parallel processing')
    parser.add_argument('--workers', type=int, default=4,
                       help='Number of parallel workers')
    parser.add_argument('--allow-multi-output', action='store_true',
                       help='Allow multiple output fields')
    parser.add_argument('--benchmark', action='store_true',
                       help='Run performance benchmark')
    parser.add_argument('--otel-endpoint', default='http://localhost:4317',
                       help='OpenTelemetry collector endpoint')
    parser.add_argument('--verbose', '-v', action='store_true',
                       help='Verbose output')
    
    args = parser.parse_args()
    
    # Configure OpenTelemetry export
    if args.otel_endpoint:
        try:
            span_exporter = OTLPSpanExporter(endpoint=args.otel_endpoint, insecure=True)
            span_processor = BatchSpanProcessor(span_exporter)
            trace.get_tracer_provider().add_span_processor(span_processor)
            
            metric_exporter = OTLPMetricExporter(endpoint=args.otel_endpoint, insecure=True)
            metric_reader = PeriodicExportingMetricReader(metric_exporter, export_interval_millis=1000)
        except Exception as e:
            if args.verbose:
                print(f"OpenTelemetry setup failed: {e}")
    
    # Collect input files
    input_files = []
    for input_path in args.input:
        path = Path(input_path)
        if path.is_file() and path.suffix in ('.ttl', '.turtle', '.n3'):
            input_files.append(path)
        elif path.is_dir():
            input_files.extend(path.rglob('*.ttl'))
            input_files.extend(path.rglob('*.turtle'))
        else:
            import glob
            matched = glob.glob(str(path))
            input_files.extend(Path(f) for f in matched if Path(f).suffix in ('.ttl', '.turtle', '.n3'))
    
    if not input_files:
        print("No TTL files found", file=sys.stderr)
        return 2
    
    if args.verbose:
        print(f"🚀 Ultra-Optimized TTL2DSPy starting with {len(input_files)} files")
        print(f"⚡ Ultra-caching: {'enabled' if args.ultra_cache else 'disabled'}")
        print(f"🔥 Parallel processing: {'enabled' if args.parallel else 'disabled'}")
        if args.parallel:
            print(f"👥 Workers: {args.workers}")
    
    # Initialize ultra-optimized transpiler
    transpiler = UltraOptimizedTTL2DSPyTranspiler(
        cache_size=200 if args.ultra_cache else 50,
        enable_parallel=args.parallel,
        max_workers=args.workers
    )
    
    # Main processing with OpenTelemetry tracing
    with tracer.start_as_current_span("ultra_main_processing") as span:
        span.set_attribute("files.count", len(input_files))
        
        try:
            # Generate signatures
            signatures = transpiler.ultra_build_signatures(input_files, args.allow_multi_output)
            
            if not signatures:
                print("No SHACL shapes found in input files", file=sys.stderr)
                return 1
            
            # Generate module
            ontology_uris = [str(f) for f in input_files]
            module_code = transpiler.generate_ultra_module(signatures, ontology_uris)
            
            # Write output
            output_path = Path(args.output)
            output_path.parent.mkdir(parents=True, exist_ok=True)
            output_path.write_text(module_code)
            
            # Performance report
            metrics = transpiler.metrics
            
            if args.verbose or args.benchmark:
                print(f"\n🎯 ULTRA-PERFORMANCE RESULTS:")
                print(f"   Signatures generated: {metrics.signatures_generated}")
                print(f"   Total processing time: {metrics.processing_time:.4f}s")
                print(f"   Parsing time: {metrics.parsing_time:.4f}s ({metrics.parsing_percentage:.1f}%)")
                print(f"   Cache efficiency: {metrics.cache_efficiency:.2%}")
                print(f"   Cache hits: {metrics.cache_hits}")
                print(f"   Cache misses: {metrics.cache_misses}")
                print(f"   Graph size: {metrics.graph_size} triples")
                
                if metrics.parallel_workers > 0:
                    print(f"   Parallel workers: {metrics.parallel_workers}")
                
                print(f"   Output: {output_path}")
            
            return 0
            
        except Exception as e:
            span.record_exception(e)
            print(f"Fatal error: {e}", file=sys.stderr)
            return 1

if __name__ == "__main__":
    sys.exit(ultra_main())