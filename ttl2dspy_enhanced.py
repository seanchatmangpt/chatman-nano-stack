#!/usr/bin/env python3
"""
Enhanced TTL to DSPy Signature Transpiler
Real improvements using 80/20 principle and Lean Six Sigma methodology

Key improvements (20% effort, 80% impact):
1. Performance optimization - Caching and graph traversal efficiency
2. Quality control - Validation and error handling improvements  
3. Multi-output support - Complete implementation of TODO item
4. Better type inference - Enhanced datatype mapping
5. Integration hooks - CNS v8.0 compatibility
6. Statistics and metrics - Process capability tracking
"""

import sys
import re
import argparse
import time
from pathlib import Path
from typing import Dict, List, Set, Optional, Tuple, Any
from datetime import datetime
from functools import lru_cache
from dataclasses import dataclass, field
import json
import rdflib
from rdflib import Graph, Namespace, Literal
from rdflib.namespace import RDF, RDFS, OWL, XSD, SH

# Define namespaces
CNS = Namespace("http://cns.io/ontology#")
SHACL = Namespace("http://www.w3.org/ns/shacl#")

@dataclass
class ProcessMetrics:
    """Track process quality metrics (Lean Six Sigma)"""
    signatures_generated: int = 0
    processing_time: float = 0.0
    error_count: int = 0
    warning_count: int = 0
    cache_hits: int = 0
    cache_misses: int = 0
    
    @property
    def error_rate(self) -> float:
        total_ops = self.signatures_generated + self.error_count
        return self.error_count / total_ops if total_ops > 0 else 0.0
    
    @property
    def cache_hit_rate(self) -> float:
        total_requests = self.cache_hits + self.cache_misses
        return self.cache_hits / total_requests if total_requests > 0 else 0.0

class EnhancedTTL2DSPyTranspiler:
    """Enhanced TTL to DSPy transpiler with real improvements"""
    
    def __init__(self, enable_caching: bool = True, quality_mode: bool = True):
        self.seen_field_names: Set[str] = set()
        self.signature_count = 0
        self.enable_caching = enable_caching
        self.quality_mode = quality_mode
        self.metrics = ProcessMetrics()
        
        # Performance: Cache for expensive operations
        self._property_shapes_cache: Dict[str, List] = {}
        self._datatype_cache: Dict[str, str] = {}
        self._class_hierarchy_cache: Dict[str, Set[str]] = {}
        
        # Quality: Track common issues
        self.quality_issues: List[Dict[str, Any]] = []
        
    @lru_cache(maxsize=1000)
    def safe_local_name(self, iri) -> str:
        """Extract local name safely and efficiently - now cached"""
        s = str(iri)
        idx = max(s.rfind('/'), s.rfind('#'))
        return s[idx + 1:] if idx >= 0 else s
    
    @lru_cache(maxsize=500)
    def snake_case(self, name: str) -> str:
        """Convert to snake_case Python identifier - now cached"""
        # Replace hyphens and other non-alphanumeric chars with underscores
        name = re.sub(r'[-\s]+', '_', name)
        # Insert underscores before capitals that follow lowercase
        name = re.sub(r'([a-z])([A-Z])', r'\1_\2', name)
        # Convert to lowercase
        name = name.lower()
        # Remove multiple consecutive underscores
        name = re.sub(r'_+', '_', name)
        # Remove leading/trailing underscores
        name = name.strip('_')
        
        # Ensure it starts with letter or underscore
        if name and name[0].isdigit():
            name = f"field_{name}"
        
        # Handle empty names
        if not name:
            name = "unnamed_field"
            
        return name
    
    def check_field_collision(self, pyname: str, original_iri: str) -> str:
        """Check for field name collisions and resolve them"""
        # Check collision with DSPy Signature reserved names
        reserved_names = {'metadata', 'instructions', 'demos', 'signature', 'config', 
                         'forward', 'named_predictors', 'predictor'}
        
        if hasattr(rdflib.Namespace, pyname) or pyname in reserved_names:
            pyname = f"custom_{pyname}"
            if self.quality_mode:
                self.quality_issues.append({
                    "type": "name_collision",
                    "original": original_iri,
                    "resolved": pyname,
                    "severity": "warning"
                })
        
        # Handle duplicate field names
        original_pyname = pyname
        counter = 1
        while pyname in self.seen_field_names:
            pyname = f"{original_pyname}_{counter}"
            counter += 1
        
        self.seen_field_names.add(pyname)
        return pyname
    
    def extract_datatype(self, prop_shape, g: Graph) -> str:
        """Enhanced datatype extraction with better type inference"""
        cache_key = str(prop_shape)
        
        if self.enable_caching and cache_key in self._datatype_cache:
            self.metrics.cache_hits += 1
            return self._datatype_cache[cache_key]
        
        self.metrics.cache_misses += 1
        
        # Check for sh:datatype
        datatype = g.value(prop_shape, SH.datatype)
        if datatype:
            if datatype == XSD.string:
                result = "dtype=str"
            elif datatype == XSD.boolean:
                result = "dtype=bool"
            elif datatype in (XSD.int, XSD.integer, XSD.long, XSD.nonNegativeInteger, 
                             XSD.positiveInteger, XSD.negativeInteger):
                result = "dtype=int"
            elif datatype in (XSD.float, XSD.double, XSD.decimal):
                result = "dtype=float"
            elif datatype in (XSD.date, XSD.dateTime, XSD.time):
                result = "dtype=str"  # Handle dates as strings
            else:
                result = "dtype=str"  # Default fallback
        else:
            # Check for sh:class (object properties)
            class_constraint = g.value(prop_shape, SH.class_)
            if class_constraint:
                result = "dtype=str"  # Objects referenced as strings
            else:
                # Enhanced inference: check for constraints that hint at type
                min_val = g.value(prop_shape, SH.minInclusive) or g.value(prop_shape, SH.minExclusive)
                max_val = g.value(prop_shape, SH.maxInclusive) or g.value(prop_shape, SH.maxExclusive)
                
                if min_val is not None or max_val is not None:
                    # Numeric constraints suggest numeric type
                    try:
                        if min_val: float(str(min_val))
                        if max_val: float(str(max_val))
                        result = "dtype=float"
                    except ValueError:
                        result = "dtype=str"
                else:
                    result = "dtype=str"  # Default to string
        
        if self.enable_caching:
            self._datatype_cache[cache_key] = result
        
        return result
    
    def find_property_shapes(self, cls, g: Graph) -> List:
        """Find property shapes using both direct and node-shape patterns - cached"""
        cache_key = str(cls)
        
        if self.enable_caching and cache_key in self._property_shapes_cache:
            self.metrics.cache_hits += 1
            return self._property_shapes_cache[cache_key]
        
        self.metrics.cache_misses += 1
        prop_shapes = set()
        
        # Pattern 1: Direct sh:targetClass on property shapes
        for prop_shape in g.subjects(SH.path, None):
            if (prop_shape, SH.targetClass, cls) in g:
                prop_shapes.add(prop_shape)
        
        # Pattern 2: Node shapes with sh:property links
        for node_shape in g.subjects(SH.targetClass, cls):
            for prop_shape in g.objects(node_shape, SH.property):
                if g.value(prop_shape, SH.path):  # Must have a path
                    prop_shapes.add(prop_shape)
        
        result = list(prop_shapes)
        
        if self.enable_caching:
            self._property_shapes_cache[cache_key] = result
        
        return result
    
    def is_output_field(self, prop_shape, g: Graph) -> bool:
        """Check if property is marked as output field"""
        # Check for cns:outputField annotation
        output_marker = g.value(prop_shape, CNS.outputField)
        if output_marker:
            # Accept various forms of "true"
            output_str = str(output_marker).lower()
            return output_str in ('true', '1', 'yes')
        
        # Check for rdfs:comment containing "output"
        comment = g.value(prop_shape, RDFS.comment)
        if comment and 'output' in str(comment).lower():
            return True
            
        return False
    
    def build_signatures(self, g: Graph, allow_multi_output: bool = False) -> Dict[str, str]:
        """Build DSPy Signatures from ontology classes with SHACL shapes"""
        start_time = time.time()
        signatures = {}
        
        # Find all classes with SHACL shapes
        classes_with_shapes = set()
        
        # Collect classes from both patterns
        for target_class in g.objects(None, SH.targetClass):
            classes_with_shapes.add(target_class)
        
        for cls in classes_with_shapes:
            try:
                cls_name = self.safe_local_name(cls)
                signature_name = f"{cls_name}Signature"
                
                # Reset field names for each signature
                self.seen_field_names.clear()
                
                # Find property shapes for this class
                prop_shapes = self.find_property_shapes(cls, g)
                
                if not prop_shapes:
                    if self.quality_mode:
                        self.quality_issues.append({
                            "type": "no_properties",
                            "class": str(cls),
                            "severity": "warning"
                        })
                    continue
                
                # Build field definitions
                input_fields = []
                output_fields = []
                
                for prop_shape in prop_shapes:
                    path = g.value(prop_shape, SH.path)
                    if not path:
                        continue
                    
                    prop_name = self.safe_local_name(path)
                    py_name = self.snake_case(prop_name)
                    py_name = self.check_field_collision(py_name, str(path))
                    
                    # Get description
                    description = g.value(prop_shape, RDFS.comment) or g.value(prop_shape, SH.description)
                    desc_str = f'"{description}"' if description else f'"{prop_name} property"'
                    
                    # Get datatype
                    dtype = self.extract_datatype(prop_shape, g)
                    
                    # Create field definition
                    if self.is_output_field(prop_shape, g):
                        field_def = f'    {py_name} = dspy.OutputField(desc={desc_str}, {dtype})'
                        output_fields.append(field_def)
                    else:
                        field_def = f'    {py_name} = dspy.InputField(desc={desc_str}, {dtype})'
                        input_fields.append(field_def)
                
                # Handle output fields based on policy
                if len(output_fields) == 0:
                    # Add a default output field
                    output_fields.append('    result = dspy.OutputField(desc="Generated result", dtype=str)')
                elif len(output_fields) > 1 and not allow_multi_output:
                    # Keep only the first output field for compatibility
                    if self.quality_mode:
                        self.quality_issues.append({
                            "type": "multiple_outputs_truncated",
                            "class": str(cls),
                            "count": len(output_fields),
                            "severity": "info"
                        })
                    output_fields = output_fields[:1]
                
                # Get class description
                class_desc = g.value(cls, RDFS.comment) or f"DSPy Signature for {cls_name}"
                
                # Build signature class with enhanced metadata
                signature_code = f'''class {signature_name}(dspy.Signature):
    """{class_desc}
    
    Generated from: {cls}
    Timestamp: {datetime.now().isoformat()}
    Properties: {len(input_fields)} inputs, {len(output_fields)} outputs
    """
    
{chr(10).join(input_fields)}
{chr(10).join(output_fields)}
'''
                
                signatures[signature_name] = signature_code
                self.signature_count += 1
                self.metrics.signatures_generated += 1
                
            except Exception as e:
                self.metrics.error_count += 1
                if self.quality_mode:
                    self.quality_issues.append({
                        "type": "signature_generation_error",
                        "class": str(cls),
                        "error": str(e),
                        "severity": "error"
                    })
                continue
        
        self.metrics.processing_time = time.time() - start_time
        return signatures
    
    def generate_enhanced_module(self, signatures: Dict[str, str], ontology_uri: str = "") -> str:
        """Generate enhanced Python module with quality metadata"""
        
        # Generate __all__ list
        signature_names = list(signatures.keys())
        all_list = ', '.join(f'"{name}"' for name in signature_names)
        
        # Quality metrics summary
        quality_summary = f"""
# Quality Metrics Summary
# Signatures generated: {self.metrics.signatures_generated}
# Processing time: {self.metrics.processing_time:.4f}s
# Error rate: {self.metrics.error_rate:.2%}
# Cache hit rate: {self.metrics.cache_hit_rate:.2%}
# Quality issues: {len(self.quality_issues)}
"""
        
        module_code = f'''"""
Enhanced DSPy Signatures generated from Turtle ontology
Generated by Enhanced TTL2DSPy on {datetime.now().isoformat()}

Ontology URI: {ontology_uri}
Signatures generated: {len(signatures)}
{quality_summary}
"""

import dspy
from typing import Union, Optional, List

# Type aliases for better IDE support
Text = str
Number = Union[int, float]
Boolean = bool

__all__ = [{all_list}]

{chr(10).join(signatures.values())}

# Auto-generated signature registry with enhanced features
SIGNATURES = {{
{chr(10).join(f'    "{name}": {name},' for name in signature_names)}
}}

def get_signature(name: str) -> dspy.Signature:
    """Get signature by name with validation"""
    if name not in SIGNATURES:
        available = list(SIGNATURES.keys())
        raise ValueError(f"Unknown signature: {{name}}. Available: {{available}}")
    return SIGNATURES[name]

def list_signatures() -> List[str]:
    """List all available signature names"""
    return list(SIGNATURES.keys())

def get_quality_report() -> dict:
    """Get quality metrics for this generation"""
    return {{
        "signatures_count": {len(signatures)},
        "processing_time": {self.metrics.processing_time:.4f},
        "error_rate": {self.metrics.error_rate:.2%},
        "cache_hit_rate": {self.metrics.cache_hit_rate:.2%},
        "quality_issues": {len(self.quality_issues)}
    }}

# CNS v8.0 Integration hooks
def get_cns_performance_metadata() -> dict:
    """Get performance metadata for CNS v8.0 integration"""
    return {{
        "generation_ticks": int({self.metrics.processing_time} * 2.4e9),  # Approximate CPU ticks at 2.4GHz
        "cache_efficiency": {self.metrics.cache_hit_rate:.4f},
        "error_density": {self.metrics.error_count} / max(1, {self.metrics.signatures_generated}),
        "quality_score": max(0, 1.0 - {self.metrics.error_rate}),
        "signature_density": {len(signatures)} / max(1, len({signature_names!r}))
    }}
'''
        
        return module_code
    
    def get_quality_report(self) -> Dict[str, Any]:
        """Generate detailed quality report"""
        return {
            "metrics": {
                "signatures_generated": self.metrics.signatures_generated,
                "processing_time_seconds": self.metrics.processing_time,
                "error_count": self.metrics.error_count,
                "error_rate": self.metrics.error_rate,
                "cache_hit_rate": self.metrics.cache_hit_rate,
                "performance_score": 1.0 - self.metrics.error_rate
            },
            "quality_issues": self.quality_issues,
            "improvement_opportunities": self._identify_improvements(),
            "process_capability": self._calculate_process_capability()
        }
    
    def _identify_improvements(self) -> List[str]:
        """Identify improvement opportunities based on metrics"""
        improvements = []
        
        if self.metrics.cache_hit_rate < 0.8:
            improvements.append("Consider increasing cache size for better performance")
        
        if self.metrics.error_rate > 0.05:
            improvements.append("High error rate detected - review input data quality")
        
        error_types = {}
        for issue in self.quality_issues:
            error_types[issue["type"]] = error_types.get(issue["type"], 0) + 1
        
        for error_type, count in error_types.items():
            if count > 5:
                improvements.append(f"Frequent {error_type} issues detected ({count} occurrences)")
        
        return improvements
    
    def _calculate_process_capability(self) -> Dict[str, float]:
        """Calculate process capability metrics (Six Sigma)"""
        # Simple capability calculation
        target_error_rate = 0.01  # 1% target error rate
        actual_error_rate = self.metrics.error_rate
        
        # Cpk approximation
        if actual_error_rate == 0:
            cpk = 2.0  # High capability
        else:
            cpk = max(0, (target_error_rate - actual_error_rate) / (3 * max(actual_error_rate, 0.001)))
        
        return {
            "cpk": cpk,
            "target_error_rate": target_error_rate,
            "actual_error_rate": actual_error_rate,
            "sigma_level": min(6.0, 3.0 + cpk)
        }

def enhanced_main():
    """Enhanced main function with improved functionality"""
    parser = argparse.ArgumentParser(
        description="Enhanced TTL to DSPy Signature Transpiler with Lean Six Sigma improvements",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Enhanced Examples:
  python ttl2dspy_enhanced.py ontology.ttl signature.py
  python ttl2dspy_enhanced.py ontologies/*.ttl signatures/ --batch --quality-report
  python ttl2dspy_enhanced.py --merge ontologies/ all_signatures.py --allow-multi-output
  python ttl2dspy_enhanced.py --performance-mode ontology.ttl output.py
        """
    )
    
    parser.add_argument('input', nargs='+', 
                       help='Input TTL files or directories (supports globs)')
    parser.add_argument('output', 
                       help='Output Python file or directory')
    parser.add_argument('--merge', action='store_true',
                       help='Combine all signatures into single module')
    parser.add_argument('--batch', action='store_true',
                       help='Process multiple files, one output per input')
    parser.add_argument('--allow-multi-output', action='store_true',
                       help='Allow multiple output fields per signature')
    parser.add_argument('--quality-report', action='store_true',
                       help='Generate detailed quality report')
    parser.add_argument('--performance-mode', action='store_true',
                       help='Enable performance optimizations and caching')
    parser.add_argument('--no-cache', action='store_true',
                       help='Disable caching for debugging')
    parser.add_argument('--verbose', '-v', action='store_true',
                       help='Verbose output')
    
    args = parser.parse_args()
    
    # Initialize enhanced transpiler
    enable_caching = not args.no_cache and (args.performance_mode or not args.verbose)
    transpiler = EnhancedTTL2DSPyTranspiler(
        enable_caching=enable_caching,
        quality_mode=True
    )
    
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
            # Try glob pattern
            import glob
            matched = glob.glob(str(path))
            input_files.extend(Path(f) for f in matched if Path(f).suffix in ('.ttl', '.turtle', '.n3'))
    
    if not input_files:
        print("No TTL files found", file=sys.stderr)
        return 2
    
    output_path = Path(args.output)
    success_count = 0
    error_count = 0
    
    if args.verbose:
        print(f"Enhanced TTL2DSPy starting with {len(input_files)} files")
        print(f"Caching: {'enabled' if enable_caching else 'disabled'}")
        print(f"Multi-output: {'allowed' if args.allow_multi_output else 'restricted'}")
    
    try:
        if args.batch:
            # Batch mode - one output file per input
            output_path.mkdir(parents=True, exist_ok=True)
            
            for ttl_file in input_files:
                if args.verbose:
                    print(f"Processing {ttl_file}...")
                
                try:
                    g = Graph()
                    g.parse(ttl_file, format="turtle")
                    
                    signatures = transpiler.build_signatures(g, allow_multi_output=args.allow_multi_output)
                    
                    if not signatures:
                        if args.verbose:
                            print(f"No SHACL shapes found in {ttl_file}")
                        continue
                    
                    output_file = output_path / f"signatures_{ttl_file.stem}.py"
                    module_code = transpiler.generate_enhanced_module(signatures, str(ttl_file))
                    
                    output_file.write_text(module_code)
                    success_count += 1
                    
                    if args.verbose:
                        print(f"Generated {len(signatures)} signatures -> {output_file}")
                
                except Exception as e:
                    error_count += 1
                    print(f"Error processing {ttl_file}: {e}", file=sys.stderr)
        
        elif args.merge:
            # Merge mode - combine all signatures
            all_signatures = {}
            combined_uri = ""
            
            for ttl_file in input_files:
                if args.verbose:
                    print(f"Processing {ttl_file}...")
                
                try:
                    g = Graph()
                    g.parse(ttl_file, format="turtle")
                    
                    signatures = transpiler.build_signatures(g, allow_multi_output=args.allow_multi_output)
                    
                    if signatures:
                        all_signatures.update(signatures)
                        if not combined_uri:
                            combined_uri = str(ttl_file)
                
                except Exception as e:
                    error_count += 1
                    print(f"Error processing {ttl_file}: {e}", file=sys.stderr)
            
            if all_signatures:
                module_code = transpiler.generate_enhanced_module(all_signatures, combined_uri)
                output_path.write_text(module_code)
                success_count = 1
                
                if args.verbose:
                    print(f"Generated {len(all_signatures)} merged signatures -> {output_path}")
        
        else:
            # Single file mode
            if len(input_files) > 1:
                print("Multiple input files require --batch or --merge mode", file=sys.stderr)
                return 2
            
            ttl_file = input_files[0]
            if args.verbose:
                print(f"Processing {ttl_file}...")
            
            g = Graph()
            g.parse(ttl_file, format="turtle")
            
            signatures = transpiler.build_signatures(g, allow_multi_output=args.allow_multi_output)
            
            if not signatures:
                print(f"No SHACL shapes found in {ttl_file}", file=sys.stderr)
                return 1
            
            module_code = transpiler.generate_enhanced_module(signatures, str(ttl_file))
            output_path.write_text(module_code)
            success_count = 1
            
            if args.verbose:
                print(f"Generated {len(signatures)} signatures -> {output_path}")
        
        # Quality reporting
        if args.quality_report or args.verbose:
            quality_report = transpiler.get_quality_report()
            
            print(f"\n=== QUALITY REPORT ===")
            print(f"Signatures generated: {quality_report['metrics']['signatures_generated']}")
            print(f"Processing time: {quality_report['metrics']['processing_time_seconds']:.4f}s")
            print(f"Error rate: {quality_report['metrics']['error_rate']:.2%}")
            print(f"Cache hit rate: {quality_report['metrics']['cache_hit_rate']:.2%}")
            print(f"Process Cpk: {quality_report['process_capability']['cpk']:.3f}")
            print(f"Sigma level: {quality_report['process_capability']['sigma_level']:.2f}")
            
            if quality_report['quality_issues']:
                print(f"\nQuality Issues ({len(quality_report['quality_issues'])}):")
                for issue in quality_report['quality_issues'][:10]:  # Show first 10
                    print(f"  {issue['severity'].upper()}: {issue['type']}")
            
            if quality_report['improvement_opportunities']:
                print(f"\nImprovement Opportunities:")
                for improvement in quality_report['improvement_opportunities']:
                    print(f"  • {improvement}")
        
        # Print summary
        if args.verbose or error_count > 0:
            print(f"\nProcessed: {success_count} success, {error_count} errors")
        
        # Return appropriate exit code
        if error_count > 0:
            return 1 if success_count == 0 else 3  # 3 = partial success
        return 0
    
    except Exception as e:
        print(f"Fatal error: {e}", file=sys.stderr)
        return 1

if __name__ == "__main__":
    sys.exit(enhanced_main())