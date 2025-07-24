#!/usr/bin/env python3
"""
BitActor TTL Code Generator
Generates BitActor implementations in C, Python, and Erlang from TTL ontologies
Uses Jinja templates for code generation with 80/20 optimization
"""

import os
import sys
from pathlib import Path
from typing import Dict, List, Any, Optional
import rdflib
from rdflib import Graph, URIRef, Literal, Namespace, RDF, RDFS, OWL
from jinja2 import Environment, FileSystemLoader
import json

# Define namespaces
BA = Namespace("http://bitactor.org/ontology#")
SH = Namespace("http://www.w3.org/ns/shacl#")
XSD = Namespace("http://www.w3.org/2001/XMLSchema#")

class BitActorTTLGenerator:
    """Generate BitActor code from TTL ontologies using Jinja templates"""
    
    def __init__(self, template_dir: str = "templates/bitactor"):
        self.template_dir = Path(template_dir)
        self.env = Environment(
            loader=FileSystemLoader(str(self.template_dir)),
            trim_blocks=True,
            lstrip_blocks=True
        )
        
        # Add custom filters
        self.env.filters['c_identifier'] = self._c_identifier
        self.env.filters['upper'] = lambda x: x.upper()
        self.env.filters['lower'] = lambda x: x.lower()
        
        self.graph = Graph()
        
    def _c_identifier(self, name: str) -> str:
        """Convert to valid C identifier"""
        import re
        name = name.split('#')[-1].split('/')[-1]
        name = re.sub(r'[^a-zA-Z0-9_]', '_', name)
        if name and name[0].isdigit():
            name = '_' + name
        return name or '_unnamed'
    
    def load_ttl(self, ttl_file: str) -> None:
        """Load TTL ontology"""
        self.graph.parse(ttl_file, format='turtle')
        print(f"✅ Loaded {len(self.graph)} triples from {ttl_file}")
    
    def extract_signals(self) -> List[Dict[str, Any]]:
        """Extract signal definitions from TTL"""
        signals = []
        signal_id = 1
        
        # Find all subclasses of ba:Signal
        for signal_class in self.graph.subjects(RDFS.subClassOf, BA.Signal):
            signal_name = str(signal_class).split('#')[-1]
            
            # Get signal metadata
            label = self.graph.value(signal_class, RDFS.label, default=signal_name)
            comment = self.graph.value(signal_class, RDFS.comment, default="")
            
            signals.append({
                'id': signal_id,
                'name': signal_name,
                'label': str(label),
                'description': str(comment),
                'uri': str(signal_class)
            })
            signal_id += 1
        
        # Also find direct instances of ba:Signal
        for signal in self.graph.subjects(RDF.type, BA.Signal):
            signal_name = str(signal).split('#')[-1]
            if not any(s['name'] == signal_name for s in signals):
                signals.append({
                    'id': signal_id,
                    'name': signal_name,
                    'label': signal_name,
                    'description': '',
                    'uri': str(signal)
                })
                signal_id += 1
        
        return signals
    
    def extract_handlers(self) -> List[Dict[str, Any]]:
        """Extract handler definitions from TTL"""
        handlers = []
        
        # Find all handlers
        for handler in self.graph.subjects(RDF.type, BA.Handler):
            handler_name = str(handler).split('#')[-1]
            
            # Get handler metadata
            processes_signal = self.graph.value(handler, BA.processesSignal)
            tick_budget = self.graph.value(handler, BA.hasTickBudget, default=8)
            
            # Extract operations (simplified - extend as needed)
            operations = []
            for op in self.graph.objects(handler, BA.hasOperation):
                operations.append(f"/* Operation: {op} */")
            
            if processes_signal:
                signal_name = str(processes_signal).split('#')[-1]
                handlers.append({
                    'name': handler_name,
                    'signal': signal_name,
                    'tick_budget': int(tick_budget),
                    'operations': operations or ["/* TODO: Implement handler */"],
                    'description': str(self.graph.value(handler, RDFS.comment, default=""))
                })
        
        # If no explicit handlers, create default ones for each signal
        if not handlers:
            for signal in self.extract_signals():
                handlers.append({
                    'name': f"{signal['name'].lower()}_handler",
                    'signal': signal['name'],
                    'tick_budget': 8,
                    'operations': [
                        f"/* Process {signal['name']} signal */",
                        "/* TODO: Implement signal processing */"
                    ],
                    'description': f"Handler for {signal['label']}"
                })
        
        return handlers
    
    def generate_context(self, ontology_name: str, prefix: str) -> Dict[str, Any]:
        """Generate template context from TTL data"""
        signals = self.extract_signals()
        handlers = self.extract_handlers()
        
        # Extract performance constraints
        tick_budget = 8  # Default
        for constraint in self.graph.subjects(RDF.type, BA.TickBudget):
            value = self.graph.value(constraint, RDF.value)
            if value:
                tick_budget = int(value)
                break
        
        return {
            'ontology_name': ontology_name,
            'prefix': prefix,
            'module_name': prefix,
            'class_prefix': prefix.title(),
            'guard_name': f"{prefix.upper()}_BITACTOR_H",
            'signals': signals,
            'handlers': handlers,
            'tick_budget': tick_budget,
            'max_signals': 256,
            'ring_size': 4096
        }
    
    def generate_c_code(self, context: Dict[str, Any], output_file: str) -> None:
        """Generate C implementation"""
        template = self.env.get_template('bitactor_c.j2')
        code = template.render(context)
        
        with open(output_file, 'w') as f:
            f.write(code)
        print(f"✅ Generated C code: {output_file}")
    
    def generate_erlang_code(self, context: Dict[str, Any], output_file: str) -> None:
        """Generate Erlang implementation"""
        template = self.env.get_template('bitactor_erlang.j2')
        code = template.render(context)
        
        with open(output_file, 'w') as f:
            f.write(code)
        print(f"✅ Generated Erlang code: {output_file}")
    
    def generate_python_code(self, context: Dict[str, Any], output_file: str) -> None:
        """Generate Python implementation"""
        template = self.env.get_template('bitactor_python.j2')
        code = template.render(context)
        
        with open(output_file, 'w') as f:
            f.write(code)
        
        # Make executable
        os.chmod(output_file, 0o755)
        print(f"✅ Generated Python code: {output_file}")
    
    def generate_test_code(self, context: Dict[str, Any], output_file: str) -> None:
        """Generate C test code"""
        template = self.env.get_template('bitactor_test_c.j2')
        code = template.render(context)
        
        with open(output_file, 'w') as f:
            f.write(code)
        print(f"✅ Generated C test: {output_file}")
    
    def generate_benchmark_code(self, context: Dict[str, Any], output_file: str) -> None:
        """Generate C benchmark code"""
        template = self.env.get_template('bitactor_benchmark_c.j2')
        code = template.render(context)
        
        with open(output_file, 'w') as f:
            f.write(code)
        print(f"✅ Generated C benchmark: {output_file}")
    
    def generate_makefile(self, context: Dict[str, Any], output_file: str) -> None:
        """Generate Makefile for building and testing"""
        makefile_content = f"""# Makefile for {context['prefix']} BitActor
# Generated by CNS BitActor TTL Generator

CC = gcc
CFLAGS = -O3 -march=native -Wall -Wextra -std=c11 -pthread
LDFLAGS = -lm -lpthread

PREFIX = {context['prefix']}

all: test benchmark

test: $(PREFIX)_test
	./$(PREFIX)_test

benchmark: $(PREFIX)_benchmark
	./$(PREFIX)_benchmark

$(PREFIX)_test: $(PREFIX)_test.c $(PREFIX)_bitactor.h
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

$(PREFIX)_benchmark: $(PREFIX)_benchmark.c $(PREFIX)_bitactor.h
	$(CC) $(CFLAGS) -DBENCHMARK_MODE -o $@ $< $(LDFLAGS)

clean:
	rm -f $(PREFIX)_test $(PREFIX)_benchmark

.PHONY: all test benchmark clean
"""
        with open(output_file, 'w') as f:
            f.write(makefile_content)
        print(f"✅ Generated Makefile: {output_file}")

    def generate_all(self, ttl_file: str, output_dir: str, prefix: str) -> None:
        """Generate all implementations from TTL"""
        # Load TTL
        self.load_ttl(ttl_file)
        
        # Create output directory
        output_path = Path(output_dir)
        output_path.mkdir(parents=True, exist_ok=True)
        
        # Extract ontology name
        ontology_name = Path(ttl_file).stem
        
        # Generate context
        context = self.generate_context(ontology_name, prefix)
        
        # Save context for debugging
        with open(output_path / f"{prefix}_context.json", 'w') as f:
            json.dump(context, f, indent=2)
        
        # Generate all implementations
        self.generate_c_code(context, output_path / f"{prefix}_bitactor.h")
        self.generate_erlang_code(context, output_path / f"{prefix}_bitactor.erl")
        self.generate_python_code(context, output_path / f"{prefix}_bitactor.py")
        
        # Generate tests and benchmarks
        self.generate_test_code(context, output_path / f"{prefix}_test.c")
        self.generate_benchmark_code(context, output_path / f"{prefix}_benchmark.c")
        self.generate_makefile(context, output_path / "Makefile")
        
        print(f"\n🚀 BitActor generation complete!")
        print(f"   Output directory: {output_path}")
        print(f"   Prefix: {prefix}")
        print(f"   Signals: {len(context['signals'])}")
        print(f"   Handlers: {len(context['handlers'])}")
        print(f"\n📝 Next steps:")
        print(f"   cd {output_path}")
        print(f"   make test      # Run unit tests")
        print(f"   make benchmark # Run performance benchmarks")

def main():
    """CLI interface"""
    if len(sys.argv) < 4:
        print("Usage: bitactor_ttl_generator.py <ttl_file> <output_dir> <prefix>")
        print("Example: bitactor_ttl_generator.py ontologies/bitactor_semantic_core.ttl generated/semantic semantic")
        sys.exit(1)
    
    ttl_file = sys.argv[1]
    output_dir = sys.argv[2]
    prefix = sys.argv[3]
    
    generator = BitActorTTLGenerator()
    generator.generate_all(ttl_file, output_dir, prefix)

if __name__ == "__main__":
    main()