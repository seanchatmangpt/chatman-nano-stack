#!/usr/bin/env python3
"""
CNS Aegis Fabric TTL-to-Code Generator
Uses Typer CLI and Jinja2 templates to generate production code from TTL ontologies
NO HANDCODING - All code is generated from semantic definitions
"""

import typer
from typing import List, Dict, Any, Optional
from pathlib import Path
import rdflib
from rdflib import Graph, Namespace, RDF, RDFS
from jinja2 import Environment, FileSystemLoader, Template
import yaml
import json
from datetime import datetime
from enum import Enum

app = typer.Typer(help="CNS Aegis Fabric Code Generator - TTL to Production Code")

# Namespaces
AEGIS = Namespace("http://cns.aegis/fabric#")
THREAT = Namespace("http://cns.aegis/threat#")
RULE = Namespace("http://cns.aegis/rule#")

class OutputFormat(str, Enum):
    erlang = "erlang"
    kubernetes = "kubernetes"
    terraform = "terraform"
    dockerfile = "dockerfile"
    bitactor = "bitactor"
    all = "all"

class AegisGenerator:
    """Main generator class for Aegis Fabric components"""
    
    def __init__(self, ttl_file: Path, template_dir: Path = Path("templates")):
        self.graph = Graph()
        self.graph.parse(ttl_file, format="turtle")
        self.graph.bind("aegis", AEGIS)
        self.graph.bind("threat", THREAT)
        self.graph.bind("rule", RULE)
        
        # Setup Jinja2 environment
        self.env = Environment(
            loader=FileSystemLoader(template_dir),
            trim_blocks=True,
            lstrip_blocks=True
        )
        
        # Extract data from TTL
        self.threats = self._extract_threats()
        self.rules = self._extract_rules()
        self.config = self._extract_config()
        
    def _extract_threats(self) -> List[Dict[str, Any]]:
        """Extract threat signatures from TTL"""
        threats = []
        
        # First get all threat types (subclasses of ThreatSignature)
        threat_types = {}
        for subclass in self.graph.subjects(RDFS.subClassOf, AEGIS.ThreatSignature):
            threat_type_name = str(subclass).split('#')[-1]
            threat_types[subclass] = threat_type_name
        
        # Then get all instances of those threat types
        for threat_type_uri, threat_type_name in threat_types.items():
            for instance in self.graph.subjects(RDF.type, threat_type_uri):
                threat_data = {
                    'name': str(instance).split('#')[-1],
                    'type': threat_type_name,
                    'uri': str(instance),
                    'properties': {}
                }
                
                # Extract all properties
                for pred, obj in self.graph.predicate_objects(instance):
                    if str(pred).startswith(str(AEGIS)):
                        prop_name = str(pred).split('#')[-1]
                        threat_data['properties'][prop_name] = str(obj)
                
                threats.append(threat_data)
        
        return threats
    
    def _extract_rules(self) -> List[Dict[str, Any]]:
        """Extract detection rules from TTL"""
        rules = []
        
        for rule_class in self.graph.subjects(RDFS.subClassOf, RULE.DetectionRule):
            rule_data = {
                'name': str(rule_class).split('#')[-1],
                'uri': str(rule_class),
                'properties': {}
            }
            
            for pred, obj in self.graph.predicate_objects(rule_class):
                if pred.startswith(AEGIS):
                    prop_name = str(pred).split('#')[-1]
                    rule_data['properties'][prop_name] = str(obj)
            
            rules.append(rule_data)
        
        return rules
    
    def _extract_config(self) -> Dict[str, Any]:
        """Extract configuration from TTL"""
        config = {
            'gossip': {},
            'bitactor': {},
            'service_mesh': {},
            'performance': {}
        }
        
        # Extract properties directly from the class definitions
        # Gossip Protocol config
        for pred, obj in self.graph.predicate_objects(AEGIS.GossipProtocol):
            if str(pred).startswith(str(AEGIS)):
                prop_name = str(pred).split('#')[-1]
                config['gossip'][prop_name] = str(obj)
        
        # BitActor config
        for pred, obj in self.graph.predicate_objects(AEGIS.BitActorAgent):
            if str(pred).startswith(str(AEGIS)):
                prop_name = str(pred).split('#')[-1]
                config['bitactor'][prop_name] = str(obj)
        
        # Service Mesh config
        for pred, obj in self.graph.predicate_objects(AEGIS.ServiceMesh):
            if str(pred).startswith(str(AEGIS)):
                prop_name = str(pred).split('#')[-1]
                config['service_mesh'][prop_name] = str(obj)
        
        # Performance targets
        for pred, obj in self.graph.predicate_objects(AEGIS.PerformanceTarget):
            if str(pred).startswith(str(AEGIS)):
                prop_name = str(pred).split('#')[-1]
                config['performance'][prop_name] = str(obj)
        
        return config
    
    def generate_erlang_gossip_protocol(self) -> str:
        """Generate Erlang/OTP gossip protocol module"""
        template = self.env.get_template('erlang_gossip_protocol.erl.j2')
        return template.render(
            threats=self.threats,
            config=self.config,
            timestamp=datetime.now().isoformat()
        )
    
    def generate_kubernetes_manifests(self) -> Dict[str, str]:
        """Generate Kubernetes deployment manifests"""
        manifests = {}
        
        # Generate deployment manifest
        deployment_template = self.env.get_template('k8s_deployment.yaml.j2')
        manifests['deployment.yaml'] = deployment_template.render(
            config=self.config,
            threats=self.threats,
            timestamp=datetime.now().isoformat()
        )
        
        # Generate service manifest
        service_template = self.env.get_template('k8s_service.yaml.j2')
        manifests['service.yaml'] = service_template.render(
            config=self.config,
            timestamp=datetime.now().isoformat()
        )
        
        # Generate ConfigMap for threat signatures
        configmap_template = self.env.get_template('k8s_configmap.yaml.j2')
        manifests['configmap.yaml'] = configmap_template.render(
            threats=self.threats,
            rules=self.rules,
            config=self.config,
            timestamp=datetime.now().isoformat()
        )
        
        return manifests
    
    def generate_terraform_infrastructure(self) -> str:
        """Generate Terraform infrastructure code"""
        template = self.env.get_template('terraform_aegis.tf.j2')
        return template.render(
            config=self.config,
            threats=self.threats,
            timestamp=datetime.now().isoformat()
        )
    
    def generate_dockerfile(self) -> str:
        """Generate Dockerfile for BitActor agents"""
        template = self.env.get_template('Dockerfile.aegis.j2')
        return template.render(
            config=self.config,
            timestamp=datetime.now().isoformat()
        )
    
    def generate_bitactor_rules(self) -> str:
        """Generate BitActor rule bytecode"""
        template = self.env.get_template('bitactor_rules.c.j2')
        return template.render(
            threats=self.threats,
            rules=self.rules,
            config=self.config,
            timestamp=datetime.now().isoformat()
        )

@app.command()
def generate(
    ttl_file: Path = typer.Argument(..., help="Path to cybersecurity_core.ttl"),
    output_dir: Path = typer.Option(Path("generated"), help="Output directory for generated code"),
    format: OutputFormat = typer.Option(OutputFormat.all, help="Output format to generate"),
    template_dir: Path = typer.Option(Path("templates"), help="Directory containing Jinja2 templates")
):
    """Generate production code from TTL definitions"""
    
    # Validate inputs
    if not ttl_file.exists():
        typer.echo(f"Error: TTL file {ttl_file} not found", err=True)
        raise typer.Exit(1)
    
    # Create output directory
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Initialize generator
    typer.echo(f"Loading TTL from {ttl_file}...")
    generator = AegisGenerator(ttl_file, template_dir)
    
    typer.echo(f"Found {len(generator.threats)} threat signatures")
    typer.echo(f"Found {len(generator.rules)} detection rules")
    
    # Generate based on format
    if format in [OutputFormat.erlang, OutputFormat.all]:
        typer.echo("Generating Erlang gossip protocol...")
        erlang_code = generator.generate_erlang_gossip_protocol()
        erlang_file = output_dir / "aegis_gossip_protocol.erl"
        erlang_file.write_text(erlang_code)
        typer.echo(f"✅ Generated: {erlang_file}")
    
    if format in [OutputFormat.kubernetes, OutputFormat.all]:
        typer.echo("Generating Kubernetes manifests...")
        k8s_manifests = generator.generate_kubernetes_manifests()
        k8s_dir = output_dir / "k8s"
        k8s_dir.mkdir(exist_ok=True)
        
        for filename, content in k8s_manifests.items():
            manifest_file = k8s_dir / filename
            manifest_file.write_text(content)
            typer.echo(f"✅ Generated: {manifest_file}")
    
    if format in [OutputFormat.terraform, OutputFormat.all]:
        typer.echo("Generating Terraform infrastructure...")
        terraform_code = generator.generate_terraform_infrastructure()
        terraform_file = output_dir / "aegis_infrastructure.tf"
        terraform_file.write_text(terraform_code)
        typer.echo(f"✅ Generated: {terraform_file}")
    
    if format in [OutputFormat.dockerfile, OutputFormat.all]:
        typer.echo("Generating Dockerfile...")
        dockerfile = generator.generate_dockerfile()
        dockerfile_path = output_dir / "Dockerfile.aegis"
        dockerfile_path.write_text(dockerfile)
        typer.echo(f"✅ Generated: {dockerfile_path}")
    
    if format in [OutputFormat.bitactor, OutputFormat.all]:
        typer.echo("Generating BitActor rules...")
        bitactor_rules = generator.generate_bitactor_rules()
        bitactor_file = output_dir / "aegis_rules.c"
        bitactor_file.write_text(bitactor_rules)
        typer.echo(f"✅ Generated: {bitactor_file}")
    
    typer.echo("✨ Code generation complete!")

@app.command()
def validate(
    ttl_file: Path = typer.Argument(..., help="Path to cybersecurity_core.ttl")
):
    """Validate TTL file syntax and structure"""
    
    try:
        graph = Graph()
        graph.parse(ttl_file, format="turtle")
        
        # Count entities
        threat_count = len(list(graph.subjects(RDF.type, AEGIS.ThreatSignature)))
        rule_count = len(list(graph.subjects(RDF.type, RULE.DetectionRule)))
        
        typer.echo(f"✅ TTL file is valid!")
        typer.echo(f"   Threats: {threat_count}")
        typer.echo(f"   Rules: {rule_count}")
        
    except Exception as e:
        typer.echo(f"❌ TTL validation failed: {e}", err=True)
        raise typer.Exit(1)

@app.command()
def list_threats(
    ttl_file: Path = typer.Argument(..., help="Path to cybersecurity_core.ttl")
):
    """List all threat signatures in the TTL"""
    
    generator = AegisGenerator(ttl_file)
    
    typer.echo("Threat Signatures:")
    typer.echo("-" * 50)
    
    for threat in generator.threats:
        typer.echo(f"\n{threat['name']} ({threat['type']})")
        for prop, value in threat['properties'].items():
            typer.echo(f"  {prop}: {value}")

@app.command()
def ci_pipeline(
    ttl_file: Path = typer.Argument(..., help="Path to cybersecurity_core.ttl"),
    output_dir: Path = typer.Option(Path("generated"), help="Output directory"),
    deploy: bool = typer.Option(False, help="Deploy to Kubernetes after generation")
):
    """Run full CI/CD pipeline: Generate, Build, Test, Deploy"""
    
    typer.echo("🚀 Starting Aegis Fabric CI/CD Pipeline")
    
    # Step 1: Generate all code
    typer.echo("\n📝 Step 1: Code Generation")
    generate(ttl_file, output_dir, OutputFormat.all)
    
    # Step 2: Run OWL compiler
    typer.echo("\n🦉 Step 2: OWL Compilation")
    import subprocess
    owl_result = subprocess.run(
        ["python", "owl_compiler.py", str(ttl_file)],
        capture_output=True,
        text=True
    )
    if owl_result.returncode != 0:
        typer.echo(f"❌ OWL compilation failed: {owl_result.stderr}", err=True)
        raise typer.Exit(1)
    typer.echo("✅ OWL compilation successful")
    
    # Step 3: Build containers
    typer.echo("\n🐳 Step 3: Building Containers")
    docker_result = subprocess.run(
        ["docker", "build", "-f", str(output_dir / "Dockerfile.aegis"), "-t", "aegis-bitactor:latest", "."],
        capture_output=True,
        text=True
    )
    if docker_result.returncode != 0:
        typer.echo(f"❌ Docker build failed: {docker_result.stderr}", err=True)
        raise typer.Exit(1)
    typer.echo("✅ Container build successful")
    
    # Step 4: Deploy if requested
    if deploy:
        typer.echo("\n☸️ Step 4: Kubernetes Deployment")
        for manifest in (output_dir / "k8s").glob("*.yaml"):
            kubectl_result = subprocess.run(
                ["kubectl", "apply", "-f", str(manifest)],
                capture_output=True,
                text=True
            )
            if kubectl_result.returncode != 0:
                typer.echo(f"❌ Deployment failed: {kubectl_result.stderr}", err=True)
                raise typer.Exit(1)
            typer.echo(f"✅ Applied: {manifest.name}")
    
    typer.echo("\n✨ CI/CD Pipeline Complete!")

if __name__ == "__main__":
    app()