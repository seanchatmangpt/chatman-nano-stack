#!/usr/bin/env python3
"""
TTL to Ash Generator - Combines TTL ontology parsing with Ash resource generation
Generates Mix tasks to create Ash resources and domains from TTL definitions
"""

import re
import sys
import argparse
from pathlib import Path
from typing import Dict, List, Tuple, Set, Optional
from dataclasses import dataclass, field
from collections import defaultdict


@dataclass
class OntologyClass:
    """Represents a class in the ontology"""
    uri: str
    name: str
    label: Optional[str] = None
    comment: Optional[str] = None
    properties: List['OntologyProperty'] = field(default_factory=list)
    parent_classes: List[str] = field(default_factory=list)
    relationships: List['OntologyRelationship'] = field(default_factory=list)


@dataclass
class OntologyProperty:
    """Represents a property in the ontology"""
    uri: str
    name: str
    domain: Optional[str] = None
    range: Optional[str] = None
    label: Optional[str] = None
    comment: Optional[str] = None
    is_functional: bool = False
    is_required: bool = False
    cardinality: Optional[str] = None


@dataclass
class OntologyRelationship:
    """Represents a relationship between classes"""
    source: str
    target: str
    property: str
    relationship_type: str  # has_many, has_one, belongs_to, many_to_many


class TTLParser:
    """Parse TTL ontology files"""
    
    def __init__(self):
        self.prefixes: Dict[str, str] = {}
        self.classes: Dict[str, OntologyClass] = {}
        self.properties: Dict[str, OntologyProperty] = {}
        self.relationships: List[OntologyRelationship] = []
    
    def parse(self, ttl_content: str) -> Dict:
        """Parse TTL content and extract ontology structure"""
        lines = ttl_content.strip().split('\n')
        
        # Parse prefixes
        for line in lines:
            if line.strip().startswith('@prefix'):
                self._parse_prefix(line)
        
        # Parse classes
        self._parse_classes(ttl_content)
        
        # Parse properties
        self._parse_properties(ttl_content)
        
        # Parse relationships
        self._parse_relationships(ttl_content)
        
        return {
            'prefixes': self.prefixes,
            'classes': self.classes,
            'properties': self.properties,
            'relationships': self.relationships
        }
    
    def _parse_prefix(self, line: str):
        """Parse a prefix declaration"""
        match = re.match(r'@prefix\s+(\w+):\s+<([^>]+)>', line)
        if match:
            prefix, uri = match.groups()
            self.prefixes[prefix] = uri
    
    def _parse_classes(self, content: str):
        """Parse class definitions"""
        # Find class declarations
        class_pattern = r'(\w+:\w+)\s+(?:rdf:type|a)\s+(?:owl:|rdfs:)Class'
        for match in re.finditer(class_pattern, content):
            class_uri = match.group(1)
            class_name = self._extract_local_name(class_uri)
            
            if class_uri not in self.classes:
                self.classes[class_uri] = OntologyClass(
                    uri=class_uri,
                    name=class_name
                )
        
        # Parse class metadata
        for class_uri, ont_class in self.classes.items():
            # Extract label
            label_pattern = rf'{class_uri}\s+rdfs:label\s+"([^"]+)"'
            label_match = re.search(label_pattern, content)
            if label_match:
                ont_class.label = label_match.group(1)
            
            # Extract comment
            comment_pattern = rf'{class_uri}\s+rdfs:comment\s+"([^"]+)"'
            comment_match = re.search(comment_pattern, content)
            if comment_match:
                ont_class.comment = comment_match.group(1)
            
            # Extract parent classes
            parent_pattern = rf'{class_uri}\s+rdfs:subClassOf\s+(\w+:\w+)'
            for parent_match in re.finditer(parent_pattern, content):
                parent_uri = parent_match.group(1)
                ont_class.parent_classes.append(parent_uri)
    
    def _parse_properties(self, content: str):
        """Parse property definitions"""
        # Object properties
        obj_prop_pattern = r'(\w+:\w+)\s+(?:rdf:type|a)\s+owl:ObjectProperty'
        for match in re.finditer(obj_prop_pattern, content):
            prop_uri = match.group(1)
            prop_name = self._extract_local_name(prop_uri)
            
            if prop_uri not in self.properties:
                self.properties[prop_uri] = OntologyProperty(
                    uri=prop_uri,
                    name=prop_name
                )
        
        # Data properties
        data_prop_pattern = r'(\w+:\w+)\s+(?:rdf:type|a)\s+owl:DatatypeProperty'
        for match in re.finditer(data_prop_pattern, content):
            prop_uri = match.group(1)
            prop_name = self._extract_local_name(prop_uri)
            
            if prop_uri not in self.properties:
                self.properties[prop_uri] = OntologyProperty(
                    uri=prop_uri,
                    name=prop_name
                )
        
        # Parse property metadata
        for prop_uri, prop in self.properties.items():
            # Extract domain
            domain_pattern = rf'{prop_uri}\s+rdfs:domain\s+(\w+:\w+)'
            domain_match = re.search(domain_pattern, content)
            if domain_match:
                prop.domain = domain_match.group(1)
            
            # Extract range
            range_pattern = rf'{prop_uri}\s+rdfs:range\s+(\w+:\w+)'
            range_match = re.search(range_pattern, content)
            if range_match:
                prop.range = range_match.group(1)
            
            # Check if functional
            if re.search(rf'{prop_uri}\s+(?:rdf:type|a)\s+owl:FunctionalProperty', content):
                prop.is_functional = True
    
    def _parse_relationships(self, content: str):
        """Parse relationships between classes"""
        for prop_uri, prop in self.properties.items():
            if prop.domain and prop.range and prop.range in self.classes:
                # Determine relationship type
                if prop.is_functional:
                    rel_type = "belongs_to"
                else:
                    rel_type = "has_many"
                
                relationship = OntologyRelationship(
                    source=prop.domain,
                    target=prop.range,
                    property=prop_uri,
                    relationship_type=rel_type
                )
                self.relationships.append(relationship)
    
    def _extract_local_name(self, uri: str) -> str:
        """Extract the local name from a URI"""
        if ':' in uri:
            return uri.split(':')[1]
        return uri


class AshGenerator:
    """Generate Ash resources and domains from parsed ontology"""
    
    def __init__(self, app_name: str = "my_app"):
        self.app_name = app_name
        self.domain_mappings: Dict[str, str] = {}
    
    def generate_from_ontology(self, ontology: Dict) -> Dict[str, str]:
        """Generate Ash code from parsed ontology"""
        generated_files = {}
        
        # Group classes by domain
        domains = self._group_classes_by_domain(ontology['classes'])
        
        # Generate domain modules
        for domain_name, classes in domains.items():
            domain_content = self._generate_domain(domain_name, classes)
            generated_files[f"lib/{self.app_name}/{domain_name}/domain.ex"] = domain_content
        
        # Generate resources
        for class_uri, ont_class in ontology['classes'].items():
            resource_content = self._generate_resource(
                ont_class,
                ontology['properties'],
                ontology['relationships']
            )
            module_path = self._class_to_module_path(ont_class)
            generated_files[f"lib/{self.app_name}/{module_path}.ex"] = resource_content
        
        # Generate mix task script
        mix_task_script = self._generate_mix_task_script(
            ontology['classes'],
            ontology['properties'],
            ontology['relationships']
        )
        generated_files["generate_ash_resources.sh"] = mix_task_script
        
        return generated_files
    
    def _group_classes_by_domain(self, classes: Dict[str, OntologyClass]) -> Dict[str, List[OntologyClass]]:
        """Group classes into logical domains"""
        domains = defaultdict(list)
        
        for class_uri, ont_class in classes.items():
            # Simple domain assignment based on class name patterns
            if any(keyword in ont_class.name.lower() for keyword in ['user', 'account', 'auth']):
                domain = 'accounts'
            elif any(keyword in ont_class.name.lower() for keyword in ['threat', 'attack', 'vulnerability']):
                domain = 'threats'
            elif any(keyword in ont_class.name.lower() for keyword in ['asset', 'resource']):
                domain = 'assets'
            elif any(keyword in ont_class.name.lower() for keyword in ['control', 'security']):
                domain = 'controls'
            else:
                domain = 'core'
            
            domains[domain].append(ont_class)
            self.domain_mappings[class_uri] = domain
        
        return domains
    
    def _generate_domain(self, domain_name: str, classes: List[OntologyClass]) -> str:
        """Generate an Ash domain module"""
        module_name = f"{self._to_module_name(self.app_name)}.{self._to_module_name(domain_name)}"
        
        resources = []
        for ont_class in classes:
            resource_module = self._class_to_module_name(ont_class)
            resources.append(f"    resource {resource_module}")
        
        return f"""defmodule {module_name} do
  use Ash.Domain,
    otp_app: :{self.app_name}

  resources do
{chr(10).join(resources)}
  end
end
"""
    
    def _generate_resource(
        self,
        ont_class: OntologyClass,
        properties: Dict[str, OntologyProperty],
        relationships: List[OntologyRelationship]
    ) -> str:
        """Generate an Ash resource module"""
        module_name = self._class_to_module_name(ont_class)
        domain_name = self.domain_mappings.get(ont_class.uri, 'core')
        domain_module = f"{self._to_module_name(self.app_name)}.{self._to_module_name(domain_name)}"
        
        # Generate attributes
        attributes = self._generate_attributes(ont_class, properties)
        
        # Generate relationships
        rels = self._generate_relationships(ont_class, relationships)
        
        # Generate actions
        actions = self._generate_actions(ont_class)
        
        return f"""defmodule {module_name} do
  use Ash.Resource,
    otp_app: :{self.app_name},
    domain: {domain_module}

  attributes do
    uuid_primary_key :id
    
{attributes}
    
    timestamps()
  end

{rels}

  actions do
    defaults [:read, :destroy]
    
    create :create do
      primary? true
    end
    
    update :update do
      primary? true
    end
  end
end
"""
    
    def _generate_attributes(self, ont_class: OntologyClass, properties: Dict[str, OntologyProperty]) -> str:
        """Generate attribute definitions for a resource"""
        attributes = []
        
        # Add properties that have this class as domain
        for prop_uri, prop in properties.items():
            if prop.domain == ont_class.uri and prop.range:
                # Skip object properties (they become relationships)
                if prop.range in self.domain_mappings:
                    continue
                
                # Map RDF types to Ash types
                ash_type = self._map_rdf_to_ash_type(prop.range)
                
                attr_def = f"    attribute :{self._to_snake_case(prop.name)}, :{ash_type}"
                
                # Add modifiers
                modifiers = []
                if prop.is_required:
                    modifiers.append("allow_nil? false")
                if prop.label:
                    modifiers.append(f'description "{prop.label}"')
                
                if modifiers:
                    attr_def += " do\n"
                    for mod in modifiers:
                        attr_def += f"      {mod}\n"
                    attr_def += "    end"
                
                attributes.append(attr_def)
        
        # Add default attributes based on class name
        if 'name' not in [self._to_snake_case(p.name) for p in properties.values()]:
            attributes.append("    attribute :name, :string, allow_nil?: false")
        
        if ont_class.comment:
            attributes.append(f'    attribute :description, :string, default: "{ont_class.comment}"')
        
        return "\n".join(attributes)
    
    def _generate_relationships(self, ont_class: OntologyClass, relationships: List[OntologyRelationship]) -> str:
        """Generate relationship definitions for a resource"""
        if not relationships:
            return ""
        
        rels = ["  relationships do"]
        
        # Outgoing relationships (where this class is the source)
        for rel in relationships:
            if rel.source == ont_class.uri:
                target_module = self._uri_to_module_name(rel.target)
                rel_name = self._to_snake_case(self._extract_local_name(rel.property))
                
                if rel.relationship_type == "belongs_to":
                    rels.append(f"    belongs_to :{rel_name}, {target_module}")
                elif rel.relationship_type == "has_many":
                    rels.append(f"    has_many :{rel_name}, {target_module}")
                elif rel.relationship_type == "has_one":
                    rels.append(f"    has_one :{rel_name}, {target_module}")
        
        # Incoming relationships (where this class is the target)
        for rel in relationships:
            if rel.target == ont_class.uri:
                source_module = self._uri_to_module_name(rel.source)
                rel_name = self._to_snake_case(self._extract_local_name(rel.source))
                
                if rel.relationship_type == "belongs_to":
                    # Reverse of belongs_to is has_many
                    rels.append(f"    has_many :{rel_name}s, {source_module}")
        
        rels.append("  end")
        
        return "\n".join(rels) if len(rels) > 2 else ""
    
    def _generate_actions(self, ont_class: OntologyClass) -> str:
        """Generate action definitions based on class characteristics"""
        # This is a placeholder - could be expanded based on ontology annotations
        return ""
    
    def _generate_mix_task_script(
        self,
        classes: Dict[str, OntologyClass],
        properties: Dict[str, OntologyProperty],
        relationships: List[OntologyRelationship]
    ) -> str:
        """Generate a shell script with mix tasks to create all resources"""
        script = ["#!/bin/bash", "", "# Generated Ash resource creation script", ""]
        
        # First create domains
        domains = set(self.domain_mappings.values())
        for domain in sorted(domains):
            domain_module = f"{self._to_module_name(self.app_name)}.{self._to_module_name(domain)}"
            script.append(f'echo "Creating domain {domain_module}..."')
            script.append(f'mix ash.gen.domain {domain_module}')
            script.append("")
        
        # Then create resources
        for class_uri, ont_class in classes.items():
            module_name = self._class_to_module_name(ont_class)
            domain = self.domain_mappings.get(class_uri, 'core')
            domain_module = f"{self._to_module_name(self.app_name)}.{self._to_module_name(domain)}"
            
            cmd = f'mix ash.gen.resource {module_name}'
            
            # Add domain
            cmd += f' --domain {domain_module}'
            
            # Add default actions
            cmd += ' --default-actions read,create,update,destroy'
            
            # Add attributes
            for prop_uri, prop in properties.items():
                if prop.domain == class_uri and prop.range:
                    # Skip object properties
                    if prop.range in self.domain_mappings:
                        continue
                    
                    ash_type = self._map_rdf_to_ash_type(prop.range)
                    attr_name = self._to_snake_case(prop.name)
                    
                    modifiers = []
                    if prop.is_required:
                        modifiers.append("required")
                    modifiers.append("public")
                    
                    modifier_str = ":".join(modifiers) if modifiers else ""
                    cmd += f' --attribute {attr_name}:{ash_type}:{modifier_str}'
            
            # Add relationships
            for rel in relationships:
                if rel.source == class_uri:
                    target_module = self._uri_to_module_name(rel.target)
                    rel_name = self._to_snake_case(self._extract_local_name(rel.property))
                    
                    if rel.relationship_type == "belongs_to":
                        cmd += f' --relationship belongs_to:{rel_name}:{target_module}:required'
                    elif rel.relationship_type == "has_many":
                        cmd += f' --relationship has_many:{rel_name}:{target_module}'
            
            # Add timestamps
            cmd += ' --timestamps'
            
            script.append(f'echo "Creating resource {module_name}..."')
            script.append(cmd)
            script.append("")
        
        return "\n".join(script)
    
    def _map_rdf_to_ash_type(self, rdf_type: str) -> str:
        """Map RDF/OWL types to Ash types"""
        type_mappings = {
            'xsd:string': 'string',
            'xsd:integer': 'integer',
            'xsd:int': 'integer',
            'xsd:boolean': 'boolean',
            'xsd:dateTime': 'utc_datetime',
            'xsd:date': 'date',
            'xsd:float': 'float',
            'xsd:double': 'float',
            'xsd:decimal': 'decimal',
            'rdfs:Literal': 'string',
        }
        
        return type_mappings.get(rdf_type, 'string')
    
    def _class_to_module_name(self, ont_class: OntologyClass) -> str:
        """Convert an ontology class to an Elixir module name"""
        domain = self.domain_mappings.get(ont_class.uri, 'core')
        return f"{self._to_module_name(self.app_name)}.{self._to_module_name(domain)}.{self._to_module_name(ont_class.name)}"
    
    def _uri_to_module_name(self, uri: str) -> str:
        """Convert a URI to an Elixir module name"""
        local_name = self._extract_local_name(uri)
        domain = self.domain_mappings.get(uri, 'core')
        return f"{self._to_module_name(self.app_name)}.{self._to_module_name(domain)}.{self._to_module_name(local_name)}"
    
    def _class_to_module_path(self, ont_class: OntologyClass) -> str:
        """Convert an ontology class to a file path"""
        domain = self.domain_mappings.get(ont_class.uri, 'core')
        return f"{domain}/{self._to_snake_case(ont_class.name)}"
    
    def _to_module_name(self, name: str) -> str:
        """Convert a string to CamelCase module name"""
        # Handle already camelized names
        if name[0].isupper():
            return name
        
        # Convert snake_case to CamelCase
        parts = name.split('_')
        return ''.join(part.capitalize() for part in parts)
    
    def _to_snake_case(self, name: str) -> str:
        """Convert a string to snake_case"""
        # Insert underscores before uppercase letters
        s1 = re.sub('(.)([A-Z][a-z]+)', r'\1_\2', name)
        return re.sub('([a-z0-9])([A-Z])', r'\1_\2', s1).lower()
    
    def _extract_local_name(self, uri: str) -> str:
        """Extract the local name from a URI"""
        if ':' in uri:
            return uri.split(':')[1]
        return uri


def main():
    parser = argparse.ArgumentParser(
        description='Generate Ash resources and domains from TTL ontology files'
    )
    parser.add_argument('ttl_file', help='Path to TTL ontology file')
    parser.add_argument(
        '--app-name',
        default='my_app',
        help='Elixir application name (default: my_app)'
    )
    parser.add_argument(
        '--output-dir',
        default='generated',
        help='Output directory for generated files (default: generated)'
    )
    
    args = parser.parse_args()
    
    # Read TTL file
    ttl_path = Path(args.ttl_file)
    if not ttl_path.exists():
        print(f"Error: TTL file not found: {ttl_path}")
        sys.exit(1)
    
    ttl_content = ttl_path.read_text()
    
    # Parse TTL
    parser = TTLParser()
    ontology = parser.parse(ttl_content)
    
    print(f"Parsed {len(ontology['classes'])} classes and {len(ontology['properties'])} properties")
    
    # Generate Ash code
    generator = AshGenerator(args.app_name)
    generated_files = generator.generate_from_ontology(ontology)
    
    # Write generated files
    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    
    for file_path, content in generated_files.items():
        full_path = output_dir / file_path
        full_path.parent.mkdir(parents=True, exist_ok=True)
        full_path.write_text(content)
        
        # Make shell scripts executable
        if file_path.endswith('.sh'):
            full_path.chmod(0o755)
        
        print(f"Generated: {full_path}")
    
    print(f"\nGeneration complete! Files written to: {output_dir}")
    print(f"\nTo create the Ash resources, run:")
    print(f"  cd {output_dir} && ./generate_ash_resources.sh")


if __name__ == '__main__':
    main()