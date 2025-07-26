#!/usr/bin/env python3
"""
🏗️ ULTRATHINK 80/20 Code Generator CLI
Python Typer CLI for generating components across all technologies in the stack
"""

import typer
import os
import json
import yaml
from pathlib import Path
from typing import Optional, Dict, List
from rich.console import Console
from rich.prompt import Prompt, Confirm
from rich.table import Table
from rich.panel import Panel
from rich.tree import Tree
import jinja2
from datetime import datetime

app = typer.Typer(
    name="generate",
    help="🏗️ ULTRATHINK 80/20 Code Generator - Create components across the entire stack",
    rich_markup_mode="rich"
)

console = Console()

# Generator templates and configurations
GENERATORS = {
    "ash_resource": {
        "name": "Ash Resource",
        "description": "Generate Ash Framework resource with actions and policies",
        "template_dir": "templates/ash",
        "output_dir": "generated_cybersecurity_project/lib/cybersecurity/resources",
        "file_extension": ".ex",
        "dependencies": ["ash", "cybersecurity_domain"]
    },
    "reactor_workflow": {
        "name": "Reactor Workflow", 
        "description": "Generate Ash Reactor workflow with steps and compensation",
        "template_dir": "templates/reactor",
        "output_dir": "generated_cybersecurity_project/lib/cybersecurity/workflows",
        "file_extension": ".ex",
        "dependencies": ["ash_reactor", "ash_resources"]
    },
    "nuxt_component": {
        "name": "Nuxt.js Component",
        "description": "Generate Vue component for Nuxt.js (NO TypeScript)",
        "template_dir": "templates/nuxt",
        "output_dir": "dashboard_80_20/components",
        "file_extension": ".vue",
        "dependencies": ["nuxt", "vue"]
    },
    "nuxt_page": {
        "name": "Nuxt.js Page",
        "description": "Generate Nuxt.js page with layout and SEO",
        "template_dir": "templates/nuxt/pages",
        "output_dir": "dashboard_80_20/pages",
        "file_extension": ".vue",
        "dependencies": ["nuxt", "vue"]
    },
    "channel_handler": {
        "name": "Phoenix Channel Handler",
        "description": "Generate ChannelHandler with routing and plugs",
        "template_dir": "templates/channels",
        "output_dir": "lib/cns_forge_web/handlers",
        "file_extension": ".ex",
        "dependencies": ["phoenix", "channel_handler"]
    },
    "bitactor": {
        "name": "BitActor",
        "description": "Generate distributed BitActor with coordination",
        "template_dir": "templates/bitactor",
        "output_dir": "lib/cns_forge/bitactor",
        "file_extension": ".ex",
        "dependencies": ["bitactor", "distributed_erlang"]
    },
    "cli_command": {
        "name": "Python CLI Command",
        "description": "Generate Typer CLI command with rich output",
        "template_dir": "templates/cli",
        "output_dir": "cli_commands",
        "file_extension": ".py",
        "dependencies": ["typer", "rich"]
    },
    "k8s_manifest": {
        "name": "Kubernetes Manifest",
        "description": "Generate K8s deployment, service, and ingress",
        "template_dir": "templates/k8s",
        "output_dir": "k8s",
        "file_extension": ".yaml",
        "dependencies": ["kubernetes"]
    },
    "dspy_signature": {
        "name": "DSPy Signature",
        "description": "Generate DSPy signature from ontology",
        "template_dir": "templates/dspy",
        "output_dir": "dspy_signatures",
        "file_extension": ".py",
        "dependencies": ["dspy", "ttl_ontology"]
    },
    "erlang_gen_server": {
        "name": "Erlang GenServer",
        "description": "Generate OTP GenServer with supervision",
        "template_dir": "templates/erlang",
        "output_dir": "erlang_modules",
        "file_extension": ".erl",
        "dependencies": ["erlang_otp"]
    }
}

# Template configurations
TEMPLATE_VARS = {
    "author": "ULTRATHINK 80/20 Generator",
    "license": "Apache 2.0",
    "generated_at": datetime.now().isoformat(),
    "framework_version": "1.0.0",
    "pipeline_stage": "typer"
}

class CodeGenerator:
    def __init__(self):
        self.template_env = jinja2.Environment(
            loader=jinja2.FileSystemLoader('templates'),
            autoescape=jinja2.select_autoescape(['html', 'xml'])
        )
        
    def generate(self, generator_type: str, name: str, options: Dict = None):
        """Generate code using specified generator"""
        
        if generator_type not in GENERATORS:
            console.print(f"❌ Unknown generator: {generator_type}")
            return False
            
        config = GENERATORS[generator_type]
        console.print(f"🏗️ Generating {config['name']}: {name}")
        
        # Gather template variables
        template_vars = self.build_template_vars(generator_type, name, options or {})
        
        # Create output directory
        output_dir = Path(config["output_dir"])
        output_dir.mkdir(parents=True, exist_ok=True)
        
        # Generate files
        success = self.generate_from_templates(config, name, template_vars)
        
        if success:
            console.print(f"✅ Generated {config['name']}: {name}")
            self.show_generated_files(config, name)
            self.show_next_steps(generator_type, name)
        else:
            console.print(f"❌ Failed to generate {config['name']}: {name}")
            
        return success
    
    def build_template_vars(self, generator_type: str, name: str, options: Dict) -> Dict:
        """Build template variables for generation"""
        
        vars = TEMPLATE_VARS.copy()
        vars.update({
            "name": name,
            "module_name": self.to_module_name(name),
            "class_name": self.to_class_name(name),
            "snake_case": self.to_snake_case(name),
            "kebab_case": self.to_kebab_case(name),
            "generator_type": generator_type,
            "options": options
        })
        
        # Generator-specific variables
        if generator_type == "ash_resource":
            vars.update(self.build_ash_resource_vars(name, options))
        elif generator_type == "reactor_workflow":
            vars.update(self.build_reactor_workflow_vars(name, options))
        elif generator_type == "nuxt_component":
            vars.update(self.build_nuxt_component_vars(name, options))
        elif generator_type == "channel_handler":
            vars.update(self.build_channel_handler_vars(name, options))
        elif generator_type == "bitactor":
            vars.update(self.build_bitactor_vars(name, options))
        elif generator_type == "k8s_manifest":
            vars.update(self.build_k8s_vars(name, options))
        elif generator_type == "dspy_signature":
            vars.update(self.build_dspy_vars(name, options))
        elif generator_type == "erlang_gen_server":
            vars.update(self.build_erlang_vars(name, options))
        
        return vars
    
    def generate_from_templates(self, config: Dict, name: str, template_vars: Dict) -> bool:
        """Generate files from templates"""
        
        template_dir = Path(config["template_dir"])
        if not template_dir.exists():
            # Create default template if not exists
            self.create_default_template(config, template_dir)
        
        output_dir = Path(config["output_dir"])
        file_extension = config["file_extension"]
        
        try:
            # Single file generation for now
            template_file = template_dir / f"template{file_extension}.j2"
            if template_file.exists():
                template = self.template_env.get_template(str(template_file))
                content = template.render(**template_vars)
                
                output_file = output_dir / f"{self.to_snake_case(name)}{file_extension}"
                output_file.write_text(content)
                
                return True
            else:
                # Generate minimal template
                content = self.generate_minimal_template(config, template_vars)
                output_file = output_dir / f"{self.to_snake_case(name)}{file_extension}"
                output_file.write_text(content)
                return True
                
        except Exception as e:
            console.print(f"❌ Template generation error: {e}")
            return False
    
    def create_default_template(self, config: Dict, template_dir: Path):
        """Create default template if none exists"""
        template_dir.mkdir(parents=True, exist_ok=True)
        
        file_extension = config["file_extension"]
        template_file = template_dir / f"template{file_extension}.j2"
        
        # Create basic template based on file type
        if file_extension == ".ex":
            template_content = self.get_elixir_template()
        elif file_extension == ".vue":
            template_content = self.get_vue_template()
        elif file_extension == ".py":
            template_content = self.get_python_template()
        elif file_extension == ".yaml":
            template_content = self.get_yaml_template()
        elif file_extension == ".erl":
            template_content = self.get_erlang_template()
        else:
            template_content = "# Generated by ULTRATHINK 80/20\n# {{ name }}\n"
        
        template_file.write_text(template_content)
    
    def generate_minimal_template(self, config: Dict, template_vars: Dict) -> str:
        """Generate minimal template content"""
        
        file_extension = config["file_extension"]
        
        if file_extension == ".ex":
            return self.render_elixir_template(template_vars)
        elif file_extension == ".vue":
            return self.render_vue_template(template_vars)
        elif file_extension == ".py":
            return self.render_python_template(template_vars)
        elif file_extension == ".yaml":
            return self.render_yaml_template(template_vars)
        elif file_extension == ".erl":
            return self.render_erlang_template(template_vars)
        else:
            return f"# Generated by ULTRATHINK 80/20\n# {template_vars['name']}\n"
    
    # Template variable builders
    
    def build_ash_resource_vars(self, name: str, options: Dict) -> Dict:
        return {
            "resource_name": self.to_class_name(name),
            "table_name": self.to_snake_case(name) + "s",
            "attributes": options.get("attributes", ["name:string", "description:string"]),
            "actions": options.get("actions", ["create", "read", "update", "destroy"]),
            "domain": options.get("domain", "Cybersecurity")
        }
    
    def build_reactor_workflow_vars(self, name: str, options: Dict) -> Dict:
        return {
            "workflow_name": self.to_class_name(name) + "Workflow",
            "steps": options.get("steps", ["step1", "step2", "step3"]),
            "inputs": options.get("inputs", ["input"]),
            "outputs": options.get("outputs", ["result"]),
            "timeout": options.get("timeout", 300)
        }
    
    def build_nuxt_component_vars(self, name: str, options: Dict) -> Dict:
        return {
            "component_name": self.to_class_name(name),
            "props": options.get("props", []),
            "emits": options.get("emits", []),
            "composables": options.get("composables", []),
            "scoped": options.get("scoped", True)
        }
    
    def build_channel_handler_vars(self, name: str, options: Dict) -> Dict:
        return {
            "handler_name": self.to_class_name(name) + "Handler",
            "events": options.get("events", ["create", "update", "delete"]),
            "plugs": options.get("plugs", ["EnsureAuthenticated"]),
            "scopes": options.get("scopes", [])
        }
    
    def build_bitactor_vars(self, name: str, options: Dict) -> Dict:
        return {
            "actor_name": self.to_class_name(name) + "Actor",
            "capabilities": options.get("capabilities", ["process", "coordinate"]),
            "state_fields": options.get("state_fields", ["status", "data"]),
            "message_types": options.get("message_types", ["process", "status"])
        }
    
    def build_k8s_vars(self, name: str, options: Dict) -> Dict:
        return {
            "app_name": self.to_kebab_case(name),
            "namespace": options.get("namespace", "cns-forge"),
            "replicas": options.get("replicas", 3),
            "port": options.get("port", 4000),
            "image": options.get("image", f"cns-forge/{self.to_kebab_case(name)}:latest")
        }
    
    def build_dspy_vars(self, name: str, options: Dict) -> Dict:
        return {
            "signature_name": self.to_class_name(name) + "Signature",
            "input_fields": options.get("input_fields", ["query:str"]),
            "output_fields": options.get("output_fields", ["result:str"]),
            "description": options.get("description", f"DSPy signature for {name}")
        }
    
    def build_erlang_vars(self, name: str, options: Dict) -> Dict:
        return {
            "module_name": self.to_snake_case(name),
            "callbacks": options.get("callbacks", ["init", "handle_call", "handle_cast"]),
            "state_type": options.get("state_type", "map"),
            "supervision": options.get("supervision", "one_for_one")
        }
    
    # Template renderers
    
    def render_elixir_template(self, vars: Dict) -> str:
        return f'''defmodule CnsForge.{vars["class_name"]} do
  @moduledoc """
  {vars["name"]} - Generated by ULTRATHINK 80/20
  Generated at: {vars["generated_at"]}
  """
  
  use GenServer
  require Logger
  
  def start_link(opts \\\\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  def init(opts) do
    Logger.info("Starting {vars["name"]}")
    {{:ok, %{{status: :initialized, opts: opts}}}}
  end
  
  def handle_call(:status, _from, state) do
    {{:reply, state.status, state}}
  end
  
  def handle_cast({{:update, data}}, state) do
    {{:noreply, Map.put(state, :data, data)}}
  end
end'''
    
    def render_vue_template(self, vars: Dict) -> str:
        return f'''<template>
  <div class="{vars["kebab_case"]}-component">
    <h2>{{ title }}</h2>
    <p>{{ description }}</p>
    <!-- Generated by ULTRATHINK 80/20 - NO TypeScript -->
  </div>
</template>

<script setup>
// {vars["name"]} Component
// Generated at: {vars["generated_at"]}

const title = ref('{vars["class_name"]}')
const description = ref('Generated component for {vars["name"]}')

// Component logic here
onMounted(() => {{
  console.log('{vars["class_name"]} component mounted')
}})
</script>

<style scoped>
.{vars["kebab_case"]}-component {{
  padding: 1rem;
  border: 1px solid #e2e8f0;
  border-radius: 0.5rem;
}}
</style>'''
    
    def render_python_template(self, vars: Dict) -> str:
        return f'''#!/usr/bin/env python3
"""
{vars["name"]} - Generated by ULTRATHINK 80/20
Generated at: {vars["generated_at"]}
"""

import typer
from rich.console import Console

app = typer.Typer(name="{vars["snake_case"]}")
console = Console()

@app.command()
def main():
    """Main command for {vars["name"]}"""
    console.print("🚀 {vars["name"]} - ULTRATHINK 80/20")

if __name__ == "__main__":
    app()'''
    
    def render_yaml_template(self, vars: Dict) -> str:
        return f'''# {vars["name"]} - Generated by ULTRATHINK 80/20
# Generated at: {vars["generated_at"]}

apiVersion: apps/v1
kind: Deployment
metadata:
  name: {vars.get("app_name", vars["kebab_case"])}
  labels:
    app: {vars.get("app_name", vars["kebab_case"])}
    generated-by: ultrathink-80-20
spec:
  replicas: {vars.get("replicas", 3)}
  selector:
    matchLabels:
      app: {vars.get("app_name", vars["kebab_case"])}
  template:
    metadata:
      labels:
        app: {vars.get("app_name", vars["kebab_case"])}
    spec:
      containers:
      - name: {vars.get("app_name", vars["kebab_case"])}
        image: {vars.get("image", f"cns-forge/{vars['kebab_case']}:latest")}
        ports:
        - containerPort: {vars.get("port", 4000)}'''
    
    def render_erlang_template(self, vars: Dict) -> str:
        return f'''-module({vars["snake_case"]}).
-behaviour(gen_server).

%% {vars["name"]} - Generated by ULTRATHINK 80/20
%% Generated at: {vars["generated_at"]}

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {{
    status = initialized,
    data = #{{}}
}}).

start_link() ->
    gen_server:start_link({{local, ?MODULE}}, ?MODULE, [], []).

init([]) ->
    {{ok, #state{{}}}}.

handle_call(status, _From, State) ->
    {{reply, State#state.status, State}}.

handle_cast({{update, Data}}, State) ->
    {{noreply, State#state{{data = Data}}}}.

handle_info(_Info, State) ->
    {{noreply, State}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {{ok, State}}.'''
    
    # Utility functions
    
    def to_module_name(self, name: str) -> str:
        return name.replace("-", "_").replace(" ", "_")
    
    def to_class_name(self, name: str) -> str:
        return "".join(word.capitalize() for word in name.replace("-", " ").replace("_", " ").split())
    
    def to_snake_case(self, name: str) -> str:
        return name.lower().replace("-", "_").replace(" ", "_")
    
    def to_kebab_case(self, name: str) -> str:
        return name.lower().replace("_", "-").replace(" ", "-")
    
    def show_generated_files(self, config: Dict, name: str):
        """Show files that were generated"""
        output_dir = Path(config["output_dir"])
        file_extension = config["file_extension"]
        output_file = output_dir / f"{self.to_snake_case(name)}{file_extension}"
        
        console.print(f"\n📁 Generated files:")
        console.print(f"  • {output_file}")
    
    def show_next_steps(self, generator_type: str, name: str):
        """Show next steps after generation"""
        console.print(f"\n🔄 Next steps for {name}:")
        
        if generator_type == "ash_resource":
            console.print("  1. Add to domain configuration")
            console.print("  2. Run: mix ash.migrate")
            console.print("  3. Add to API endpoints")
        elif generator_type == "reactor_workflow":
            console.print("  1. Define workflow steps")
            console.print("  2. Add to main workflow")
            console.print("  3. Test execution")
        elif generator_type == "nuxt_component":
            console.print("  1. Import in parent component")
            console.print("  2. Add to component library")
            console.print("  3. Test in browser")
        elif generator_type == "channel_handler":
            console.print("  1. Add to router")
            console.print("  2. Configure channel topic")
            console.print("  3. Test WebSocket connection")
        elif generator_type == "k8s_manifest":
            console.print("  1. Review configuration")
            console.print("  2. kubectl apply -f <file>")
            console.print("  3. Monitor deployment")

# Template content getters (for creating default templates)

    def get_elixir_template(self) -> str:
        return '''defmodule CnsForge.{{ class_name }} do
  @moduledoc """
  {{ name }} - Generated by ULTRATHINK 80/20
  {{ description | default("Generated module") }}
  """
  
  use GenServer
  require Logger
  
  def start_link(opts \\\\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  def init(opts) do
    Logger.info("Starting {{ name }}")
    {:ok, %{status: :initialized, opts: opts}}
  end
  
  def handle_call(:status, _from, state) do
    {:reply, state.status, state}
  end
end'''

    def get_vue_template(self) -> str:
        return '''<template>
  <div class="{{ kebab_case }}-component">
    <h2>{{ title }}</h2>
    <!-- Generated by ULTRATHINK 80/20 - NO TypeScript -->
  </div>
</template>

<script setup>
// {{ name }} Component
const title = ref('{{ class_name }}')
</script>

<style scoped>
.{{ kebab_case }}-component {
  padding: 1rem;
}
</style>'''

    def get_python_template(self) -> str:
        return '''#!/usr/bin/env python3
"""{{ name }} - Generated by ULTRATHINK 80/20"""

import typer
from rich.console import Console

app = typer.Typer()
console = Console()

@app.command()
def main():
    """{{ description | default("Main command") }}"""
    console.print("🚀 {{ name }}")

if __name__ == "__main__":
    app()'''

    def get_yaml_template(self) -> str:
        return '''# {{ name }} - Generated by ULTRATHINK 80/20
apiVersion: apps/v1
kind: Deployment
metadata:
  name: {{ app_name }}
spec:
  replicas: {{ replicas | default(3) }}'''

    def get_erlang_template(self) -> str:
        return '''-module({{ snake_case }}).
-behaviour(gen_server).

%% {{ name }} - Generated by ULTRATHINK 80/20

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.'''

@app.command()
def create(
    generator_type: str = typer.Argument(..., help="Type of component to generate"),
    name: str = typer.Argument(..., help="Name of the component"),
    interactive: bool = typer.Option(False, "--interactive", "-i", help="Interactive mode"),
    output_dir: Optional[str] = typer.Option(None, "--output", "-o", help="Custom output directory")
):
    """🏗️ Generate a new component"""
    
    if generator_type not in GENERATORS:
        console.print(f"❌ Unknown generator: {generator_type}")
        console.print(f"Available generators: {', '.join(GENERATORS.keys())}")
        raise typer.Exit(1)
    
    options = {}
    
    if interactive:
        options = gather_interactive_options(generator_type)
    
    if output_dir:
        GENERATORS[generator_type]["output_dir"] = output_dir
    
    generator = CodeGenerator()
    success = generator.generate(generator_type, name, options)
    
    if not success:
        raise typer.Exit(1)

@app.command()
def list():
    """📋 List all available generators"""
    
    console.print("🏗️ Available ULTRATHINK 80/20 Generators")
    
    table = Table()
    table.add_column("Generator", style="cyan")
    table.add_column("Description")
    table.add_column("Output", style="green")
    table.add_column("Dependencies", style="yellow")
    
    for gen_type, config in GENERATORS.items():
        deps = ", ".join(config.get("dependencies", []))
        
        table.add_row(
            gen_type,
            config["description"],
            config["output_dir"],
            deps
        )
    
    console.print(table)

@app.command()
def template(
    generator_type: str = typer.Argument(..., help="Generator type"),
    action: str = typer.Option("view", "--action", "-a", help="Action: view, edit, create")
):
    """📝 Manage generator templates"""
    
    if generator_type not in GENERATORS:
        console.print(f"❌ Unknown generator: {generator_type}")
        raise typer.Exit(1)
    
    config = GENERATORS[generator_type]
    template_dir = Path(config["template_dir"])
    
    if action == "view":
        show_template_info(config, template_dir)
    elif action == "create":
        create_template_directory(config, template_dir)
    elif action == "edit":
        edit_template(config, template_dir)

def gather_interactive_options(generator_type: str) -> Dict:
    """Gather options interactively"""
    options = {}
    
    console.print(f"\n🎯 Interactive {generator_type} generator")
    
    if generator_type == "ash_resource":
        options["attributes"] = Prompt.ask("Attributes (comma-separated)", default="name:string,description:string").split(",")
        options["actions"] = Prompt.ask("Actions (comma-separated)", default="create,read,update,destroy").split(",")
        options["domain"] = Prompt.ask("Domain", default="Cybersecurity")
    
    elif generator_type == "nuxt_component":
        options["props"] = Prompt.ask("Props (comma-separated)", default="").split(",") if Prompt.ask("Props?", default="") else []
        options["scoped"] = Confirm.ask("Scoped styles?", default=True)
    
    elif generator_type == "k8s_manifest":
        options["replicas"] = int(Prompt.ask("Replicas", default="3"))
        options["port"] = int(Prompt.ask("Port", default="4000"))
        options["namespace"] = Prompt.ask("Namespace", default="cns-forge")
    
    return options

def show_template_info(config: Dict, template_dir: Path):
    """Show information about a template"""
    console.print(f"\n📝 Template: {config['name']}")
    console.print(f"Directory: {template_dir}")
    console.print(f"Output: {config['output_dir']}")
    console.print(f"Extension: {config['file_extension']}")
    
    if template_dir.exists():
        console.print("\n📁 Template files:")
        for file in template_dir.glob("*"):
            console.print(f"  • {file.name}")
    else:
        console.print("⚠️ Template directory does not exist")

def create_template_directory(config: Dict, template_dir: Path):
    """Create template directory with default templates"""
    template_dir.mkdir(parents=True, exist_ok=True)
    console.print(f"✅ Created template directory: {template_dir}")
    
    generator = CodeGenerator()
    generator.create_default_template(config, template_dir)
    console.print("✅ Created default template")

def edit_template(config: Dict, template_dir: Path):
    """Edit template (would open in editor)"""
    console.print(f"📝 Edit template in: {template_dir}")
    console.print("Use your preferred editor to modify the template files")

if __name__ == "__main__":
    console.print("""
🏗️ ULTRATHINK 80/20 Code Generator
==================================

Generate components across the entire technology stack:
- Ash Resources (Elixir)
- Reactor Workflows (Elixir)
- Nuxt.js Components (Vue, NO TypeScript)
- Channel Handlers (Phoenix)
- BitActors (Distributed Elixir)
- Python CLI Commands (Typer)
- Kubernetes Manifests
- DSPy Signatures
- Erlang GenServers

20% effort → 80% code generation capability
    """)
    
    app()