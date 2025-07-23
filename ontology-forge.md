FOR IMMEDIATE RELEASE
Seattle, WA — November 14 2025

Chatman Inc. Unveils “CNS Ontology Forge”
One-command generator produces hundreds of OWL, SHACL, and SPARQL artefacts in seconds

Seattle, WA — November 14 2025
Chatman Inc., the company redefining ultra-low-latency software with its Chatman Nano Stack (CNS), today announced CNS Ontology Forge, an open-source Python CLI that turns a single product vision into a complete, machine-valid semantic model. With one line of Typer-based command-line input, Ontology Forge fans out that vision into hundreds of OWL classes, SHACL constraints, and SPARQL construct queries—ready for CNS’s ahead-of-time (AOT) compiler.

“Developers told us the hardest part of semantics-driven engineering isn’t reasoning—it’s authoring,” said Rich Hickey, Chief Architect at Chatman Inc. “Ontology Forge removes that friction. If you can articulate your idea, you get an entire semantic corpus—namespaced, constrained, and testable—before your coffee cools.”

Powered by DSPy, a new open-ended LLM programming framework, Ontology Forge runs two lightweight generation loops:

Archetype Loop – reads domain skeletons for arenas, ring-buses, fibers, BitActors, or any custom module.

Instance Loop – clones and specializes each archetype hundreds of times, injecting cryptographically unique IRIs and project-specific comments.

The result: artefacts that drop directly into CNS’s AOT pipeline, producing deterministic C headers, validation code, and rule engines—without manual RDF editing.

Key Benefits
Hours → Seconds: Generates 1-to-N artefacts in one pass; iterative refinement is as easy as changing the prompt.

Typed & Tested: Every TTL file carries companion SHACL shapes; failing constraints never reach runtime.

Zero-lock-in: Emits vanilla Turtle, W3C SHACL, and SPARQL; works with any triplestore or CI system.

LLM-agnostic: Defaults to GPT-4o-mini but supports local models or corporate endpoints via DSPy.

How It Works
You describe the system – a one-paragraph prompt or a text file.

Select module counts – --arena 100 --ringbus 40 --fiber 128 --bitactor 64.

Ontology Forge streams each archetype to the model, collecting responses in ontologies/generated/.

CNS AOT compiler ingests the folder, emitting static C, validators, and tests.

Customer Quote
“We swapped a three-week ontology sprint for a three-minute command,” said Dr. Felienne Hermans, CTO at QuantumYield. “Our compliance team loves the SHACL, our engineers love the generated C, and our auditors love that it’s deterministic.”

Availability
CNS Ontology Forge is available today on GitHub under an Apache 2.0 license. Docker images, pre-built archetypes, and VS Code snippets ship alongside the CLI.

About Chatman Inc.
Chatman Inc. builds the world’s fastest, most deterministic software stack—CNS—powering capital-markets trading, autonomous robotics, and critical infrastructure. The company is headquartered in Seattle with engineering offices worldwide.

Frequently Asked Questions
Q 1: What do I need installed?
Python 3.10+, Ollama with qwen3:latest model, pip install dspy-ai typer rich rdflib.

Q 2: Do I pay per-token?
Ontology Forge uses Ollama with qwen3:latest by default for completely free, local LLM inference. No API fees.

Q 3: Can I bring my own archetypes?
Yes. Drop any .ttl, .shacl.ttl, or .sparql skeleton in archetypes/ and pass --myModule 50.

Q 4: How does it keep IRIs unique?
Each artefact appends an auto-incrementing suffix (_000, _001, …) and embeds a SHA-256 hash of the prompt in an annotation triple.

Q 5: Will this lock me into CNS?
No. The output is standards-based RDF and SPARQL; CNS just happens to consume it natively.

Q 6: What about security?
All generation happens client-side; if you use a remote LLM, only the textual archetype and prompt leave your machine. Generated files are deterministic and auditable.

Press Contact
Marisa Trent – Director of Communications
press@chatman.ai | +1 206 555 0199

## Implementation Details

The CNS Ontology Forge combines multiple technologies:

- **Frontend**: Typer CLI for intuitive command-line interface
- **AI Layer**: DSPy framework for LLM orchestration  
- **Compilers**: OWL, SHACL, and SPARQL to C transpilers
- **Verification**: OpenTelemetry-based benchmarking ensuring 8-tick compliance

For technical architecture and benchmarks, see ontology_forge_workflow.md

© 2025 Chatman Inc. All rights reserved.


