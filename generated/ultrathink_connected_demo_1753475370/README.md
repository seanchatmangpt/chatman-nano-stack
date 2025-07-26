# Ultrathink

An Ash.Reactor application generated from the `ultrathink` ontology.

## Overview

This project demonstrates a complete Ash.Reactor implementation that transforms ontological concepts into working Elixir code. It includes:

- **Domain Resources**: Ash resources for each ontology class
- **Reactor Workflows**: Business logic orchestration using Ash.Reactor
- **Complete CRUD Operations**: Full create, read, update, delete functionality
- **Comprehensive Tests**: Full test coverage for all components

## Generated Components

### Resources
- `BitActor` - BitActor: Autonomous AI trading agent with TTL-bounded execution
- `IntelligenceNode` - Intelligence Node: Hyper-intelligent processing node in the swarm
- `CoordinationReactor` - Coordination Reactor: Ash.Reactor workflow for swarm coordination
- `Signal` - Trading Signal: Market signal with embedded intelligence
- `EmergentBehavior` - communicates with: Intelligent behavior emerging from swarm interaction

### Workflows
- `MainWorkflow` - Main domain orchestration
- `BitActorWorkflow` - Operations for BitActor resources
- `IntelligenceNodeWorkflow` - Operations for IntelligenceNode resources
- `CoordinationReactorWorkflow` - Operations for CoordinationReactor resources
- `SignalWorkflow` - Operations for Signal resources
- `EmergentBehaviorWorkflow` - Operations for EmergentBehavior resources

## Getting Started

### Prerequisites
- Elixir 1.15+
- PostgreSQL 12+

### Installation

1. Install dependencies:
   ```bash
   mix deps.get
   ```

2. Set up the database:
   ```bash
   mix ecto.setup
   ```

3. Run tests:
   ```bash
   mix test
   ```

### Usage

#### Using Resources Directly

```elixir
# Create a resource
{:ok, resource} = Ash.create(MyApp.Resources.SomeResource, %{
  name: "Example",
  description: "Created via Ash"
})

# Read resources
{:ok, resources} = Ash.read(MyApp.Resources.SomeResource)

# Update a resource
{:ok, updated} = Ash.update(resource, %{name: "Updated Name"})
```

#### Using Reactor Workflows

```elixir
# Run the main workflow
{:ok, result} = Reactor.run(Ultrathink.Workflows.MainWorkflow, %{
  operation: :process,
  data: %{key: "value"}
})

# Run a resource-specific workflow
{:ok, result} = Reactor.run(Ultrathink.Workflows.SomeResourceWorkflow, %{
  action: :create,
  resource_data: %{name: "Example"}
})
```

## Architecture

This application follows the Ash.Reactor pattern:

1. **Resources** define the domain model and available actions
2. **Workflows** orchestrate complex business logic using Reactor
3. **Domain** coordinates all resources and provides a unified interface

### Key Features

- **Declarative Resources**: Domain logic defined as data
- **Saga Pattern**: Distributed transaction support via Reactor
- **Type Safety**: Comprehensive validation and error handling
- **Observability**: Built-in telemetry and monitoring
- **Test Coverage**: Comprehensive test suite

## Development

### Running Tests
```bash
# Run all tests
mix test

# Run with coverage
mix coveralls

# Run specific test file
mix test test/path/to/test_file.exs
```

### Code Quality
```bash
# Format code
mix format

# Static analysis
mix credo

# Type checking  
mix dialyzer
```

## Generated from Ontology

This project was automatically generated from the `ultrathink` ontology using the Ontology to Ash.Reactor Generator.

**Generated at**: 2025-07-25T13:29:31.011145

## License

Generated code - modify as needed for your use case.
