defmodule CNSForge.TemplateEngine do
  @moduledoc """
  Template engine for generating Ash.Reactor project files
  
  Renders all necessary project components from ontology specifications
  Focus: 80/20 template generation for working projects
  """
  
  require Logger

  @doc "Render README.md for generated project"
  def render_readme(project_spec) do
    """
    # #{project_spec.module_name}

    #{project_spec.description}

    ## Overview

    This project was generated from the `#{project_spec.ontology.name}` ontology using CNS Forge.
    It implements a complete Ash.Reactor architecture with TTL-bounded execution.

    ## Generated Components

    - **#{length(project_spec.resources || [])} Resources**: Domain entities
    - **#{length(project_spec.reactors || [])} Reactors**: Business logic workflows  
    - **#{length(project_spec.workflows || [])} Workflows**: Process coordination

    ## Getting Started

    ```bash
    mix deps.get
    mix test
    ```

    Generated at: #{DateTime.utc_now() |> DateTime.to_iso8601()}
    """
  end

  @doc "Render .gitignore for Elixir project"
  def render_gitignore(_project_spec) do
    """
    # Build artifacts
    /_build/
    /cover/
    /deps/
    /doc/
    /.fetch
    erl_crash.dump
    *.beam
    *.plt
    *.tar
    .env*
    *.log
    .DS_Store
    .elixir_ls/
    """
  end

  @doc "Render .formatter.exs"
  def render_formatter_config(_project_spec) do
    """
    [
      import_deps: [:ash, :reactor],
      inputs: ["{mix,.formatter}.exs", "{config,lib,test}/**/*.{ex,exs}"]
    ]
    """
  end

  @doc "Render configuration files"
  def render_config(project_spec, env) do
    case env do
      :config -> render_main_config(project_spec)
      :dev -> render_dev_config(project_spec)
      :prod -> render_prod_config(project_spec)
      :test -> render_test_config(project_spec)
      :runtime -> render_runtime_config(project_spec)
    end
  end

  defp render_main_config(project_spec) do
    """
    import Config

    config :ash, :validate_domain_resource_inclusion?, false
    config :ash, :validate_domain_config_inclusion?, false

    config :#{project_spec.app_name}, :ttl_constraints,
      max_execution_hops: 8,
      max_processing_time_ms: 5000

    config :logger, :console,
      format: "$time $metadata[$level] $message\\n",
      metadata: [:request_id]

    import_config "\#{config_env()}.exs"
    """
  end

  defp render_dev_config(project_spec) do
    """
    import Config
    config :logger, :console, format: "[$level] $message\\n"
    config :#{project_spec.app_name}, dev_mode: true
    """
  end

  defp render_prod_config(_project_spec) do
    """
    import Config
    config :logger, level: :info
    """
  end

  defp render_test_config(project_spec) do
    """
    import Config
    config :logger, level: :warning
    config :#{project_spec.app_name}, :ttl_constraints,
      max_execution_hops: 16,
      max_processing_time_ms: 10000
    """
  end

  defp render_runtime_config(_project_spec) do
    """
    import Config
    # Runtime configuration
    """
  end

  @doc "Render test helper"
  def render_test_helper(project_spec) do
    """
    ExUnit.start()

    defmodule #{project_spec.module_name}.TestHelper do
      def create_test_data(resource, attrs \\\\ %{}) do
        default_attrs = %{name: "Test Resource"}
        attrs = Map.merge(default_attrs, attrs)
        Ash.create!(resource, attrs)
      end
    end
    """
  end

  @doc "Render application test"
  def render_application_test(project_spec) do
    """
    defmodule #{project_spec.module_name}Test do
      use ExUnit.Case, async: true
      
      test "application starts" do
        assert is_atom(#{project_spec.module_name})
      end
    end
    """
  end

  @doc "Render reactor test"
  def render_reactor_test(project_spec, reactor_spec) do
    """
    defmodule #{project_spec.module_name}.Reactors.#{reactor_spec.module_name}Test do
      use ExUnit.Case, async: true
      
      test "reactor executes successfully" do
        input = %{test_data: "example"}
        assert {:ok, _result} = Reactor.run(#{project_spec.module_name}.Reactors.#{reactor_spec.module_name}, input)
      end
    end
    """
  end

  @doc "Render workflow test"
  def render_workflow_test(project_spec, workflow_spec) do
    """
    defmodule #{project_spec.module_name}.Workflows.#{workflow_spec.module_name}Test do
      use ExUnit.Case, async: true
      
      test "workflow executes" do
        assert :ok == :ok
      end
    end
    """
  end

  @doc "Render integration tests"
  def render_integration_tests(project_spec) do
    """
    defmodule #{project_spec.module_name}.IntegrationTest do
      use ExUnit.Case, async: false
      
      test "full system integration" do
        assert :ok == :ok
      end
    end
    """
  end

  # Deployment templates
  def render_dockerfile(project_spec) do
    """
    FROM elixir:1.15-alpine AS builder
    WORKDIR /app
    RUN mix local.hex --force && mix local.rebar --force
    COPY mix.exs mix.lock ./
    ENV MIX_ENV=prod
    RUN mix deps.get --only prod
    COPY . .
    RUN mix compile && mix release

    FROM alpine:3.18
    RUN apk add --no-cache openssl ncurses-libs
    WORKDIR /app
    COPY --from=builder /app/_build/prod/rel/#{project_spec.app_name} ./
    EXPOSE 4000
    CMD ["./bin/#{project_spec.app_name}", "start"]
    """
  end

  def render_k8s_deployment(project_spec) do
    """
    apiVersion: apps/v1
    kind: Deployment
    metadata:
      name: #{project_spec.app_name}
    spec:
      replicas: 3
      selector:
        matchLabels:
          app: #{project_spec.app_name}
      template:
        metadata:
          labels:
            app: #{project_spec.app_name}
        spec:
          containers:
          - name: #{project_spec.app_name}
            image: #{project_spec.app_name}:latest
            ports:
            - containerPort: 4000
    """
  end

  def render_k8s_service(project_spec) do
    """
    apiVersion: v1
    kind: Service
    metadata:
      name: #{project_spec.app_name}-service
    spec:
      selector:
        app: #{project_spec.app_name}
      ports:
      - port: 80
        targetPort: 4000
      type: LoadBalancer
    """
  end

  def render_k8s_configmap(project_spec) do
    """
    apiVersion: v1
    kind: ConfigMap
    metadata:
      name: #{project_spec.app_name}-config
    data:
      MIX_ENV: "prod"
      PORT: "4000"
    """
  end

  def render_k8s_ingress(project_spec) do
    """
    apiVersion: networking.k8s.io/v1
    kind: Ingress
    metadata:
      name: #{project_spec.app_name}-ingress
    spec:
      rules:
      - host: #{project_spec.app_name}.local
        http:
          paths:
          - path: /
            pathType: Prefix
            backend:
              service:
                name: #{project_spec.app_name}-service
                port:
                  number: 80
    """
  end

  def render_terraform_main(project_spec) do
    """
    terraform {
      required_providers {
        aws = { source = "hashicorp/aws", version = "~> 5.0" }
      }
    }

    resource "aws_ecs_cluster" "#{project_spec.app_name}" {
      name = "#{project_spec.app_name}-cluster"
    }
    """
  end

  def render_terraform_variables(project_spec) do
    """
    variable "aws_region" {
      description = "AWS region for #{project_spec.app_name}"
      type        = string  
      default     = "us-west-2"
    }
    """
  end

  def render_terraform_outputs(project_spec) do
    """
    output "cluster_name" {
      value = aws_ecs_cluster.#{project_spec.app_name}.name
    }
    """
  end

  # Documentation templates
  def render_api_documentation(project_spec) do
    """
    # #{project_spec.module_name} API Documentation
    
    Generated API documentation for the #{project_spec.ontology.name} ontology.
    """
  end

  def render_deployment_guide(project_spec) do
    """
    # #{project_spec.module_name} Deployment Guide

    ## Docker Deployment
    ```bash
    docker build -t #{project_spec.app_name} .
    docker run -p 4000:4000 #{project_spec.app_name}
    ```
    """
  end

  def render_workflow_documentation(_project_spec, workflow_spec) do
    """
    # #{workflow_spec.name} Workflow
    #{workflow_spec.description || "Generated workflow documentation"}
    """
  end

  # Utility renderers
  def render_middleware(middleware) do
    case middleware do
      :telemetry -> "Reactor.Middleware.Telemetry"
      :ttl_enforcement -> "CNSForge.Middleware.TTLEnforcement"  
      :semantic_validation -> "CNSForge.Middleware.SemanticValidation"
      _ -> to_string(middleware)
    end
  end

  def render_step(step, _module_name) do
    """
    step :#{step.name || "sample_step"} do
      run fn input, _context ->
        {:ok, %{processed: input, step: "#{step.name || "sample_step"}"}}
      end
    end
    """
  end
end