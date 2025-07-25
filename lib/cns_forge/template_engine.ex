defmodule CNSForge.TemplateEngine do
  @moduledoc """
  Template engine for generating complete Elixir/Ash.Reactor project files
  from ontology specifications.
  """

  @templates_dir Path.join([__DIR__, "..", "..", "templates", "project_scaffolder"])

  def render_mix_exs(project_spec) do
    template = """
    defmodule <%= @module_name %>.MixProject do
      use Mix.Project

      def project do
        [
          app: :<%= @app_name %>,
          version: "<%= @version %>",
          elixir: "<%= @elixir_version %>",
          elixirc_paths: elixirc_paths(Mix.env()),
          start_permanent: Mix.env() == :prod,
          aliases: aliases(),
          deps: deps(),
          description: "<%= @description %>",
          package: package(),
          test_coverage: [tool: ExCoveralls],
          preferred_cli_env: [
            coveralls: :test,
            "coveralls.detail": :test,
            "coveralls.post": :test,
            "coveralls.html": :test
          ],
          dialyzer: [
            plt_file: {:no_warn, "priv/plts/dialyzer.plt"}
          ]
        ]
      end

      def application do
        [
          mod: {<%= @module_name %>.Application, []},
          extra_applications: [:logger, :runtime_tools, :crypto, :inets, :ssl]
        ]
      end

      defp elixirc_paths(:test), do: ["lib", "test/support"]
      defp elixirc_paths(_), do: ["lib"]

      defp deps do
        [
          # Core Ash Framework
          {:ash, "~> 3.0"},
          {:ash_postgres, "~> 2.0"},
          {:ash_phoenix, "~> 2.0"},
          
          # Reactor Framework
          {:reactor, "~> 0.8"},
          
          # Phoenix Framework
          {:phoenix, "~> 1.7.0"},
          {:phoenix_ecto, "~> 4.4"},
          {:ecto_sql, "~> 3.10"},
          {:postgrex, ">= 0.0.0"},
          {:phoenix_html, "~> 4.0"},
          {:phoenix_live_reload, "~> 1.2", only: :dev},
          {:phoenix_live_view, "~> 0.20.0"},
          {:phoenix_live_dashboard, "~> 0.8.0"},
          
          # Asset compilation
          {:esbuild, "~> 0.8", runtime: Mix.env() == :dev},
          {:tailwind, "~> 0.2.0", runtime: Mix.env() == :dev},
          
          # Telemetry and monitoring
          {:telemetry_metrics, "~> 0.6"},
          {:telemetry_poller, "~> 1.0"},
          {:opentelemetry, "~> 1.0"},
          {:opentelemetry_exporter, "~> 1.0"},
          
          # JSON and utilities
          {:gettext, "~> 0.20"},
          {:jason, "~> 1.2"},
          {:dns_cluster, "~> 0.1.1"},
          {:bandit, "~> 1.0"},
          
          # CNS Forge integration
          {:cns_forge, path: "<%= @cns_forge_path %>"},
          
          # Semantic web support
          {:semantic_web, "~> 0.1.0", optional: true},
          {:sparql, "~> 0.3.0", optional: true},
          
          # Development and testing
          {:floki, ">= 0.30.0", only: :test},
          {:excoveralls, "~> 0.18", only: :test},
          {:ex_doc, "~> 0.31", only: :dev, runtime: false},
          {:dialyxir, "~> 1.4", only: [:dev], runtime: false},
          {:credo, "~> 1.7", only: [:dev, :test], runtime: false}
        ]
      end

      defp aliases do
        [
          setup: ["deps.get", "ecto.setup", "assets.setup", "assets.build"],
          "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
          "ecto.reset": ["ecto.drop", "ecto.setup"],
          test: ["ecto.create --quiet", "ecto.migrate --quiet", "test"],
          "assets.setup": ["tailwind.install --if-missing", "esbuild.install --if-missing"],
          "assets.build": ["tailwind default", "esbuild default"],
          "assets.deploy": ["tailwind default --minify", "esbuild default --minify", "phx.digest"],
          quality: ["format", "credo --strict", "dialyzer"],
          ci: ["setup", "quality", "test --cover"]
        ]
      end

      defp package do
        [
          description: "<%= @description %>",
          files: ~w(lib .formatter.exs mix.exs README* LICENSE*),
          licenses: ["MIT"],
          links: %{"GitHub" => "https://github.com/cns-forge/<%= @app_name %>"}
        ]
      end
    end
    """
    
    EEx.eval_string(template, assigns: [
      module_name: project_spec.module_name,
      app_name: project_spec.app_name,
      version: project_spec.version,
      elixir_version: project_spec.elixir_version,
      description: project_spec.description,
      cns_forge_path: "../cns"
    ])
  end

  def render_application(project_spec) do
    template = """
    defmodule <%= @module_name %>.Application do
      @moduledoc false

      use Application

      @impl true
      def start(_type, _args) do
        children = [
          # Telemetry setup
          <%= @module_name %>Web.Telemetry,
          
          # Database
          <%= @module_name %>.Repo,
          
          # DNS cluster for distributed deployment
          {DNSCluster, query: Application.get_env(:<%= @app_name %>, :dns_cluster_query) || :ignore},
          
          # PubSub for real-time features
          {Phoenix.PubSub, name: <%= @module_name %>.PubSub},
          
          # HTTP client for external APIs
          {Finch, name: <%= @module_name %>.Finch},
          
          # Ash Registry for resources
          {Registry, keys: :unique, name: <%= @module_name %>.Registry},
          
          # CNS Forge integration
          {CNSForge.ReactorSupervisor, [
            name: <%= @module_name %>.ReactorSupervisor,
            reactors: [
              <%= for reactor <- @reactors do %>
              <%= @module_name %>.Reactors.<%= reactor.module_name %>,<% end %>
            ]
          ]},
          
          # Semantic processing supervisor
          <%= @module_name %>.SemanticSupervisor,
          
          # Web endpoint
          <%= @module_name %>Web.Endpoint
        ]

        opts = [strategy: :one_for_one, name: <%= @module_name %>.Supervisor]
        Supervisor.start_link(children, opts)
      end

      @impl true
      def config_change(changed, _new, removed) do
        <%= @module_name %>Web.Endpoint.config_change(changed, removed)
        :ok
      end
    end
    """
    
    EEx.eval_string(template, assigns: [
      module_name: project_spec.module_name,
      app_name: project_spec.app_name,
      reactors: project_spec.reactors
    ])
  end

  def render_repo(project_spec) do
    template = """
    defmodule <%= @module_name %>.Repo do
      use Ecto.Repo,
        otp_app: :<%= @app_name %>,
        adapter: Ecto.Adapters.Postgres
        
      use AshPostgres.Repo, registry: <%= @module_name %>.Registry
    end
    """
    
    EEx.eval_string(template, assigns: [
      module_name: project_spec.module_name,
      app_name: project_spec.app_name
    ])
  end

  def render_registry(project_spec) do
    template = """
    defmodule <%= @module_name %>.Registry do
      use Ash.Registry,
        extensions: [
          # Add extensions here
        ]

      entries do
        <%= for resource <- @resources do %>
        entry <%= @module_name %>.Resources.<%= resource.module_name %><% end %>
      end
    end
    """
    
    EEx.eval_string(template, assigns: [
      module_name: project_spec.module_name,
      resources: project_spec.resources
    ])
  end

  def render_web_module(project_spec) do
    template = """
    defmodule <%= @module_name %>Web do
      @moduledoc \"\"\"
      The entrypoint for defining your web interface, such
      as controllers, components, channels, and so on.

      This can be used in your application as:

          use <%= @module_name %>Web, :controller
          use <%= @module_name %>Web, :html

      The definitions below will be executed for every controller,
      component, etc, so keep them short and clean, focused
      on imports, uses and aliases.

      Do NOT define functions inside the quoted expressions
      below. Instead, define additional modules and import
      those modules here.
      \"\"\"

      def static_paths, do: ~w(assets fonts images favicon.ico robots.txt)

      def router do
        quote do
          use Phoenix.Router, helpers: false

          # Import common connection and controller functions
          import Plug.Conn
          import Phoenix.Controller
          import Phoenix.LiveView.Router
        end
      end

      def channel do
        quote do
          use Phoenix.Channel
        end
      end

      def controller do
        quote do
          use Phoenix.Controller,
            formats: [:html, :json],
            layouts: [html: <%= @module_name %>Web.Layouts]

          import Plug.Conn
          import <%= @module_name %>Web.Gettext

          unquote(verified_routes())
        end
      end

      def live_view do
        quote do
          use Phoenix.LiveView,
            layout: {<%= @module_name %>Web.Layouts, :app}

          unquote(html_helpers())
        end
      end

      def live_component do
        quote do
          use Phoenix.LiveComponent

          unquote(html_helpers())
        end
      end

      def html do
        quote do
          use Phoenix.Component

          # Import convenience functions from controllers
          import Phoenix.Controller,
            only: [get_csrf_token: 0, view_module: 1, view_template: 1]

          # Include general helpers for rendering HTML
          unquote(html_helpers())
        end
      end

      defp html_helpers do
        quote do
          # HTML escaping functionality
          import Phoenix.HTML
          # Core UI components and translation
          import <%= @module_name %>Web.CoreComponents
          import <%= @module_name %>Web.Gettext

          # Shortcut for generating JS commands
          alias Phoenix.LiveView.JS

          # Routes generation with the ~p sigil
          unquote(verified_routes())
        end
      end

      def verified_routes do
        quote do
          use Phoenix.VerifiedRoutes,
            endpoint: <%= @module_name %>Web.Endpoint,
            router: <%= @module_name %>Web.Router,
            statics: <%= @module_name %>Web.static_paths()
        end
      end

      @doc \"\"\"
      When used, dispatch to the appropriate controller/view/etc.
      \"\"\"
      defmacro __using__(which) when is_atom(which) do
        apply(__MODULE__, which, [])
      end
    end
    """
    
    EEx.eval_string(template, assigns: [
      module_name: project_spec.module_name
    ])
  end

  def render_reactor(project_spec, reactor_spec) do
    template = """
    defmodule <%= @module_name %>.Reactors.<%= @reactor_module_name %> do
      @moduledoc \"\"\"
      <%= @description %>
      
      Generated from TTL ontology class: <%= @reactor_name %>
      TTL Budget: <%= @ttl_budget %> hops
      \"\"\"

      use Reactor

      # Middleware configuration
      <%= for middleware <- @middleware do %>
      middleware <%= render_middleware(middleware) %><% end %>

      # Input definitions
      <%= for input <- @inputs do %>
      input :<%= input.name %><%= if input[:type], do: ", type: #{inspect(input.type)}", else: "" %><% end %>

      # Step definitions
      <%= for step <- @steps do %>
      <%= render_step(step, @module_name) %>
      <% end %>

      # Return the final result
      return :result

      # Utility functions for this reactor
      defp validate_ttl_budget(token, budget) do
        if token.ttl_remaining <= 0 do
          {:error, :ttl_exceeded}
        else
          {:ok, %{token | ttl_remaining: min(token.ttl_remaining, budget)}}
        end
      end

      defp emit_telemetry(event, metadata \\\\ %{}) do
        :telemetry.execute(
          [:<%= @app_name %>, :reactor, :<%= @reactor_name %>] ++ event,
          %{timestamp: System.monotonic_time()},
          metadata
        )
      end
    end
    """
    
    EEx.eval_string(template, assigns: [
      module_name: project_spec.module_name,
      app_name: project_spec.app_name,
      reactor_module_name: reactor_spec.module_name,
      reactor_name: reactor_spec.name,
      description: reactor_spec.description,
      ttl_budget: reactor_spec.ttl_budget,
      middleware: reactor_spec.middleware,
      inputs: reactor_spec.inputs,
      steps: reactor_spec.steps
    ])
  end

  def render_ash_resource(project_spec, resource_spec) do
    template = """
    defmodule <%= @module_name %>.Resources.<%= @resource_module_name %> do
      @moduledoc \"\"\"
      <%= @description %>
      
      Generated from TTL ontology class: <%= @resource_name %>
      \"\"\"

      use Ash.Resource,
        domain: <%= @module_name %>.Domain,
        data_layer: AshPostgres.DataLayer

      # Resource configuration
      resource do
        description("<%= @description %>")
      end

      # PostgreSQL table configuration
      postgres do
        table :<%= @resource_name %>s
        repo <%= @module_name %>.Repo
      end

      # Attributes
      attributes do
        uuid_primary_key :id

        <%= for attr <- @attributes do %>
        attribute :<%= attr.name %>, :<%= attr.type %><%= if attr.required, do: ", allow_nil?: false", else: "" %> do
          <%= if attr.description do %>description "<%= attr.description %>"<% end %>
          <%= if attr.default do %>default <%= inspect(attr.default) %><% end %>
        end
        <% end %>

        timestamps()
      end

      # Actions
      actions do
        defaults [:create, :read, :update, :destroy]

        <%= for action <- @actions do %>
        <%= render_action(action) %>
        <% end %>
      end

      # Relationships
      relationships do
        <%= for relationship <- @relationships do %>
        <%= render_relationship(relationship, @module_name) %>
        <% end %>
      end

      # Validations
      validations do
        # Add custom validations here
      end

      # Calculations
      calculations do
        # Add calculations here
      end
    end
    """
    
    EEx.eval_string(template, assigns: [
      module_name: project_spec.module_name,
      resource_module_name: resource_spec.module_name,
      resource_name: resource_spec.name,
      description: resource_spec.description,
      attributes: resource_spec.attributes,
      actions: resource_spec.actions,
      relationships: resource_spec.relationships
    ])
  end

  def render_config(project_spec, env) do
    case env do
      :config -> render_base_config(project_spec)
      :dev -> render_dev_config(project_spec)
      :prod -> render_prod_config(project_spec)
      :test -> render_test_config(project_spec)
      :runtime -> render_runtime_config(project_spec)
    end
  end

  defp render_base_config(project_spec) do
    template = """
    import Config

    # Configure the main application
    config :<%= @app_name %>,
      namespace: <%= @module_name %>,
      ecto_repos: [<%= @module_name %>.Repo],
      generators: [timestamp_type: :utc_datetime]

    # Configure the endpoint
    config :<%= @app_name %>, <%= @module_name %>Web.Endpoint,
      url: [host: "localhost"],
      adapter: Bandit.PhoenixAdapter,
      render_errors: [
        formats: [html: <%= @module_name %>Web.ErrorHTML, json: <%= @module_name %>Web.ErrorJSON],
        layout: false
      ],
      pubsub_server: <%= @module_name %>.PubSub,
      live_view: [signing_salt: "<%= @signing_salt %>"]

    # Configure esbuild
    config :esbuild,
      version: "0.21.5",
      <%= @app_name %>: [
        args: ~w(js/app.js --bundle --target=es2017 --outdir=../priv/static/assets --external:/fonts/* --external:/images/*),
        cd: Path.expand("../assets", __DIR__),
        env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
      ]

    # Configure tailwind
    config :tailwind,
      version: "3.4.0",
      <%= @app_name %>: [
        args: ~w(
          --config=tailwind.config.js
          --input=css/app.css
          --output=../priv/static/assets/app.css
        ),
        cd: Path.expand("../assets", __DIR__)
      ]

    # Configure Ash
    config :ash, :validate_domain_resource_inclusion?, false
    config :ash, :validate_domain_config_inclusion?, false

    # Configure CNS Forge
    config :cns_forge,
      telemetry_enabled: true,
      ttl_enforcement: true,
      semantic_validation: true

    # Configure Phoenix
    config :phoenix, :json_library, Jason

    # Import environment specific config
    import_config "\#{config_env()}.exs"
    """
    
    EEx.eval_string(template, assigns: [
      app_name: project_spec.app_name,
      module_name: project_spec.module_name,
      signing_salt: Base.encode64(:crypto.strong_rand_bytes(8))
    ])
  end

  defp render_dev_config(project_spec) do
    template = """
    import Config

    # Configure the database
    config :<%= @app_name %>, <%= @module_name %>.Repo,
      username: "postgres",
      password: "postgres",
      hostname: "localhost",
      database: "<%= @app_name %>_dev",
      stacktrace: true,
      show_sensitive_data_on_connection_error: true,
      pool_size: 10

    # Configure the endpoint
    config :<%= @app_name %>, <%= @module_name %>Web.Endpoint,
      http: [ip: {127, 0, 0, 1}, port: 4000],
      check_origin: false,
      code_reloader: true,
      debug_errors: true,
      secret_key_base: "<%= @secret_key_base %>",
      watchers: [
        esbuild: {Esbuild, :install_and_run, [:<%= @app_name %>, ~w(--sourcemap=inline --watch)]},
        tailwind: {Tailwind, :install_and_run, [:<%= @app_name %>, ~w(--watch)]}
      ]

    # Watch static and templates for browser reloading
    config :<%= @app_name %>, <%= @module_name %>Web.Endpoint,
      live_reload: [
        patterns: [
          ~r"priv/static/(?!uploads/).*(js|css|png|jpeg|jpg|gif|svg)$",
          ~r"priv/gettext/.*(po)$",
          ~r"lib/<%= @app_name %>_web/(controllers|live|components)/.*(ex|heex)$"
        ]
      ]

    # Enable dev routes for dashboard and mailbox
    config :<%= @app_name %>, dev_routes: true

    # Do not include metadata nor timestamps in development logs
    config :logger, :console, format: "[$level] $message\\n"

    # Set a higher stacktrace during development
    config :phoenix, :stacktrace_depth, 20

    # Initialize plugs at runtime for faster development compilation
    config :phoenix, :plug_init_mode, :runtime

    # Disable swoosh api client as it is only required for production adapters
    config :swoosh, :api_client, false
    """
    
    EEx.eval_string(template, assigns: [
      app_name: project_spec.app_name,
      module_name: project_spec.module_name,
      secret_key_base: Base.encode64(:crypto.strong_rand_bytes(64))
    ])
  end

  defp render_prod_config(project_spec) do
    template = """
    import Config

    # For production, don't forget to configure the url host
    # to something meaningful, Phoenix uses this information
    # when generating URLs.

    config :<%= @app_name %>, <%= @module_name %>Web.Endpoint, cache_static_manifest: "priv/static/cache_manifest.json"

    # Configures Swoosh API Client
    config :swoosh, api_client: Swoosh.ApiClient.Finch, finch_name: <%= @module_name %>.Finch

    # Disable Swoosh Local Memory Storage
    config :swoosh, local: false

    # Do not print debug messages in production
    config :logger, level: :info

    # Runtime production configuration, including reading
    # of environment variables, is done on config/runtime.exs.
    """
    
    EEx.eval_string(template, assigns: [
      app_name: project_spec.app_name,
      module_name: project_spec.module_name
    ])
  end

  defp render_test_config(project_spec) do
    template = """
    import Config

    # Configure the database
    config :<%= @app_name %>, <%= @module_name %>.Repo,
      username: "postgres",
      password: "postgres",
      hostname: "localhost",
      database: "<%= @app_name %>_test\#{System.get_env("MIX_TEST_PARTITION")}",
      pool: Ecto.Adapters.SQL.Sandbox,
      pool_size: System.schedulers_online() * 2

    # Configure the endpoint
    config :<%= @app_name %>, <%= @module_name %>Web.Endpoint,
      http: [ip: {127, 0, 0, 1}, port: 4002],
      secret_key_base: "<%= @secret_key_base %>",
      server: false

    # In test we don't send emails
    config :<%= @app_name %>, <%= @module_name %>.Mailer, adapter: Swoosh.Adapters.Test

    # Disable swoosh api client as it is only required for production adapters
    config :swoosh, :api_client, false

    # Print only warnings and errors during test
    config :logger, level: :warning

    # Initialize plugs at runtime for faster test compilation
    config :phoenix, :plug_init_mode, :runtime

    # Enable helpful, but potentially expensive runtime checks
    config :phoenix_live_view,
      enable_expensive_runtime_checks: true
    """
    
    EEx.eval_string(template, assigns: [
      app_name: project_spec.app_name,
      module_name: project_spec.module_name,
      secret_key_base: Base.encode64(:crypto.strong_rand_bytes(64))
    ])
  end

  defp render_runtime_config(project_spec) do
    template = """
    import Config

    # config/runtime.exs is executed for all environments, including
    # during releases. It is executed after config/config.exs and after the
    # environment's specific config file is loaded.

    if System.get_env("PHX_SERVER") do
      config :<%= @app_name %>, <%= @module_name %>Web.Endpoint, server: true
    end

    if config_env() == :prod do
      database_url =
        System.get_env("DATABASE_URL") ||
          raise \"\"\"
          environment variable DATABASE_URL is missing.
          For example: ecto://USER:PASS@HOST/DATABASE
          \"\"\"

      maybe_ipv6 = if System.get_env("ECTO_IPV6") in ~w(true 1), do: [:inet6], else: []

      config :<%= @app_name %>, <%= @module_name %>.Repo,
        url: database_url,
        pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10"),
        socket_options: maybe_ipv6

      # The secret key base is used to sign/encrypt cookies and other secrets.
      # A default value is used in config/dev.exs and config/test.exs but you
      # want to use a different value for prod and you most likely don't want
      # to check this value into version control, so we use an environment
      # variable instead.
      secret_key_base =
        System.get_env("SECRET_KEY_BASE") ||
          raise \"\"\"
          environment variable SECRET_KEY_BASE is missing.
          You can generate one by calling: mix phx.gen.secret
          \"\"\"

      host = System.get_env("PHX_HOST") || "example.com"
      port = String.to_integer(System.get_env("PORT") || "4000")

      config :<%= @app_name %>, <%= @module_name %>Web.Endpoint,
        url: [host: host, port: 443, scheme: "https"],
        http: [
          # Enable IPv6 and bind on all interfaces.
          # Set it to  {0, 0, 0, 0, 0, 0, 0, 1} for local network only access.
          # See the documentation on https://hexdocs.pm/bandit/Bandit.html#t:options/0
          # for details about using IPv6 vs IPv4 and loopback vs public addresses.
          ip: {0, 0, 0, 0, 0, 0, 0, 0},
          port: port
        ],
        secret_key_base: secret_key_base

      # Configure Swoosh for email sending in production
      # config :<%= @app_name %>, <%= @module_name %>.Mailer,
      #   adapter: Swoosh.Adapters.Mailgun,
      #   api_key: System.get_env("MAILGUN_API_KEY"),
      #   domain: System.get_env("MAILGUN_DOMAIN")
    end
    """
    
    EEx.eval_string(template, assigns: [
      app_name: project_spec.app_name,
      module_name: project_spec.module_name
    ])
  end

  # Helper functions for rendering components

  defp render_middleware(:telemetry), do: "Reactor.Middleware.Telemetry"
  defp render_middleware(:semantic), do: "CNSForge.SemanticMiddleware"
  defp render_middleware(:ttl_enforcement), do: "CNSForge.TTLEnforcementMiddleware"
  defp render_middleware({module, opts}), do: "#{module}, #{inspect(opts)}"
  defp render_middleware(module), do: to_string(module)

  defp render_step(step, module_name) do
    case step.type do
      :transform ->
        """
        step :#{step.name}, #{module_name}.Steps.#{Macro.camelize(step.name)} do
          argument :input, input(:#{step.name}_input)
          
          run fn %{input: input} ->
            # #{step.description || "Step implementation"}
            {:ok, input}
          end
        end
        """
        
      :map ->
        """
        map :#{step.name} do
          source input(:#{step.name}_source)
          
          return :#{step.name}_result
          
          step :process_item, #{module_name}.Steps.ProcessItem do
            argument :item, element(:#{step.name})
          end
        end
        """
        
      _ ->
        """
        step :#{step.name} do
          run fn _args ->
            # #{step.description || "Generic step implementation"}
            {:ok, :completed}
          end
        end
        """
    end
  end

  defp render_action(action) do
    """
    action :#{action.name}, :#{action.type} do
      description "#{action.description || "Generated action"}"
      
      # Add action-specific configuration here
    end
    """
  end

  defp render_relationship(relationship, module_name) do
    case relationship.type do
      :belongs_to ->
        """
        belongs_to :#{relationship.name}, #{module_name}.Resources.#{relationship.resource}
        """
        
      :has_many ->
        """
        has_many :#{relationship.name}, #{module_name}.Resources.#{relationship.resource}
        """
        
      :has_one ->
        """
        has_one :#{relationship.name}, #{module_name}.Resources.#{relationship.resource}
        """
        
      _ ->
        """
        # Relationship: #{relationship.name} (#{relationship.type})
        """
    end
  end
end