defmodule CnsWeb.Endpoint do
  @moduledoc """
  Phoenix Endpoint configuration for CNS Swarm with optimized WebSocket handling.
  """
  
  use Phoenix.Endpoint, otp_app: :cns
  
  # The session will be stored in the cookie and signed,
  # this means its contents can be read but not tampered with.
  # Set :encryption_salt if you would also like to encrypt it.
  @session_options [
    store: :cookie,
    key: "_cns_key",
    signing_salt: "swarm_salt",
    same_site: "Lax"
  ]
  
  # WebSocket configuration optimized for swarm communications
  socket "/socket", CnsWeb.UserSocket,
    websocket: [
      timeout: 45_000,  # 45 seconds
      transport_log: false,
      serializer: [{CnsWeb.SwarmSocketSerializer, "~> 1.0"}],
      # Enable compression for 80/20 optimization
      compress: true,
      # Custom check origin for security
      check_origin: [
        "//localhost",
        "//127.0.0.1",
        "//[::1]",
        "//cns.local"
      ]
    ],
    longpoll: [
      timeout: 20_000,  # 20 seconds
      transport_log: false,
      # Custom serializer for longpoll fallback
      serializer: [{Phoenix.Socket.V1.JSONSerializer, "~> 1.0"}]
    ]
  
  # Serve at "/" the static files from "priv/static" directory.
  #
  # You should set gzip to true if you are running phx.digest
  # when deploying your static files in production.
  plug Plug.Static,
    at: "/",
    from: :cns,
    gzip: false,
    only: CnsWeb.static_paths()
  
  # Code reloading can be explicitly enabled under the
  # :code_reloader configuration of your endpoint.
  if code_reloading? do
    socket "/phoenix/live_reload/socket", Phoenix.LiveReloader.Socket
    plug Phoenix.LiveReloader
    plug Phoenix.CodeReloader
    plug Phoenix.Ecto.CheckRepoStatus, otp_app: :cns
  end
  
  plug Phoenix.LiveDashboard.RequestLogger,
    param_key: "request_logger",
    cookie_key: "request_logger"
  
  plug Plug.RequestId
  plug Plug.Telemetry, event_prefix: [:phoenix, :endpoint]
  
  # CORS configuration for frontend integration
  plug Corsica,
    origins: [
      "http://localhost:3000",
      "http://localhost:8080",
      "https://cns.local"
    ],
    allow_headers: [
      "accept",
      "authorization",
      "content-type",
      "origin",
      "x-swarm-id",
      "x-optimization-mode"
    ]
  
  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Phoenix.json_library()
  
  plug Plug.MethodOverride
  plug Plug.Head
  plug Plug.Session, @session_options
  
  # Custom plugs for swarm optimization
  plug CnsWeb.Plugs.SwarmOptimization
  plug CnsWeb.Plugs.MetricsCollection
  
  plug CnsWeb.Router
end

defmodule CnsWeb.SwarmSocketSerializer do
  @moduledoc """
  Custom serializer for swarm WebSocket messages with 80/20 optimization.
  """
  
  @behaviour Phoenix.Socket.Serializer
  
  alias Phoenix.Socket.{Broadcast, Message, Reply}
  
  def fastlane!(%Broadcast{} = msg) do
    # Optimize broadcast serialization for swarm channels
    data = %{
      topic: msg.topic,
      event: msg.event,
      payload: optimize_payload(msg.payload)
    }
    
    {:socket_push, :text, Phoenix.json_library().encode!(data)}
  end
  
  def encode!(%Reply{} = reply) do
    data = %{
      topic: reply.topic,
      event: "phx_reply",
      payload: %{
        status: reply.status,
        response: optimize_payload(reply.payload)
      },
      ref: reply.ref
    }
    
    {:socket_push, :text, Phoenix.json_library().encode!(data)}
  end
  
  def encode!(%Message{} = msg) do
    data = %{
      topic: msg.topic,
      event: msg.event,
      payload: optimize_payload(msg.payload),
      ref: msg.ref
    }
    
    {:socket_push, :text, Phoenix.json_library().encode!(data)}
  end
  
  def decode!(raw_message, _opts) do
    [opcode | payload] = String.split(raw_message, " ", parts: 2)
    
    case opcode do
      "heartbeat" ->
        :heartbeat
        
      _ ->
        %{
          "topic" => topic,
          "event" => event,
          "payload" => payload,
          "ref" => ref
        } = Phoenix.json_library().decode!(raw_message)
        
        %Message{
          topic: topic,
          event: event,
          payload: payload,
          ref: ref
        }
    end
  end
  
  # 80/20 payload optimization
  defp optimize_payload(payload) when is_map(payload) do
    # Remove non-critical fields for 80/20 optimization
    critical_fields = [
      "id", "status", "stage", "type", "level", "critical",
      "metrics", "error", "result", "optimization"
    ]
    
    # If optimization flag is set, filter to critical fields only
    if Map.get(payload, "optimization") == "80_20" do
      payload
      |> Map.take(critical_fields)
      |> compress_metrics()
    else
      payload
    end
  end
  
  defp optimize_payload(payload), do: payload
  
  defp compress_metrics(%{"metrics" => metrics} = payload) when is_map(metrics) do
    # Keep only essential metrics for 80/20 mode
    essential_metrics = Map.take(metrics, [
      "cpu", "memory", "latency", "error_rate", "throughput"
    ])
    
    Map.put(payload, "metrics", essential_metrics)
  end
  
  defp compress_metrics(payload), do: payload
end