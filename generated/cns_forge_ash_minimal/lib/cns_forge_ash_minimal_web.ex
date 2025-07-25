defmodule CnsForgeAshMinimalWeb do
  @moduledoc """
  The entrypoint for defining your web interface, such
  as controllers, components, channels, and so on.
  """

  def controller do
    quote do
      use Phoenix.Controller,
        formats: [:html, :json],
        layouts: [html: CnsForgeAshMinimalWeb.Layouts]

      import Plug.Conn
      import CnsForgeAshMinimalWeb.Gettext

      unquote(verified_routes())
    end
  end

  def verified_routes do
    quote do
      use Phoenix.VerifiedRoutes,
        endpoint: CnsForgeAshMinimalWeb.Endpoint,
        router: CnsForgeAshMinimalWeb.Router,
        statics: CnsForgeAshMinimalWeb.static_paths()
    end
  end

  @doc """
  When used, dispatch to the appropriate controller/view/etc.
  """
  defmacro __using__(which) when is_atom(which) do
    apply(__MODULE__, which, [])
  end
end