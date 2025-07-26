defmodule Ultrathink.Repo do
  @moduledoc """
  Database repository for ultrathink
  """
  
  use AshPostgres.Repo, otp_app: :ultrathink
  
  def installed_extensions do
    ["uuid-ossp", "citext"]
  end
end
