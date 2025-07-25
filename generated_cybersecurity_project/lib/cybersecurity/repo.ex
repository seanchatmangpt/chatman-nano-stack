defmodule Cybersecurity.Repo do
  @moduledoc """
  Database repository for cybersecurity
  """
  
  use AshPostgres.Repo, otp_app: :cybersecurity
  
  def installed_extensions do
    ["uuid-ossp", "citext"]
  end
end
