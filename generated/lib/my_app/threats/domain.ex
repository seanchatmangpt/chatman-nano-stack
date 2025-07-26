defmodule MyApp.Threats do
  use Ash.Domain,
    otp_app: :my_app

  resources do
    resource MyApp.Threats.Threat
    resource MyApp.Threats.Vulnerability
  end
end
