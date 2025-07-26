defmodule Cybersec.Threats do
  use Ash.Domain,
    otp_app: :cybersec

  resources do
    resource Cybersec.Threats.Threat
    resource Cybersec.Threats.Vulnerability
  end
end
