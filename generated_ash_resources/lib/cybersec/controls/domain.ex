defmodule Cybersec.Controls do
  use Ash.Domain,
    otp_app: :cybersec

  resources do
    resource Cybersec.Controls.SecurityControl
    resource Cybersec.Controls.SecurityIncident
  end
end
