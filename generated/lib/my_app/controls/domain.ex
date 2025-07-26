defmodule MyApp.Controls do
  use Ash.Domain,
    otp_app: :my_app

  resources do
    resource MyApp.Controls.SecurityControl
    resource MyApp.Controls.SecurityIncident
  end
end
