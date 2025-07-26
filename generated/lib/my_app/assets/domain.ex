defmodule MyApp.Assets do
  use Ash.Domain,
    otp_app: :my_app

  resources do
    resource MyApp.Assets.Asset
    resource MyApp.Assets.NetworkAsset
  end
end
