defmodule Cybersec.Assets do
  use Ash.Domain,
    otp_app: :cybersec

  resources do
    resource Cybersec.Assets.Asset
    resource Cybersec.Assets.NetworkAsset
  end
end
