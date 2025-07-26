defmodule Cybersec.Assets.NetworkAsset do
  use Ash.Resource,
    otp_app: :cybersec,
    domain: Cybersec.Assets

  attributes do
    uuid_primary_key :id
    

    
    timestamps()
  end



  actions do
    defaults [:read, :destroy]
    
    create :create do
      primary? true
    end
    
    update :update do
      primary? true
    end
  end
end
