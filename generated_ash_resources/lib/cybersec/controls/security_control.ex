defmodule Cybersec.Controls.SecurityControl do
  use Ash.Resource,
    otp_app: :cybersec,
    domain: Cybersec.Controls

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
