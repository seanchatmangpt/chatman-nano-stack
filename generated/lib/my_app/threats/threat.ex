defmodule MyApp.Threats.Threat do
  use Ash.Resource,
    otp_app: :my_app,
    domain: MyApp.Threats

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
