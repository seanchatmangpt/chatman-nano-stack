defmodule MyApp.Assets.Asset do
  use Ash.Resource,
    otp_app: :my_app,
    domain: MyApp.Assets

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
