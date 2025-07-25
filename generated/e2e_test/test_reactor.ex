defmodule TestService.Reactor do
  use Ash.Reactor
  
  input :request
  
  step :validate do
    run fn input, _context ->
      {:ok, Map.put(input, :validated, true)}
    end
  end
  
  step :process do
    run fn input, _context ->
      {:ok, Map.put(input, :processed, true)}
    end
  end
  
  step :complete do
    run fn input, _context ->
      {:ok, Map.put(input, :completed, true)}
    end
  end
end