defmodule Pipeline.Workflows.ErrorHandler do
  use Reactor
  
  input :error
  input :context
  
  step :analyze_error do
    argument :error, input(:error)
    argument :context, input(:context)
    
    run fn args, _context ->
      error_type = case args.error do
        %RuntimeError{} -> :runtime_error
        %ArgumentError{} -> :argument_error
        _ -> :unknown_error
      end
      
      {:ok, %{type: error_type, context: args.context}}
    end
  end
  
  step :create_recovery_plan do
    argument :analysis, result(:analyze_error)
    
    run fn args, _context ->
      plan = case args.analysis.type do
        :runtime_error -> %{action: :restart, delay: 1000}
        :argument_error -> %{action: :validate, delay: 0}
        _ -> %{action: :escalate, delay: 0}
      end
      
      {:ok, plan}
    end
  end
  
  return :create_recovery_plan
end
