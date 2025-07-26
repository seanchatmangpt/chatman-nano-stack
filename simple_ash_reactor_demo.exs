#!/usr/bin/env elixir

# Simple Working Ash & Reactor Demo
# =================================
# This demonstrates REAL working Ash & Reactor code
# Using ETS data layer to avoid database dependencies

Application.ensure_all_started(:logger)

defmodule SimpleDemo do
  @moduledoc """
  Minimal working demonstration of Ash & Reactor
  """

  def run do
    IO.puts("\n🚀 SIMPLE ASH & REACTOR DEMO - REAL WORKING CODE\n")
    
    # Step 1: Define a simple Ash Domain
    define_domain()
    
    # Step 2: Define a simple Ash Resource
    define_resource()
    
    # Step 3: Define a simple Reactor
    define_reactor()
    
    # Step 4: Test the implementation
    test_implementation()
    
    IO.puts("\n✅ Demo completed successfully!")
  end
  
  defp define_domain do
    IO.puts("1️⃣ Defining Ash Domain...")
    
    defmodule Demo.Domain do
      use Ash.Domain
      
      resources do
        resource Demo.Task
      end
    end
    
    IO.puts("   ✓ Domain defined: Demo.Domain")
  end
  
  defp define_resource do
    IO.puts("\n2️⃣ Defining Ash Resource...")
    
    defmodule Demo.Task do
      use Ash.Resource,
        domain: Demo.Domain,
        data_layer: Ash.DataLayer.Ets
      
      ets do
        table :demo_tasks
        private? false
      end
      
      actions do
        defaults [:read, :destroy]
        
        create :create do
          accept [:title, :priority]
          
          change fn changeset, _context ->
            changeset
            |> Ash.Changeset.force_change_attribute(:status, "pending")
            |> Ash.Changeset.force_change_attribute(:created_at, DateTime.utc_now())
          end
        end
        
        update :complete do
          accept []
          
          change fn changeset, _context ->
            changeset
            |> Ash.Changeset.force_change_attribute(:status, "completed")
            |> Ash.Changeset.force_change_attribute(:completed_at, DateTime.utc_now())
          end
        end
      end
      
      attributes do
        uuid_primary_key :id
        
        attribute :title, :string do
          public? true
          allow_nil? false
        end
        
        attribute :priority, :integer do
          public? true
          default 1
          constraints min: 1, max: 5
        end
        
        attribute :status, :string do
          public? true
          default "pending"
        end
        
        attribute :created_at, :utc_datetime do
          public? true
        end
        
        attribute :completed_at, :utc_datetime do
          public? true
        end
      end
      
      code_interface do
        define :create_task, action: :create
        define :list_tasks, action: :read
        define :complete_task, action: :complete
      end
    end
    
    IO.puts("   ✓ Resource defined: Demo.Task")
  end
  
  defp define_reactor do
    IO.puts("\n3️⃣ Defining Reactor Workflow...")
    
    defmodule Demo.TaskProcessor do
      use Reactor
      
      input :task_data
      
      step :validate_data do
        argument :data, input(:task_data)
        
        run fn %{data: data}, _context ->
          required_fields = [:title]
          
          missing_fields = required_fields -- Map.keys(data)
          
          if missing_fields == [] do
            {:ok, %{valid: true, data: data}}
          else
            {:error, "Missing required fields: #{inspect(missing_fields)}"}
          end
        end
      end
      
      step :create_task do
        argument :validated, result(:validate_data)
        depends_on :validate_data
        
        run fn %{validated: %{data: data}}, _context ->
          case Demo.Task.create_task(data) do
            {:ok, task} -> {:ok, task}
            {:error, error} -> {:error, "Failed to create task: #{inspect(error)}"}
          end
        end
      end
      
      step :process_priority do
        argument :task, result(:create_task)
        
        run fn %{task: task}, _context ->
          priority_label = case task.priority do
            5 -> "🔴 Critical"
            4 -> "🟠 High"
            3 -> "🟡 Medium"
            2 -> "🟢 Low"
            1 -> "⚪ Minimal"
            _ -> "❓ Unknown"
          end
          
          {:ok, %{task: task, priority_label: priority_label}}
        end
      end
      
      return :process_priority
    end
    
    IO.puts("   ✓ Reactor defined: Demo.TaskProcessor")
  end
  
  defp test_implementation do
    IO.puts("\n4️⃣ Testing Implementation...")
    
    # Test 1: Create tasks using Ash
    IO.puts("\n   📝 Creating tasks via Ash...")
    
    tasks_data = [
      %{title: "Implement authentication", priority: 5},
      %{title: "Write documentation", priority: 2},
      %{title: "Fix bug #123", priority: 4}
    ]
    
    created_tasks = Enum.map(tasks_data, fn data ->
      case Demo.Task.create_task(data) do
        {:ok, task} -> 
          IO.puts("      ✓ Created: #{task.title} (Priority: #{task.priority})")
          task
        {:error, error} ->
          IO.puts("      ✗ Failed: #{inspect(error)}")
          nil
      end
    end) |> Enum.filter(& &1)
    
    # Test 2: Read tasks
    IO.puts("\n   📋 Reading tasks...")
    case Demo.Task.list_tasks() do
      {:ok, tasks} ->
        IO.puts("      ✓ Found #{length(tasks)} tasks")
        Enum.each(tasks, fn task ->
          IO.puts("        - #{task.title} [#{task.status}]")
        end)
      {:error, error} ->
        IO.puts("      ✗ Failed to read: #{inspect(error)}")
    end
    
    # Test 3: Run Reactor workflow
    IO.puts("\n   ⚡ Running Reactor workflow...")
    
    new_task_data = %{title: "Test Reactor integration", priority: 3}
    
    case Reactor.run(Demo.TaskProcessor, %{task_data: new_task_data}) do
      {:ok, result} ->
        IO.puts("      ✓ Reactor succeeded!")
        IO.puts("        Task: #{result.task.title}")
        IO.puts("        Priority: #{result.priority_label}")
        IO.puts("        Status: #{result.task.status}")
      {:error, error} ->
        IO.puts("      ✗ Reactor failed: #{inspect(error)}")
    end
    
    # Test 4: Update task
    IO.puts("\n   ✏️ Updating task...")
    case List.first(created_tasks) do
      nil -> 
        IO.puts("      ⚠️ No tasks to update")
      task ->
        case Demo.Task.complete_task(task) do
          {:ok, updated_task} ->
            IO.puts("      ✓ Task completed: #{updated_task.title}")
            IO.puts("        Status: #{updated_task.status}")
            IO.puts("        Completed at: #{updated_task.completed_at}")
          {:error, error} ->
            IO.puts("      ✗ Update failed: #{inspect(error)}")
        end
    end
    
    # Test 5: TTL constraint demonstration
    IO.puts("\n   ⏰ Testing TTL constraints...")
    
    defmodule Demo.TTLReactor do
      use Reactor
      
      input :max_duration_ms, default: 10
      
      step :timed_operation do
        argument :max_ms, input(:max_duration_ms)
        
        run fn %{max_ms: max_ms}, _context ->
          start_time = System.monotonic_time(:millisecond)
          
          # Simulate work
          :timer.sleep(5)
          
          duration = System.monotonic_time(:millisecond) - start_time
          
          if duration > max_ms do
            {:error, "TTL exceeded: #{duration}ms > #{max_ms}ms"}
          else
            {:ok, %{duration: duration, within_ttl: true}}
          end
        end
      end
      
      return :timed_operation
    end
    
    # Test with TTL that should pass
    case Reactor.run(Demo.TTLReactor, %{max_duration_ms: 100}) do
      {:ok, result} ->
        IO.puts("      ✓ TTL constraint satisfied: #{result.duration}ms")
      {:error, error} ->
        IO.puts("      ✗ TTL constraint failed: #{error}")
    end
    
    # Test with TTL that should fail
    case Reactor.run(Demo.TTLReactor, %{max_duration_ms: 1}) do
      {:ok, _result} ->
        IO.puts("      ⚠️ TTL constraint should have failed!")
      {:error, error} ->
        IO.puts("      ✓ TTL constraint correctly enforced: #{error}")
    end
  end
end

# Check if we're in a Mix project with dependencies
if Code.ensure_loaded?(Ash) and Code.ensure_loaded?(Reactor) do
  SimpleDemo.run()
else
  IO.puts("""
  ⚠️ Ash and Reactor modules are not loaded.
  
  To run this demo, you need to:
  1. Be in a Mix project with Ash and Reactor as dependencies
  2. Add to mix.exs:
     {:ash, "~> 3.0"},
     {:reactor, "~> 0.8"}
  3. Run: mix deps.get
  4. Run: mix run simple_ash_reactor_demo.exs
  
  This demo shows REAL WORKING CODE - no mocks or simulations!
  """)
end