Architectural Analysis and Implementation Strategy for the CNS Forge PrototypeExecutive SummaryThis report presents a comprehensive architectural analysis and implementation strategy for the CNS Forge Prototype, as defined in the Product Requirements Document (PRD). It validates the feasibility of realizing the prototype's ambitious vision using the Elixir Ash Reactor ecosystem. The core thesis of this analysis is that the proposed architecture not only meets but strategically enhances the PRD's requirements, establishing a robust foundation for the "Technology Applications, Inc. Vision 2026." The conceptual "BitActor Mesh" finds a powerful and pragmatic implementation as a set of logical workflows orchestrated by Ash.Reactor, achieving the PRD's goal of a "fabric of provably correct, self-optimizing systems."The analysis reveals a profound synergy between the PRD's core principles—Bit-Level Atomicity, Ephemeral Execution, and Universal Observability—and the foundational capabilities of the Elixir/OTP platform and the Ash Framework. Ash.Reactor, a dynamic, concurrent, dependency-resolving saga orchestrator, is identified as the ideal engine for this system.1 It provides the necessary dependency-driven concurrency, saga-based compensation for atomicity, and a structured, declarative environment for implementing the logic of the conceptual "BitActor."The most significant implementation challenge identified is the stateful, hop-based Time-To-Live (TTL) mechanism, a feature not native to Elixir or Reactor. This report proposes a robust and elegant pattern to solve this by managing the TTL as explicit state within the data "token" that flows through the Reactor workflow, complemented by Elixir's native process timers for hard deadline enforcement. This approach fully satisfies the PRD's requirements for guaranteed forward progress and the prevention of system stalls.Crucially, this analysis extends beyond the immediate prototype. The selection of the Ash ecosystem provides a direct and powerful path to achieving the broader "Vision 2026." Through extensions like AshAI for intelligent agent-driven orchestration and AshEvents for comprehensive event sourcing, the proposed architecture is not merely a technical solution but a highly strategic technology choice that de-risks future development and aligns perfectly with the long-term goal of composing an entire "digital reality."Based on this exhaustive analysis, the primary recommendation is to proceed with the Elixir Ash Reactor ecosystem for the CNS Forge Prototype. The proposed architecture is demonstrably capable of meeting all specified requirements while providing a scalable, maintainable, and future-proof foundation for the ambitious vision ahead.1. Introduction: Realizing the CNS Forge Vision with a Declarative, Resource-Oriented ArchitectureThe CNS Forge PRD articulates a vision that transcends conventional application development. It describes an "ecosystem composer" designed to orchestrate a "digital reality" by translating high-level directives into a self-optimizing fabric of systems. This vision demands an architectural foundation that is not only powerful and scalable but also inherently composable, verifiable, and introspectable. The Elixir Ash ecosystem, with Ash.Reactor at its core, provides precisely this foundation.1.1 The CNS Forge as an Ecosystem Composer: An InterpretationThe PRD's description of the CNS Forge as an "ecosystem composer" suggests a system that functions as a meta-framework for assembling business capabilities, rather than being a single, monolithic application. The goal of orchestrating a "digital reality" implies the need for a system that can model complex business logic not as opaque, imperative code, but as transparent, composable, and introspectable data structures. This interpretation aligns perfectly with the central philosophy of the Ash Framework: "model your domain, derive the rest".3Ash is explicitly designed to act as the "spinal cord" for an application layer, providing structure and coordination while allowing for complete customization and integration with other systems.5 The framework achieves this by modeling an application's behavior through Resources and their Actions. An Action is a declarative, fully-typed representation of a unit of business logic (e.g., :create_user, :publish_post). A Resource groups these related actions around a domain concept (e.g., User, Post).5This declarative approach is the key to realizing the "ecosystem composer" vision. Because Ash Actions are data, they are introspectable at runtime. This allows other systems and extensions to understand and build functionality around them automatically. For instance, the AshGraphql extension can read the definition of a create action and generate a corresponding GraphQL mutation, AshJsonApi can generate a REST endpoint, and AshPostgres can handle database persistence—all without requiring the developer to write duplicative integration code.3 The CNS Forge, therefore, can be built not just to execute logic, but to understand its own logic, a prerequisite for the "self-optimizing" systems described in the vision. This philosophical alignment is further reinforced by Ash's own vibrant ecosystem of extensions for APIs, data layers, authentication, background jobs, and AI, demonstrating its proven capacity to compose complex systems from modular parts.41.2 The Ash Framework and Reactor: A Foundation for Provably Correct, Composable SystemsThe PRD's requirement for a "fabric of provably correct" systems necessitates a framework that prioritizes consistency, atomicity, and verifiability. The combination of Ash's declarative resources and Reactor's orchestration capabilities provides a powerful solution.Ash.Reactor is a library designed to orchestrate complex, multi-step workflows, particularly those that span multiple services or require transactional guarantees beyond what a single database can offer.10 It is described as a "dynamic, concurrent, dependency resolving saga orchestrator".1 This addresses the CNS Forge's need to translate "high-level, outcome-based directives" into a coherent series of actions. The core mechanism that enables this is the saga pattern. A saga is a sequence of local transactions where each step can be compensated for if a subsequent step fails. This provides robust, transaction-like semantics across distributed systems, which is essential for composing a "provably correct" fabric of services.2The synergy between Ash and Reactor is what makes this architecture so compelling. Ash provides the declarative building blocks—the "what." A developer defines an Ash.Resource with actions like create_user, create_stripe_customer, and send_welcome_email. Reactor then provides the orchestration layer—the "how" and "when." A developer defines a Reactor workflow that specifies the dependencies between these actions. For example, it can declare that create_user and create_stripe_customer can run concurrently, but send_welcome_email must wait for the user to be successfully created.10 This separation of concerns—declarative business logic in Ash, imperative orchestration in Reactor—creates a system that is both highly expressive and rigorously controlled, directly fulfilling the core requirements of the CNS Forge vision.2. Architectural Blueprint: Mapping CNS Forge Principles to the Ash EcosystemTranslating the abstract principles of the CNS Forge PRD into a concrete implementation requires a careful mapping of its unique concepts—the BitActor, Bit-Level Atomicity, and the BitActor Mesh—to the capabilities of the Ash ecosystem. This section provides a detailed blueprint for this translation, arguing that a logical, rather than literal, interpretation yields the most robust and idiomatic architecture.2.1 The BitActor Paradigm: A Comparative Analysis of Elixir Processes and Reactor StepsThe PRD's central abstraction is the BitActor, an "ephemeral, time-limited, single-purpose process that encapsulates one atomic hop of logic." At first glance, this concept bears a strong resemblance to the core Actor Model processes that underpin Elixir and the Erlang VM, such as GenServer for stateful servers or Task for one-off computations.14 These BEAM processes are famously lightweight, concurrent, and isolated, making them a plausible candidate for implementing BitActors.However, a literal, one-to-one mapping of a BitActor to a BEAM process would be architecturally suboptimal. The PRD's emphasis on "picosecond-resolution observability" and "massive micro-concurrency" for every "atomic hop of logic" suggests a level of granularity that would make spawning a distinct OS-level process (even a lightweight BEAM process) for each logical step inefficient and impractical. The overhead of process creation, scheduling, and garbage collection, while minimal, would become significant at the scale implied by the vision.A more effective and idiomatic approach is to treat the BitActor as a logical construct implemented by a Reactor.Step. The Reactor.Step behaviour is a perfect fit:It encapsulates a single unit of work within its run/3 callback.It supports compensating logic via compensate/4 and undo logic via undo/4, directly aligning with the requirement for an "atomic hop."It is designed to be composed into larger workflows orchestrated by the Reactor engine.2This logical mapping allows the system to achieve the desired "massive micro-concurrency" at the level of Reactor's internal task supervisor, which can execute thousands of steps concurrently without the overhead of spawning an equal number of BEAM processes. The BitActor, therefore, becomes a conceptual unit of logic, while the Reactor.Step becomes its concrete, efficient implementation. This honors the spirit of the BitActor—its atomicity and single-purpose nature—without incurring the performance penalty of a literal interpretation. The name "BitActor" itself signals a desire for a level of determinism and control that transcends typical application logic. It implies that the fundamental unit of computation should be as small and predictable as a bit flip. While a literal bit-level process is not feasible, the combination of Elixir's deterministic functional core (where pure functions have no side effects) and Reactor's explicit state-in/state-out steps provides the closest practical architectural equivalent to this ideal.162.2 Achieving Bit-Level Atomicity and Verifiability through Reactor's Saga OrchestrationThe PRD's principle of "Bit-Level Atomicity" is precisely what the saga orchestration pattern, as implemented by Ash.Reactor, is designed to provide. A traditional database transaction provides atomicity within a single data source, but the CNS Forge vision requires composing systems that may interact with multiple databases, external APIs, and other services. This is where the saga pattern excels.10A saga is a sequence of local transactions, where each Reactor.Step represents one such transaction. Reactor ensures that if any step in the sequence fails, it can execute compensating actions for all previously completed steps, effectively rolling the entire distributed transaction back to a consistent state.2This is implemented through the Reactor.Step behaviour's callbacks:run(arguments, context, options): Executes the primary logic of the step.compensate(error, arguments, context, options): Called when the run function itself fails. It can decide whether to retry the step or signal that the failure is permanent.undo(result, arguments, context, options): Called when a downstream step fails. Its job is to reverse the effects of a successful run operation.For example, a CreateStripeSubscriptionStep would have a run function that calls the Stripe API. Its undo function would call the Stripe API to delete that same subscription. If a later step, like SendWelcomeEmailStep, fails, Reactor will automatically invoke the undo function on the subscription step, ensuring no customer is billed without receiving a welcome email. This provides a robust, verifiable, and distributed form of atomicity that is essential for a "provably correct" system.2.3 Modeling the BitActor Mesh with Reactor's Concurrent Dependency GraphThe "BitActor Mesh" is described as a network of interconnected BitActors. This concept is directly and elegantly implemented by Reactor's core execution model: a Directed Acyclic Graph (DAG).2When a developer defines a workflow using the Reactor DSL, they specify the steps and declare the dependencies between them by defining what inputs each step requires. A step's inputs can be the initial arguments to the workflow or the results of other steps.12 Reactor analyzes these dependencies and builds a DAG. Its execution engine then identifies all nodes in the graph with no inbound edges (i.e., steps whose dependencies are all met) and runs them concurrently. As steps complete, their results become available, satisfying the dependencies of downstream steps, which are then scheduled for execution.This model perfectly realizes the "mesh" concept. For instance, in a user registration workflow, the steps :register_user_in_db, :create_stripe_customer, and :find_stripe_plan might all be defined with inputs that come directly from the initial workflow arguments. Reactor will recognize that they have no dependencies on each other and execute all three simultaneously. A subsequent step, :create_stripe_subscription, which requires the results of all three, will only be executed after they have all successfully completed.11 This automatic concurrency, derived from a declarative description of dependencies, is a powerful implementation of the interconnected, self-organizing mesh envisioned in the PRD.PRD ConceptDescriptionAsh/Reactor ImplementationKey SourcesCNS ForgeAn "ecosystem composer" for orchestrating a digital reality.A top-level Ash.Domain that contains and orchestrates multiple Ash.Reactor workflows.3BitActorAn ephemeral, time-limited, single-purpose process for one atomic hop of logic.A logical construct implemented as a module that uses the Reactor.Step behaviour.2Bit-Level AtomicityGuarantee that each logical hop is atomic and the system remains consistent.The Reactor Saga Pattern, using the run, compensate, and undo callbacks in a Reactor.Step.10BitActor MeshA mesh of interconnected BitActors.Reactor's dependency-based Directed Acyclic Graph (DAG) execution model, enabling concurrent steps.2TTL-Driven ExecutionProcesses with a defined Time-To-Live (TTL) in logical hops.A custom pattern where the TTL is passed as explicit state in the "token" between Reactor.Steps, combined with Process.send_after for wall-clock timeouts.17Pulse LogA telemetry record for every BitActor state transition.Custom :telemetry events emitted by a Reactor.Middleware that wraps each step, leveraging Ash.Tracer.19SignalThe data payload passed between BitActors.An immutable Elixir map or struct (the "token") passed between Reactor.Steps via input() and result() helpers.113. Core Feature Implementation StrategyThe PRD outlines three key epics that form the functional core of the CNS Forge prototype. This section details a robust implementation strategy for each, mapping the requirements to specific patterns and features within the Elixir Ash ecosystem.3.1 Epic 1: Ephemeral, TTL-Driven ExecutionThis epic introduces the most unique and challenging requirement: a system driven by ephemeral processes with a defined Time-To-Live (TTL). This forces a specific architectural style that prioritizes bounded execution contexts. A hard limit on logical "hops," such as the "8 Hops" principle mentioned in the success criteria, is an architectural guardrail. It compels developers to design shallow, wide workflows rather than deep, complex call chains that are difficult to reason about. This constraint encourages a more event-driven or message-passing style, where one short-lived, bounded workflow completes and emits a new signal to trigger another. This reframes the TTL requirement from a simple timeout mechanism to a strategic tool for enforcing architectural discipline and managing complexity.3.1.1 A Robust Pattern for Implementing Time-To-Live (TTL) on Reactor StepsNeither Elixir's native processes nor Reactor's steps have a built-in concept of a decrementing, hop-based TTL.21 Therefore, a custom pattern is required. The recommended approach is to manage the TTL as an explicit piece of state within the data "token" that flows through the Reactor workflow.This pattern directly addresses Requirements 1.1, 1.2, and 1.3:Initialization (Req. 1.1): The initial stimulus BitActor (e.g., the Phoenix controller handling the HTTP request) will be responsible for creating the initial token. This token will be an Elixir map containing the request payload and an initial TTL value, for example: %{params: %{...}, ttl: 8, transaction_id: "..."}.Decrementation (Req. 1.2): Each Reactor.Step (representing a BitActor hop) will be responsible for decrementing the TTL. Its run/3 function will receive the token, perform its logic, decrement the ttl key in the token map, and pass the updated token in its return value, e.g., {:ok, %{result_data: "...", ttl: 7,...}}.Termination (Req. 1.3): Before executing its core logic, each step will check the incoming TTL. If the TTL is zero or less, the step will immediately terminate its branch of the workflow by returning an error. For example: if token.ttl <= 0, do: {:error, :ttl_expired}, else: #... proceed with logic. The Reactor orchestrator will catch this error. A custom Reactor.Middleware can be designed to intercept this specific error type and emit the required ttl_expired tombstone signal to the telemetry mesh.20This pattern is clean, robust, and aligns with the functional programming principles of Elixir by avoiding hidden state and managing the TTL explicitly as data.3.1.2 Guaranteeing Forward Progress and Preventing StallsThe logical, hop-based TTL must be complemented by real-world, time-based timeouts to prevent system stalls caused by long-running external calls or unresponsive services. The Ash ecosystem provides multiple layers for managing this.Action-Level Timeouts: When a Reactor.Step calls an Ash.Action (e.g., to query a database), it can leverage Ash's built-in timeout mechanisms. Timeouts can be configured globally on the Ash.Domain, or specified per-request using Ash.Query.timeout/2 or Ash.Changeset.timeout/2.23Generic Step Timeouts: For steps that perform generic Elixir code, such as making an HTTP call to an external API, standard OTP patterns should be used. Task.yield/2 allows a process to wait for a task to complete for a specified duration, returning nil on timeout.17 For synchronous interactions that need to be bounded, GenServer.call/3 accepts a timeout parameter.24Hard Deadline Enforcement: To enforce an absolute wall-clock deadline for an entire workflow, a more powerful OTP pattern can be employed. When the workflow is initiated, the spawner process can use :timer.kill_after/2 to send a :kill signal to the orchestrating Reactor process if it does not complete within the specified time.17 This acts as a definitive backstop, ensuring that no process can run away indefinitely, thus guaranteeing forward progress for the system as a whole.3.2 Epic 2: Atomic, Single-Hop LogicThis epic focuses on ensuring that each unit of work is a verifiable, self-contained atom, enabling precise debugging and replay.3.2.1 Encapsulating Logic within Reactor.Step BehavioursRequirement 2.1 specifies an init/1 function for a BitActor. In this architecture, this function will be the public API entry point that initiates the workflow. It will take an input token and use it to start the Reactor execution via Reactor.run/2. Each subsequent logical hop will be implemented as a distinct module that use Reactor.Step.11 The core logic resides within the run/3 callback of each step, which accepts the input token and produces an output token and a new TTL, precisely as required.3.2.2 Ensuring State Purity and Replayability via Serializable TokensRequirement 2.2—that a BitActor's state must be fully contained within the token it carries—is naturally met by this architecture. The "token" is an immutable Elixir map or struct. Because Elixir data structures are immutable, passing the token from one step to the next inherently prevents hidden state modifications and side effects. The state of the entire transaction at any point in the workflow is simply the value of the token at that point.This directly enables Requirement 2.3: serializability for time-travel debugging. Any Elixir term, including the map used as a token, can be easily serialized using built-in functions like :erlang.term_to_binary/1 or converted to formats like JSON. By logging the token at each hop (as specified in Epic 3), the entire state transition can be captured. To replay or debug, one can simply re-run the Reactor workflow, feeding it the logged token from a specific point in the original execution.3.3 Epic 3: Universal Instrumentation & ObservabilityThis epic demands complete, bit-level traceability for every signal, a requirement that aligns strongly with the extensive telemetry capabilities built into the Elixir ecosystem.3.3.1 Generating Pulse Logs with Ash Telemetry HooksThe "pulse log" specified in Requirement 3.1 is a perfect use case for Elixir's :telemetry library, which is deeply integrated into Ash.19 The most idiomatic and efficient way to implement this is not by manually instrumenting every BitActor, but by using a Reactor.Middleware.20A custom middleware module can be written to implement callbacks like before_step and after_step.The after_step callback will be invoked by Reactor after every single step completes.Inside this callback, a custom telemetry event, such as [:cns_forge, :bit_actor, :hop], will be executed.The event's payload will contain all the information required by Requirement 3.2: the BitActor's unique ID (the step name), its type, the input token, the output token, and the remaining TTL.This centralized approach ensures that every single hop is instrumented universally and consistently without cluttering the business logic of the individual steps.3.3.2 Reconstructing Causal Chains for High-Fidelity DiagnosticsTo meet Requirement 3.3, the system must be able to reconstruct the full causal chain of BitActors for any transaction. This is achieved by implementing robust correlation IDs.Transaction ID: When the initial stimulus BitActor is spawned (e.g., from the HTTP request), a unique transaction_id (e.g., a UUID) will be generated. This ID will be added to the initial token.Correlation in Token: Because the token is passed immutably through every step in the chain, every Reactor.Step will have access to this transaction_id.Logging: The Reactor.Middleware responsible for telemetry will include the transaction_id in the metadata of every "pulse log" event it emits.Reconstruction: The telemetry events are consumed by a logging backend (e.g., Logstash, Splunk, or a custom :telemetry handler that writes to a database). To reconstruct a transaction's journey, one simply needs to query the backend for all log entries matching a given transaction_id. The logs, when sorted by their timestamp, provide a complete, hop-by-hop replay of the entire decision-making process, from signal ingress to final actuation.4. System Integration and Technical WiringThe CNS Forge Prototype requires integration with several layers of a typical application stack, from the web server to the database. This section details the technical "wiring" necessary to connect these components using the Ash ecosystem, creating the cohesive BitActor Mesh.4.1 Ingress Layer: Translating Cowboy HTTP Requests into Reactor WorkflowsThe PRD specifies integration with Cowboy, the underlying web server for the popular Phoenix framework. The most robust and idiomatic way to handle this is to leverage Phoenix and the ash_phoenix integration package.4The implementation pattern is as follows:A Phoenix router will map an incoming HTTP request (e.g., POST /api/orders) to a specific controller action.The controller's role is minimal. It acts as a thin translation layer, responsible for extracting parameters from the request and creating the initial "token" for the CNS Forge. This token will include the payload, the initial TTL, and a newly generated transaction_id.The controller then invokes a generic Ash.Resource action. This is the preferred way to initiate Reactor workflows within an Ash application, as it wraps the entire operation within Ash's comprehensive ecosystem of authorization, transaction management, and telemetry.12This generic action's run implementation will be the Reactor module itself. For example: action :place_order, :struct do... run MyApp.Workflows.PlaceOrderReactor end.This pattern cleanly separates the concerns of the web layer (HTTP protocol handling) from the business logic layer (the Reactor workflow), creating a stimulus:http_request BitActor at the system's boundary.4.2 Workflow Engine: A Declarative Approach with Ash.ReactorThe PRD calls for replacing the existing gen_bpm workflow engine. Ash.Reactor is a modern and vastly superior alternative. The requirement to auto-generate bitactor_bpm_node modules is conceptually fulfilled, and improved upon, by Ash's declarative DSL.Instead of relying on imperative code generation, developers will define workflow logic using the high-level Ash.Reactor DSL within a Reactor module.12 Each logical node (decision, task, etc.) from the old BPM model maps directly to a Reactor.Step. These steps are declarative, mapping to business-centric Ash actions like create, read_one, update, or custom generic actions.For example, a decision node in gen_bpm becomes an Ash.Reactor.read_one step that fetches some data, followed by a generic step with conditional logic that determines which downstream steps to enable. This approach is more readable, more maintainable, and less error-prone than managing a collection of generated modules, as the entire workflow is defined in a single, cohesive file.4.3 Memory Layer: Transactional State Management with the Mnesia Data LayerThe PRD requires the use of Mnesia or ETS for the memory layer, with all access mediated by atomic BitActors. The Ash framework provides first-class, built-in support for both through its data layer abstractions.32Configuration: To use Mnesia for a given resource, a developer simply adds data_layer: Ash.DataLayer.Mnesia to the use Ash.Resource macro.34 Ash handles all the underlying complexity of interacting with the Mnesia database.Transactional Integrity: A key advantage of Mnesia over raw ETS is its native support for ACID transactions.35 This is critical for fulfilling the atomicity requirements of the CNS Forge. Ash.Reactor integrates seamlessly with this feature via its transaction step. A block of Reactor.Steps that perform Ash actions on Mnesia-backed resources can be wrapped in a reactor.transaction block. This ensures that all the database operations within that block are executed inside a single Mnesia transaction, which will be committed atomically or fully rolled back on any failure.12This powerful combination directly implements the PRD's requirement for memory access to be mediated by atomic, serializable signals, as each database operation is simply a step within a transactional Reactor workflow. While Mnesia is excellent for this purpose, it is important to recognize its operational characteristics: it is designed for smaller datasets on a limited number of nodes and is not a direct replacement for a large-scale SQL database like PostgreSQL.354.4 Signal Routing: A Comparative Analysis of Elixir's Registry and gprocTo route signals between BitActors (processes), the PRD specifies the use of a process registry. In the Elixir ecosystem, the two primary candidates for this role are the built-in Registry module and the established Erlang library gproc. A careful analysis indicates that for the CNS Forge prototype, Registry is the superior choice.Registry is a modern, high-performance, and scalable key-value process store designed specifically for local (single-node) registration.37 In contrast, gproc is an older, more feature-rich library that supports both local and global (multi-node) registration, as well as advanced features like property storage and aggregated counters.39While gproc's feature set is extensive, the CNS Forge prototype, operating as a single logical mesh, does not require distributed registration. In the context of local registration, Registry offers significant advantages in performance and simplicity. Benchmarks have shown it to be substantially faster and more scalable than gproc in highly concurrent scenarios on multi-core systems, largely because its partitioned, decentralized design avoids the serialization bottlenecks present in gproc.38 Furthermore, its API is simpler and more idiomatic to Elixir developers, and being part of the standard library, it introduces no external dependencies.The recommended routing mechanism is as follows:A consumer process (e.g., a long-lived GenServer acting as a specific type of BitActor) will register its PID with Registry under a unique key, such as {:signal_consumer, :user_created}.A producer (e.g., a Reactor.Step completing a user creation workflow) can then use Registry.lookup/2 to find the PID(s) associated with that key and send a message (the "signal").This provides a clean, decoupled, and highly performant mechanism for routing signals within the BitActor mesh.FeatureElixir RegistryErlang gprocRecommendation for CNS ForgeScopeLocal (single-node) only.Supports both local and distributed (global) registration.Registry. Local scope is sufficient for the prototype's single-mesh design.PerformanceHighly scalable and significantly faster in concurrent local scenarios due to its partitioned architecture.Slower in concurrent local scenarios; can become a bottleneck.Registry. Performance is critical for a high-throughput system.APISimple, modern, and idiomatic Elixir API.More complex, feature-rich Erlang-style API.Registry. Simplicity leads to higher developer productivity and fewer errors.DependenciesBuilt into the Elixir standard library since v1.4.An external dependency that must be added to the project.Registry. Minimizing external dependencies simplifies maintenance.Feature SetFocused on key-value process registration and pub/sub.Richer feature set including properties, await-registration, and aggregated counters.Registry. The advanced features of gproc are not required by the PRD.5. A Reference Implementation WalkthroughTo provide a concrete illustration of the proposed architecture, this section traces a single transaction—the creation of a new user via an HTTP API call—through the various layers of the Reactor-based BitActor mesh. This walkthrough demonstrates how the system's components work in concert to fulfill the PRD's requirements and success criteria.5.1 Tracing a Single Transaction Through the Reactor-based MeshConsider a POST /users request with a JSON body containing a new user's details.Stimulus Input (Hop 1): The request is received by a Phoenix controller, which acts as the entry point. It creates the initial token: %{params: %{name: "Alice",...}, ttl: 8, transaction_id: "uuid-123-abc"}. It then calls the Ash entry point: MyApp.Users.create_user_workflow(%{token: initial_token}). This create_user_workflow is a generic Ash action whose run implementation is the CreateUserReactor module. This first step constitutes the stimulus:http_request BitActor.Decoder / Parser (Hop 2): The CreateUserReactor begins execution. Its first step is :decode_and_validate_params. This Reactor.Step takes the initial token, uses an Ash changeset to validate the params, and transforms them into canonical user_attributes. It returns a new token: {:ok, %{user_attributes: %{...}, ttl: 7, transaction_id: "..."}}.Workflow Engine (Hop 3): A decision step, :check_for_duplicates, is executed. This step uses the Ash.Reactor.read_one helper to perform a non-blocking read against the User resource, checking if a user with the given email already exists. read_one :check_for_duplicates, User, :get_by_email do... end. If a user is found, the workflow might be configured to halt or branch. Assuming no duplicate is found, it proceeds, returning {:ok, %{..., ttl: 6,...}}.Memory (Hop 4): The :create_user_in_db step is executed. This step is defined using the Ash.Reactor.create helper: create :create_user_in_db, User, :create do... end. This action is configured to use the Ash.DataLayer.Mnesia data layer. The operation is atomic. Upon success, the step's result is the newly created User record, which is added to the token. The TTL is decremented: {:ok, %{created_user: %User{...}, ttl: 5,...}}.Actuation Layer (Hop 5): The :send_welcome_email step runs. This is a generic Reactor.Step that uses an external library (e.g., Swoosh) to send an email to the address from the created_user record in the token. This is the action BitActor. It returns {:ok, %{..., email_sent_at: ~U[...], ttl: 4,...}}.Telemetry: Throughout this entire process, a Reactor.Middleware has been silently observing. After each of the five hops, it has emitted a :telemetry event ([:cns_forge, :bit_actor, :hop]). Each event payload contains the transaction_id, the step name (e.g., :create_user_in_db), the input token for that step, the output token, and the remaining TTL. These "pulse logs" are captured by a telemetry reporter and stored for analysis.5.2 Validating the 8 Hops Principle and Micro-Concurrency at ScaleThis walkthrough directly validates the prototype's success criteria.Verifiable Trace: The sequence of "pulse logs" captured by the telemetry system, all sharing the same transaction_id, allows an operator to reconstruct the exact, hop-by-hop journey of the transaction. This enables a full replay of the decision-making process, satisfying a key success criterion.The "8 Hops" Principle: The example workflow completed in 5 hops, well within the specified limit of 8. This demonstrates the feasibility of designing bounded, short-lived workflows that adhere to this architectural constraint.Massive Micro-Concurrency: The architecture's scalability can be illustrated by extending the example. If user creation also required creating an associated Profile record in a separate Mnesia table, a new Reactor.Step, :create_profile_in_db, could be added to the workflow. Because creating the user and creating the profile are independent operations that only rely on the initial validated attributes, Reactor's DAG execution model would run the :create_user_in_db and :create_profile_in_db steps concurrently. This demonstrates micro-concurrency. When thousands of such requests arrive simultaneously, the BEAM VM will schedule thousands of independent CreateUserReactor processes, each managing its own concurrent step execution. This ability to scale both horizontally (across many requests) and vertically (with concurrent steps within a single request) fulfills the criterion for "massive micro-concurrency" without contention.6. Strategic Evolution: From Prototype to Digital RealityThe selection of the Elixir Ash ecosystem is not merely a tactical choice for the prototype; it is a strategic decision that provides a clear and powerful trajectory toward the "Vision 2026" goal of orchestrating a "digital reality." The framework's architecture and its advanced extensions, particularly AshAI and AshEvents, offer a pre-built roadmap for evolving the system from simple workflow orchestration to intelligent, intent-driven composition and ultimate verifiability. This progression mirrors a move up the ladder of abstraction: the prototype starts with Ash.Resource (code-level logic), uses Ash.Reactor to compose it (workflow-level), and AshAI provides the final step to composition via intent (AI-level). This alignment between the project's long-term vision and the framework's capabilities is a powerful de-risking factor.6.1 Scaling the Vision: From Orchestrated Workflows to AI-Driven Agents with AshAIThe "Vision 2026" document speaks of translating "high-level, outcome-based directives" into a fabric of systems. This implies a future state far more dynamic and intelligent than the statically defined workflows of the prototype. The AshAI extension provides a concrete path to this future.7AshAI is a comprehensive toolbox for integrating Large Language Models (LLMs) into an Ash application. Its most powerful feature is the ability to expose Ash actions as "tools" that an AI agent can understand and invoke.42 Because Ash actions are declarative, typed, and self-describing, AshAI can automatically generate the necessary schemas and descriptions for an LLM to know when and how to use them.The strategic evolution is clear:Prototype Stage: The CNS Forge uses Ash.Reactor to execute pre-defined, static workflows composed of Ash actions (e.g., a PlaceOrderReactor).Evolutionary Stage: The high-level directives are no longer static workflows but natural language goals provided to an AI agent. For example, instead of triggering a specific reactor, a system might issue the directive: "Create a premium subscription for user 'alice@example.com' and notify her."AI-Driven Orchestration: The AshAI-powered agent receives this goal. It consults its available tools, which are the very same Ash actions used in the prototype (create_user, create_subscription, send_email, etc.). The agent dynamically determines the correct sequence of actions to call, resolves their dependencies, and executes them to achieve the desired outcome.This transforms the CNS Forge from a workflow engine into a true "ecosystem composer" driven by intent. Crucially, because the agent is still executing the same underlying, policy-enforced Ash actions, all the security, validation, and atomicity guarantees of the original system remain intact.426.2 Enhancing Verifiability and Auditability with AshEventsThe PRD's emphasis on "provably correct" systems implies a need for an unimpeachable, replayable audit trail that goes beyond diagnostic logging. The AshEvents extension, which provides first-class support for event sourcing, delivers this ultimate form of verifiability.7Event sourcing is an architectural pattern where all changes to application state are stored as a sequence of immutable events. Instead of overwriting data in a database, the system appends a new event describing the change (e.g., UserCreated, UserEmailUpdated). AshEvents automates this process by wrapping resource actions. When an action is performed on a resource with AshEvents enabled, it automatically persists a corresponding event to a dedicated event log before the action is committed.43This provides several strategic advantages for the CNS Forge vision:Complete Audit Trail: The event log becomes a perfect, chronological, and immutable record of every single state change that has ever occurred in the system. This is the highest possible standard of auditability.State Reconstruction: The entire state of the "digital reality" can be rebuilt at any point in time by simply replaying the events from the log up to that point. This makes the system's state completely verifiable and reproducible.Temporal Queries: The event log enables powerful analysis of how the system evolved over time, which is impossible with traditional state-oriented databases.By adopting AshEvents in a future iteration, the CNS Forge can move beyond the "pulse logs" used for debugging and implement a true, business-level system of record. This provides the foundational data integrity required to build and maintain a trusted and "provably correct" digital reality.7. Conclusion and Strategic RecommendationsThe analysis conducted in this report confirms that the Elixir Ash Reactor ecosystem is an exceptionally strong fit for implementing the CNS Forge Prototype. The architectural philosophy of Ash, centered on declarative, introspectable resources, aligns seamlessly with the PRD's vision of an "ecosystem composer." The technical capabilities of Ash.Reactor provide a direct and robust implementation path for the conceptual "BitActor Mesh," delivering the required atomicity, concurrency, and verifiability through its dependency-driven saga orchestration model.The primary technical challenges, most notably the novel requirement for a hop-based Time-To-Live (TTL), have been addressed with elegant patterns that leverage the strengths of both the Ash framework and the underlying Elixir/OTP platform. The proposed solution for TTL management, which treats the TTL as explicit state within the workflow's data token, is not only effective but also reinforces the principles of functional purity and determinism that are central to the PRD's goals. Similarly, the universal observability requirements are met idiomatically through the use of Reactor.Middleware and Elixir's standard :telemetry library, ensuring comprehensive instrumentation without compromising the clarity of the core business logic.Therefore, the principal recommendation of this report is to proceed with the development of the CNS Forge Prototype using the proposed architecture. This involves:Adopting the Elixir Ash ecosystem as the foundational technology stack.Implementing the logical "BitActor" concept using Reactor.Step modules.Leveraging Ash.Reactor's DAG and saga capabilities to build the "BitActor Mesh."Utilizing the Ash.DataLayer.Mnesia for transactional, in-memory state management.Employing Elixir's built-in Registry for high-performance signal routing.Beyond meeting the immediate needs of the prototype, this technology choice offers a compelling and low-risk path toward the long-term "Vision 2026." The Ash ecosystem's advanced extensions, such as AshAI and AshEvents, provide pre-built, idiomatic solutions for the more ambitious goals of intelligent, intent-driven orchestration and complete, event-sourced verifiability. By selecting Ash, Technology Applications, Inc. is not just choosing a framework for a single project; it is investing in a strategic platform that is designed to grow in sophistication and capability, ensuring that the CNS Forge can evolve from a prototype into the cornerstone of a truly dynamic and intelligent digital reality.

How to Build Data Processing Pipelines
Problem
You need to process large datasets through multiple transformation steps with error handling, progress tracking, and efficient batch processing. Your data pipeline should handle millions of records while being resilient to failures.

Solution Overview
This guide shows you how to build robust ETL (Extract, Transform, Load) and batch processing workflows using Reactor's map steps, async processing, and collect patterns. You'll learn to process data efficiently at scale.

Prerequisites
Understanding of Reactor basics (inputs, steps, arguments)
Familiarity with Elixir's Enum and Stream modules
Basic knowledge of data processing concepts
Processing Large Datasets
Reactor uses Iterex internally for efficient, resumable iteration over large datasets. This provides several advantages over standard Elixir streams:

Key Benefits:

Resumable: Can pause and resume processing from any point
Memory efficient: Processes data in configurable chunks without loading everything into memory
Lazy evaluation: Only processes data as needed, not all at once
Resource management: Proper cleanup of external resources (files, database connections)
When you need these patterns:

Processing files larger than available memory (> 1GB)
Long-running ETL jobs that may need to pause/resume
Data pipelines requiring checkpoint/recovery capabilities
Streaming data from external APIs with rate limits
Complete ETL Pipeline Example
Here's a complete data processing pipeline that extracts user data, transforms it, and loads it into multiple destinations:

defmodule DataPipeline.UserETL do
  use Reactor

  input :source_file
  input :output_destinations

  # Step 1: Extract - Read and parse data
  step :extract_data, DataPipeline.Steps.ExtractCSV do
    argument :file_path, input(:source_file)
  end

  # Step 2: Validate data quality
  step :validate_data_quality, DataPipeline.Steps.DataQualityCheck do
    argument :raw_data, result(:extract_data)
  end

  # Step 3: Transform users in batches
  map :transform_users do
    source result(:extract_data, [:users])
    allow_async? true
    return :validate_user

    step :clean_user do
      argument :user, element(:transform_users)
      run fn %{user: user} ->
        clean_and_normalize_user(%{user: user, rules: %{}})
      end
    end

    step :enrich_user do
      argument :clean_user, result(:clean_user)
      run &enrich_with_external_data/1
    end

    step :validate_user do
      argument :user, result(:enrich_user)
      run &validate_business_rules/1
    end
  end

  # Step 4: Collect transformation results
  collect :process_results do
    argument :transformed_users, result(:transform_users)
    argument :source_stats, result(:extract_data, [:stats])
    
    transform fn %{transformed_users: users, source_stats: stats} ->
      successful_users = Enum.filter(users, &match?({:ok, _}, &1))
      failed_users = Enum.filter(users, &match?({:error, _}, &1))
      
      %{
        successful: Enum.map(successful_users, &elem(&1, 1)),
        failed: failed_users,
        source_count: stats.total_count,
        success_rate: length(successful_users) / length(users)
      }
    end
  end

  # Step 5: Load to multiple destinations in parallel
  step :load_to_database, DataPipeline.Steps.LoadToDatabase do
    argument :users, result(:process_results, [:successful])
    async? true
  end

  step :load_to_search_index, DataPipeline.Steps.LoadToElasticsearch do
    argument :users, result(:process_results, [:successful])
    async? true
  end

  step :generate_report, DataPipeline.Steps.GenerateProcessingReport do
    argument :results, result(:process_results)
    wait_for [:load_to_database, :load_to_search_index]
  end

  return :generate_report
end

Step Implementations
1. Data Extraction
defmodule DataPipeline.Steps.ExtractCSV do
  use Reactor.Step

  @impl true
  def run(%{file_path: path}, _context, _options) do
    case File.exists?(path) do
      true ->
        users = 
          path
          |> File.stream!()
          |> CSV.decode!(headers: true)
          |> Enum.to_list()
        
        stats = %{
          total_count: length(users),
          file_size: File.stat!(path).size,
          extracted_at: DateTime.utc_now()
        }
        
        {:ok, %{users: users, stats: stats}}
        
      false ->
        {:error, "Source file not found: #{path}"}
    end
  end
end

2. Data Quality Validation
defmodule DataPipeline.Steps.DataQualityCheck do
  use Reactor.Step

  @impl true
  def run(%{raw_data: %{users: users}}, _context, _options) do
    # Analyze data quality
    quality_issues = analyze_quality(users)
    
    rules = %{
      email_required: true,
      phone_format: ~r/^\+?[\d\s\-\(\)]+$/,
      name_min_length: 2,
      max_age: 120
    }
    
    case quality_issues do
      [] -> 
        {:ok, %{rules: rules, issues: [], status: :passed}}
      issues when length(issues) < 100 ->
        {:ok, %{rules: rules, issues: issues, status: :warnings}}
      issues ->
        {:error, "Too many quality issues: #{length(issues)} problems found"}
    end
  end

  defp analyze_quality(users) do
    users
    |> Enum.with_index()
    |> Enum.flat_map(fn {user, index} ->
      check_user_quality(user, index)
    end)
  end

  defp check_user_quality(user, index) do
    issues = []
    
    issues = if is_nil(user["email"]) or user["email"] == "" do
      ["Row #{index + 1}: Missing email" | issues]
    else
      issues
    end
    
    issues = if is_nil(user["name"]) or String.length(user["name"]) < 2 do
      ["Row #{index + 1}: Invalid name" | issues]
    else
      issues
    end
    
    issues
  end
end

3. User Transformation Functions
def clean_and_normalize_user(%{user: user, rules: _rules}) do
  cleaned = %{
    id: user["id"],
    email: String.downcase(String.trim(user["email"] || "")),
    name: String.trim(user["name"] || ""),
    phone: normalize_phone(user["phone"]),
    age: parse_age(user["age"]),
    created_at: DateTime.utc_now()
  }
  
  {:ok, cleaned}
rescue
  e -> {:error, "Failed to clean user #{user["id"]}: #{inspect(e)}"}
end

def enrich_with_external_data(%{clean_user: user}) do
  case ExternalAPI.get_user_profile(user.email) do
    {:ok, profile} ->
      enriched = Map.merge(user, %{
        company: profile.company,
        location: profile.location,
        verified: profile.verified
      })
      {:ok, enriched}
      
    {:error, :not_found} ->
      # Continue without enrichment
      {:ok, Map.put(user, :verified, false)}
      
    {:error, reason} ->
      {:error, "Enrichment failed for #{user.email}: #{reason}"}
  end
end

def validate_business_rules(%{user: user}) do
  errors = []
  
  errors = if String.length(user.name) < 2 do
    ["Name too short" | errors]
  else
    errors
  end
  
  errors = if not String.contains?(user.email, "@") do
    ["Invalid email format" | errors]
  else
    errors
  end
  
  errors = if user.age && user.age > 120 do
    ["Unrealistic age" | errors]
  else
    errors
  end
  
  case errors do
    [] -> {:ok, user}
    errors -> {:error, "Validation failed: #{Enum.join(errors, ", ")}"}
  end
end

4. Data Loading Steps
defmodule DataPipeline.Steps.LoadToDatabase do
  use Reactor.Step

  @impl true
  def run(%{users: users}, _context, _options) do
    batches = Enum.chunk_every(users, 1000)
    
    results = Enum.map(batches, fn batch ->
      case MyApp.Repo.insert_all("users", batch, 
           on_conflict: :replace_all,
           conflict_target: [:email]) do
        {count, _} -> {:ok, count}
        error -> {:error, error}
      end
    end)
    
    total_inserted = results
    |> Enum.filter(&match?({:ok, _}, &1))
    |> Enum.map(&elem(&1, 1))
    |> Enum.sum()
    
    {:ok, %{inserted: total_inserted, total_batches: length(batches)}}
  end

  @impl true
  def compensate(_reason, %{users: users}, _context, _options) do
    # Cleanup on failure - remove any partially inserted data
    user_emails = Enum.map(users, & &1.email)
    MyApp.Repo.delete_all(from u in "users", where: u.email in ^user_emails)
    :ok
  end
end

defmodule DataPipeline.Steps.LoadToElasticsearch do
  use Reactor.Step

  @impl true
  def run(%{users: users}, _context, _options) do
    bulk_requests = Enum.map(users, fn user ->
      %{
        index: %{
          _index: "users",
          _id: user.id,
          _source: user
        }
      }
    end)
    
    case Elasticsearch.bulk_request(bulk_requests) do
      {:ok, response} ->
        indexed = response["items"] |> length()
        {:ok, %{indexed: indexed}}
        
      {:error, reason} ->
        {:error, "Elasticsearch indexing failed: #{reason}"}
    end
  end
end

Running the Pipeline
# Process a large CSV file
{:ok, report} = Reactor.run(DataPipeline.UserETL, %{
  source_file: "/data/users_export.csv",
  output_destinations: [:database, :elasticsearch]
})

IO.puts("Processing completed!")
IO.puts("Success rate: #{report.success_rate * 100}%")
IO.puts("Total processed: #{report.source_count}")

Advanced Patterns
Streaming Large Files
For very large files, process data in chunks to avoid memory issues and enable efficient error handling:

step :extract_streaming_data do
  run fn %{file_path: path} ->
    iter = 
      path
      |> File.stream!()
      |> CSV.decode!(headers: true)
      |> Iter.from()
    
    {:ok, iter}
  end
end

map :process_streaming_data do
  source result(:extract_streaming_data)
  allow_async? true
  return :process_chunk

  compose :process_chunk, ChunkProcessor do
    argument :chunk_data, element(:process_streaming_data)
  end
end

defmodule ChunkProcessor do
  use Reactor

  input :chunk_data

  step :validate_chunk do
    argument :data, input(:chunk_data)
    run &validate_chunk_data/1
  end

  step :transform_records do
    argument :data, result(:validate_chunk)
    run fn %{data: records} ->
      processed = Enum.map(records, &transform_record/1)
      {:ok, processed}
    end
  end

  step :save_checkpoint do
    argument :original, input(:chunk_data)
    argument :processed, result(:transform_records)
    
    run fn %{original: orig, processed: proc} ->
      save_chunk_checkpoint(orig, proc)
      {:ok, proc}
    end
    
    compensate fn _reason, %{original: chunk_data}, _context, _options ->
      log_failed_chunk(chunk_data)
      :retry
    end
  end

  return :save_checkpoint
end

Resumable Data Processing
Pause and resume processing across reactor executions:

step :prepare_processing_iter do
  argument :source_iter, result(:extract_streaming_data)
  
  run fn %{source_iter: iter} ->
    if File.exists?("/tmp/processing_checkpoint.json") do
      # Resume from checkpoint
      checkpoint = load_checkpoint()
      remaining_iter = Iter.drop(iter, checkpoint.processed_count)
      {:ok, remaining_iter}
    else
      # Start fresh processing
      {:ok, iter}
    end
  end
end

Parallel Processing with Error Handling
map :process_files do
  source input(:file_list)
  allow_async? true

  step :process_file do
    argument :file_path, element(:process_files)
    max_retries 3
    
    run fn %{file_path: path} ->
      case process_single_file(path) do
        {:ok, result} -> {:ok, result}
        {:error, :temp_failure} -> :retry
        {:error, reason} -> {:error, reason}
      end
    end
    
    compensate fn reason, %{file_path: path}, _, _ ->
      case reason do
        %File.Error{} -> :retry
        _other -> :ok
      end
    end
  end
end

Data Aggregation
map :calculate_totals do
  source input(:sales_data)
  
  step :sum_by_region do
    argument :region_data, element(:calculate_totals)
    
    run fn %{region_data: data} ->
      total = Enum.sum(Enum.map(data, & &1.amount))
      {:ok, %{region: data.region, total: total}}
    end
  end
end

collect :overall_totals do
  argument :region_totals, result(:calculate_totals)
  
  transform fn %{region_totals: totals} ->
    grand_total = Enum.sum(Enum.map(totals, & &1.total))
    %{grand_total: grand_total, by_region: totals}
  end
end

Monitoring and Observability
Progress Tracking
debug :log_progress do
  argument :batch_results, result(:transform_users)
  argument :message, value("Batch processing completed")
end

# Or use a regular step for custom progress tracking
step :track_progress do
  argument :batch_results, result(:transform_users)
  
  run fn %{batch_results: results} ->
    completed = length(results)
    successful = Enum.count(results, &match?({:ok, _}, &1))
    
    IO.puts("Processed #{completed} records, #{successful} successful")
    {:ok, %{completed: completed, successful: successful}}
  end
end

Telemetry Integration
Reactor provides observability using the conventional telemetry package. Add the telemetry middleware to emit events for monitoring your data pipelines:

defmodule DataPipeline.UserETL do
  use Reactor
  
  middlewares do
    middleware Reactor.Middleware.Telemetry
  end
  
  # Steps...
end

# In your application
:telemetry.attach("data-pipeline-handler", 
  [:reactor, :step, :stop], 
  &DataPipeline.Telemetry.handle_event/4, 
  %{}
)

Troubleshooting
Common Issues
Data inconsistency:

Implement proper compensation for database operations
Use database transactions where appropriate
Add data validation at multiple stages
For memory and performance issues, see Performance Optimization.

Debugging Tips
Add debug steps to monitor data flow:

debug :inspect_batch do
  argument :batch, element(:transform_users)
  argument :label, value("Processing batch")
end

Related Guides
Performance Optimization - Scaling techniques
Testing Strategies - Testing data pipelines
Async Workflows Tutorial - Concurrency basics

How to Orchestrate HTTP APIs with Reactor
Problem
You need to integrate with multiple HTTP APIs in production workflows, handling authentication, rate limits, circuit breakers, API versioning, and service discovery patterns.

Solution Overview
This guide shows you how to build production-ready API orchestration using reactor_req, covering real-world concerns like authentication management, circuit breakers, rate limiting, and service resilience patterns.

Prerequisites
Understanding of Reactor basics (inputs, steps, arguments)
Familiarity with error handling and compensation patterns
Basic knowledge of HTTP APIs and the Req HTTP client
Setup
Add reactor_req to your dependencies:

# mix.exs
def deps do
  [
    {:reactor, "~> 0.15"},
    {:reactor_req, "~> 0.1"},
    {:req, "~> 0.5"}
  ]
end

HTTP Client Integration with Reactor
The reactor_req package provides direct integration between Reactor and the Req HTTP client. Create lib/api_client.ex:

defmodule ApiClient do
  use Reactor

  input :base_url
  input :user_id

  req_new :setup_client do
    base_url input(:base_url)
    headers value([{"user-agent", "MyApp/1.0"}])
    retry value(:transient)
    retry_delay value(fn attempt -> 200 * attempt end)
  end

  template :build_profile_url do
    argument :user_id, input(:user_id)
    template "/users/<%= @user_id %>"
  end

  template :build_preferences_url do
    argument :user_id, input(:user_id)
    template "/users/<%= @user_id %>/preferences"
  end

  req_get :fetch_profile do
    request result(:setup_client)
    url result(:build_profile_url)
    headers value(%{"accept" => "application/json"})
  end

  req_get :fetch_preferences do
    request result(:setup_client)
    url result(:build_preferences_url)
  end

  step :combine_data do
    argument :profile, result(:fetch_profile, [:body])
    argument :preferences, result(:fetch_preferences, [:body])

    run fn %{profile: profile, preferences: prefs}, _context ->
      {:ok, %{profile: profile, preferences: prefs}}
    end
  end

  return :combine_data
end

Authentication Management
Handle API authentication and token refresh patterns:

defmodule AuthenticatedApiClient do
  use Reactor

  input :client_id
  input :client_secret
  input :api_endpoint

  step :build_oauth_payload do
    argument :client_id, input(:client_id)
    argument :client_secret, input(:client_secret)
    
    run fn %{client_id: client_id, client_secret: client_secret}, _context ->
      payload = %{
        grant_type: "client_credentials",
        client_id: client_id,
        client_secret: client_secret
      }
      {:ok, payload}
    end
  end

  req_post :get_auth_token do
    url value("https://auth.example.com/oauth/token")
    json result(:build_oauth_payload)
  end

  step :extract_access_token do
    argument :token_response, result(:get_auth_token, [:body])
    
    run fn %{token_response: resp}, _context ->
      {:ok, resp["access_token"]}
    end
  end

  template :build_auth_header do
    argument :token, result(:extract_access_token)
    template "Bearer <%= @token %>"
  end

  req_new :prepare_authenticated_client do
    base_url input(:api_endpoint)
    headers [{"authorization", result(:build_auth_header)}]
  end

  req_get :fetch_protected_data do
    request result(:prepare_authenticated_client)
    url value("/protected/data")
  end

  return :fetch_protected_data
end

API Versioning
Handle different API versions by composing version-specific reactors:

defmodule UserApiV1 do
  use Reactor

  input :user_id

  req_new :client do
    base_url value("https://api.example.com/v1")
  end

  template :build_user_path do
    argument :user_id, input(:user_id)
    template "/users/<%= @user_id %>"
  end

  req_get :fetch_user do
    request result(:client)
    url result(:build_user_path)
  end

  step :normalize_response do
    argument :response, result(:fetch_user, [:body])
    
    run fn %{response: resp}, _context ->
      normalized = %{
        id: resp["user_id"],
        name: resp["full_name"],
        email: resp["email_address"]
      }
      {:ok, normalized}
    end
  end

  return :normalize_response
end

defmodule UserApiV2 do
  use Reactor

  input :user_id

  req_new :client do
    base_url value("https://api.example.com/v2")
  end

  template :build_user_path do
    argument :user_id, input(:user_id)
    template "/users/<%= @user_id %>"
  end

  req_get :fetch_user do
    request result(:client)
    url result(:build_user_path)
  end

  return :fetch_user
end

defmodule VersionedUserApi do
  use Reactor

  input :api_version, default: "v1"
  input :user_id

  switch :fetch_user_by_version do
    on input(:api_version)

    match "v1" do
      compose :get_user, UserApiV1 do
        argument :user_id, input(:user_id)
      end
      return :get_user
    end

    match "v2" do
      compose :get_user, UserApiV2 do
        argument :user_id, input(:user_id)
      end
      return :get_user
    end

    default do
      flunk :unsupported_version do
        argument :version, input(:api_version)
        message "Unsupported API version: <%= @version %>"
      end
    end
  end

  return :fetch_user_by_version
end

Testing API Integration
Test your API orchestration patterns:

iex -S mix

# Test basic HTTP client integration
{:ok, result} = Reactor.run(ApiClient, %{
  base_url: "https://jsonplaceholder.typicode.com", 
  user_id: "1"
})

# Test rate limiting
requests = [%{id: 1}, %{id: 2}, %{id: 3}]
{:ok, results} = Reactor.run(RateLimitedApi, %{requests: requests})

# Test versioning
{:ok, normalized} = Reactor.run(VersionedApi, %{
  api_version: "v2",
  user_id: "123"
})

Related Guides
Error Handling Tutorial - Compensation and undo patterns
Async Workflows Tutorial - Concurrent processing
Performance Optimization - Scaling and monitoring

Building Async Workflows
In this tutorial, you'll learn how to build efficient concurrent workflows that take advantage of Reactor's dependency resolution and async execution capabilities.

What you'll build
A data processing pipeline that:

Fetches user data from multiple sources concurrently
Processes different data types in parallel
Aggregates results efficiently
Handles mixed sync/async requirements
Optimizes performance with proper concurrency control
You'll learn
How Reactor's concurrency model works
When to use async vs sync execution
How to optimize workflow performance
Managing dependencies for maximum parallelisation
Controlling concurrency limits and resource usage
Prerequisites
Complete the Getting Started tutorial
Complete the Error Handling tutorial
Basic understanding of Elixir processes
Step 1: Set up the project
If you don't have a project from the previous tutorials:

mix igniter.new reactor_tutorial --install reactor
cd reactor_tutorial

Step 2: Understanding Reactor's concurrency model
Reactor runs steps asynchronously by default:

Independent steps run in parallel - Steps with no dependencies execute simultaneously
Dependencies control execution order - Steps wait for their dependencies to complete
Automatic task management - Reactor manages Elixir tasks and supervision
Configurable concurrency - Control how many steps run at once
Async vs Sync execution
# Async (default) - runs in a separate task
step :fetch_data do
  async? true  # This is the default
  run &fetch_from_api/1
end

# Sync - runs in the main process
step :critical_operation do
  async? false  # Forces synchronous execution
  run &update_database/1
end

Step 3: Create simple data operations
Let's create some data operations that will run concurrently. Create lib/data_sources.ex:

defmodule DataSources do
  def fetch_user_profile(user_id) do
    Process.sleep(200)
    
    {:ok, %{
      id: user_id,
      name: "User #{user_id}",
      email: "user#{user_id}@example.com"
    }}
  end

  def fetch_user_preferences(user_id) do
    Process.sleep(150)
    
    {:ok, %{
      user_id: user_id,
      theme: "light",
      language: "en"
    }}
  end

  def fetch_user_activity(user_id) do
    Process.sleep(300)
    
    {:ok, %{
      user_id: user_id,
      last_login: DateTime.utc_now(),
      login_count: 42
    }}
  end
end

Step 4: Build a concurrent data reactor
Now let's build a reactor that fetches data concurrently. Create lib/async_user_data_reactor.ex:

defmodule AsyncUserDataReactor do
  use Reactor

  input :user_id

  # These steps have no dependencies on each other, so they run in parallel
  step :fetch_profile do
    argument :user_id, input(:user_id)
    run fn %{user_id: user_id}, _context ->
      DataSources.fetch_user_profile(user_id)
    end
  end

  step :fetch_preferences do
    argument :user_id, input(:user_id)  
    run fn %{user_id: user_id}, _context ->
      DataSources.fetch_user_preferences(user_id)
    end
  end

  step :fetch_activity do
    argument :user_id, input(:user_id)
    run fn %{user_id: user_id}, _context ->
      DataSources.fetch_user_activity(user_id)
    end
  end

  # This step waits for all the fetch steps to complete
  step :aggregate_data do
    argument :profile, result(:fetch_profile)
    argument :preferences, result(:fetch_preferences)
    argument :activity, result(:fetch_activity)
    
    run fn args, _context ->
      user_data = %{
        profile: args.profile,
        preferences: args.preferences,
        activity: args.activity,
        summary: "User #{args.profile.id} has #{args.activity.login_count} logins"
      }
      {:ok, user_data}
    end
  end

  return :aggregate_data
end

Step 5: Test the concurrent execution
Let's test our reactor to see the difference between concurrent and sequential execution:

iex -S mix

# Test the concurrent execution
start_time = :erlang.monotonic_time(:millisecond)

{:ok, result} = Reactor.run(AsyncUserDataReactor, %{user_id: 123})

end_time = :erlang.monotonic_time(:millisecond)
duration = end_time - start_time

IO.puts("Completed in #{duration}ms")
IO.inspect(result.summary)

Step 6: Understanding the execution flow
The reactor completes in about 300ms instead of 650ms (200+150+300) because:

All three fetch steps run concurrently (they only depend on input)
:aggregate_data waits for all three to complete
Total time is limited by the slowest operation (300ms) not the sum
Compare with synchronous execution:

# Force all steps to run synchronously
{:ok, result} = Reactor.run(AsyncUserDataReactor, %{user_id: 123}, %{}, async?: false)

This takes the full 650ms because each step runs sequentially.

Step 7: Controlling async behavior
You can control which steps run synchronously when needed:

defmodule SyncVsAsyncReactor do
  use Reactor

  input :user_id

  # I/O operations - keep async (default)
  step :fetch_profile do
    argument :user_id, input(:user_id)
    # async? true  # This is the default
    run fn %{user_id: user_id}, _context ->
      DataSources.fetch_user_profile(user_id)
    end
  end

  # CPU-intensive work - tune reactor concurrency as needed
  step :process_data do
    argument :profile, result(:fetch_profile)
    # async? true is the default - adjust reactor max_concurrency instead
    
    run fn %{profile: profile}, _context ->
      Process.sleep(100)
      {:ok, Map.put(profile, :processed, true)}
    end
  end

  return :process_data
end

What you learned
You now understand Reactor's concurrency model:

Steps run async by default - Enables automatic parallelisation
Dependencies determine execution order - Independent steps run concurrently
Tune concurrency for your workload - Adjust limits based on system capacity
Performance optimisation - Balance concurrency with system resources
Concurrency control - Manage resource usage with limits
Performance guidelines:
I/O operations → Generally benefit from high concurrency
CPU-intensive work → Tune concurrency to match CPU cores and workload
Resource limits → Set concurrency limits based on system capacity
What's next
Now that you understand concurrency, you're ready for advanced workflow patterns:

Composition - Build complex workflows with sub-reactors
Recursive Execution - Advanced iterative patterns
Testing Strategies - Test concurrent workflows effectively
Common issues
Steps aren't running in parallel: Check for hidden dependencies in arguments - each argument creates a dependency

For comprehensive performance and concurrency troubleshooting, see Performance Optimization.

Happy building concurrent workflows! ⚡

How to Test Reactors and Steps
Problem
You need comprehensive testing strategies for your reactors, including unit tests for individual steps, integration tests for complete workflows, and proper testing of error handling and compensation logic.

Solution Overview
This guide shows you different approaches to testing reactors, from testing individual step modules to full workflow integration tests. We'll cover unit testing, integration testing, error scenario testing, and concurrent execution testing.

Prerequisites
Understanding of Reactor basics
Familiarity with ExUnit testing
Basic knowledge of mocking with Mimic (for advanced scenarios)
Testing Strategies
1. Unit Testing Individual Steps
The most granular level is testing individual step modules directly.

Basic Step Testing
defmodule MyApp.Steps.ValidateEmailTest do
  use ExUnit.Case, async: true
  
  alias MyApp.Steps.ValidateEmail
  
  test "validates correct email format" do
    arguments = %{email: "user@example.com"}
    context = %{}
    options = []
    
    assert {:ok, "user@example.com"} = ValidateEmail.run(arguments, context, options)
  end
  
  test "returns error for invalid email" do
    arguments = %{email: "invalid-email"}
    context = %{}
    options = []
    
    assert {:error, %ArgumentError{}} = ValidateEmail.run(arguments, context, options)
  end
end

Testing Anonymous Function Steps
Instead of testing anonymous functions inline, extract them to public functions that can be unit tested:

defmodule MyApp.UserReactor do
  use Reactor
  
  input :name
  
  step :greet do
    argument :name, input(:name)
    run &greet_user/2
  end
  
  def greet_user(%{name: name}, _context) do
    {:ok, "Hello, #{name}!"}
  end
end

defmodule MyApp.UserReactorTest do
  use ExUnit.Case, async: true
  
  test "greet_user formats greeting correctly" do
    assert {:ok, "Hello, Marty!"} = 
      MyApp.UserReactor.greet_user(%{name: "Marty"}, %{})
  end
  
  test "greet_user handles edge cases" do
    assert {:ok, "Hello, !"} = 
      MyApp.UserReactor.greet_user(%{name: ""}, %{})
  end
end

2. Integration Testing Complete Reactors
Test entire workflows by running complete reactors.

Module-Based Reactor Testing
Test your actual reactor modules directly. When testing reactors that interact with databases or other shared resources, you'll typically want to disable async execution to ensure proper isolation with test sandboxes:

defmodule MyApp.UserRegistrationReactor do
  use Reactor
  
  input :email
  input :password
  
  step :validate_email, MyApp.Steps.ValidateEmail do
    argument :email, input(:email)
  end
  
  step :hash_password, MyApp.Steps.HashPassword do
    argument :password, input(:password)
  end
  
  step :create_user, MyApp.Steps.CreateUser do
    argument :email, result(:validate_email)
    argument :password_hash, result(:hash_password)
  end
end

defmodule MyApp.UserRegistrationReactorTest do
  use ExUnit.Case, async: false
  
  alias MyApp.UserRegistrationReactor
  
  test "successful user registration flow" do
    inputs = %{
      email: "user@example.com",
      password: "secure_password"
    }
    
    assert {:ok, %{id: user_id}} = Reactor.run(UserRegistrationReactor, inputs, async?: false)
    assert is_binary(user_id)
  end
  
  test "handles invalid email gracefully" do
    inputs = %{
      email: "invalid-email",
      password: "secure_password"
    }
    
    assert {:error, _reason} = Reactor.run(UserRegistrationReactor, inputs, async?: false)
  end
end

3. Testing Error Handling and Compensation
Test how your reactors handle failures and compensation using Mimic to control step behavior.

Setting Up Mimic for Step Mocking
We recommend using Mimic as your mocking library for testing Reactor steps. Mimic allows you to stub function calls without modifying your production code.

First, set up your test helper to copy the step modules you want to mock:

# test/test_helper.exs
Mimic.copy(MyApp.Steps.ProcessPayment)
Mimic.copy(MyApp.Steps.ReserveInventory)
Mimic.copy(MyApp.Steps.SendConfirmation)
ExUnit.start()

Testing Error Scenarios
defmodule MyApp.PaymentReactor do
  use Reactor
  
  input :payment_data
  input :items
  
  step :process_payment, MyApp.Steps.ProcessPayment do
    argument :payment_data, input(:payment_data)
  end
  
  step :reserve_inventory, MyApp.Steps.ReserveInventory do
    argument :items, input(:items)
    argument :payment_id, result(:process_payment, [:id])
  end
  
  step :send_confirmation, MyApp.Steps.SendConfirmation do
    argument :payment_id, result(:process_payment, [:id])
  end
end

defmodule MyApp.PaymentReactorTest do
  use ExUnit.Case, async: false
  use Mimic
  
  alias MyApp.PaymentReactor
  
  test "successful payment flow" do
    MyApp.Steps.ProcessPayment
    |> stub(:run, fn _args, _context, _opts -> 
      {:ok, %{id: "payment_123", status: :completed}}
    end)
    
    MyApp.Steps.ReserveInventory
    |> stub(:run, fn _args, _context, _opts -> 
      {:ok, %{reservation_id: "res_456"}}
    end)
    
    MyApp.Steps.SendConfirmation
    |> stub(:run, fn _args, _context, _opts -> 
      {:ok, :sent}
    end)
    
    inputs = %{payment_data: %{amount: 100}, items: [%{id: 1}]}
    assert {:ok, :sent} = Reactor.run(PaymentReactor, inputs, async?: false)
  end
  
  test "payment failure with no compensation needed" do
    MyApp.Steps.ProcessPayment
    |> stub(:run, fn _args, _context, _opts -> 
      {:error, :insufficient_funds}
    end)
    
    inputs = %{payment_data: %{amount: 100}, items: [%{id: 1}]}
    assert {:error, _reason} = Reactor.run(PaymentReactor, inputs, async?: false)
  end
  
  test "inventory failure triggers payment compensation" do
    MyApp.Steps.ProcessPayment
    |> stub(:run, fn _args, _context, _opts -> 
      {:ok, %{id: "payment_123", status: :completed}}
    end)
    |> expect(:compensate, fn _reason, _args, _context, _opts ->
      :ok
    end)
    
    MyApp.Steps.ReserveInventory
    |> stub(:run, fn _args, _context, _opts -> 
      {:error, :out_of_stock}
    end)
    
    inputs = %{payment_data: %{amount: 100}, items: [%{id: 1}]}
    assert {:error, _reason} = Reactor.run(PaymentReactor, inputs, async?: false)
  end
  
  test "compensation returns continue value" do
    MyApp.Steps.ProcessPayment
    |> stub(:run, fn _args, _context, _opts -> 
      {:error, :temporary_failure}
    end)
    |> stub(:compensate, fn _reason, _args, _context, _opts ->
      {:continue, %{id: "fallback_payment", status: :manual_review}}
    end)
    
    MyApp.Steps.ReserveInventory
    |> stub(:run, fn _args, _context, _opts -> 
      {:ok, %{reservation_id: "res_456"}}
    end)
    
    MyApp.Steps.SendConfirmation
    |> stub(:run, fn _args, _context, _opts -> 
      {:ok, :sent}
    end)
    
    inputs = %{payment_data: %{amount: 100}, items: [%{id: 1}]}
    assert {:ok, :sent} = Reactor.run(PaymentReactor, inputs, async?: false)
  end
end

Testing Undo Logic
test "later step failure triggers undo of earlier steps" do
  MyApp.Steps.ProcessPayment
  |> stub(:run, fn _args, _context, _opts -> 
    {:ok, %{id: "payment_123"}}
  end)
  |> expect(:undo, fn _result, _args, _context, _opts ->
    :ok
  end)
  
  MyApp.Steps.ReserveInventory
  |> stub(:run, fn _args, _context, _opts -> 
    {:ok, %{reservation_id: "res_456"}}
  end)
  
  MyApp.Steps.SendConfirmation
  |> stub(:run, fn _args, _context, _opts -> 
    {:error, :email_service_down}
  end)
  
  inputs = %{payment_data: %{amount: 100}, items: [%{id: 1}]}
  assert {:error, _reason} = Reactor.run(PaymentReactor, inputs, async?: false)
end

Best Practices
Test Organization
Unit Tests: Create one test file per step module
Integration Tests: Group related workflow tests together
Use async: true: For pure unit tests that don't depend on external state
Use async: false: For integration tests that need deterministic execution
Related Guides
Debugging Workflows - Troubleshooting techniques
Error Handling Tutorial - Learn compensation patterns
Async Workflows Tutorial - Understanding concurrency
This comprehensive testing approach ensures your reactors are reliable, maintainable, and perform well under various conditions.