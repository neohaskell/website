# Integration learning-path investigation

The owner requested a fresh Wayfinder session to plan integration teaching across the remaining documentation. Using existing integrations belongs in the beginner path; implementing a reusable integration belongs much later, even when an AI agent writes it. The tracker owns the page decisions and queue. This record supplies source evidence for that planning conversation, not a completed lesson or an executed runtime check.

## Investigated source

The actual Git clone under `.agents-state/storage/project/dist-newstyle/src/neohaskell-d0904e0943b43ca70fc25fb4f297a48ea71553754f944bf5650eb18680061115` was verified at `414602b9510a92dfac647bc68d6afadfb594502c`, the framework used by the native Neo 0.18.0 examples. Its `AGENTS.md` was read before inspection. No framework files, native projects, stored histories or running services were changed.

The current integration guide and inbound-worker page are still plans. Their briefs emphasize building handlers, HTTP calls and worker implementations. The existing workflows-across-entities concept plan mentions event-to-command coordination, but the learning path has not yet developed the requested progression from using a supplied integration to authoring a reusable provider.

## Source-backed findings

| Subject | Finding | Primary source at the investigated revision |
| --- | --- | --- |
| Existing integration users | The facade explicitly separates configuring reusable integrations from implementing their effects. `outbound`, `batch` and `none` serve the application-facing side. | [Integration](https://github.com/neohaskell/NeoHaskell/blob/414602b9510a92dfac647bc68d6afadfb594502c/core/service/Integration.hs) |
| Event-to-command coordination | `Integration.Command.Emit` is a generic action that emits a typed command. The Event Model DSL also has `triggersCommand` declarations. This supports explaining one entity's accepted event leading to another entity's command and separate decision. | [Command integration](https://github.com/neohaskell/NeoHaskell/blob/414602b9510a92dfac647bc68d6afadfb594502c/core/service/Integration/Command.hs), [Event Model](https://github.com/neohaskell/NeoHaskell/blob/414602b9510a92dfac647bc68d6afadfb594502c/core/service/Service/EventModel.hs) |
| Inbound work | `TypedInbound` declares allowed command/entity outputs. Workers receive an emit callback and run as asynchronous tasks. The generic timer accepts an application-supplied `toCommand` function. | [Integration facade](https://github.com/neohaskell/NeoHaskell/blob/414602b9510a92dfac647bc68d6afadfb594502c/core/service/Integration.hs), [Timer](https://github.com/neohaskell/NeoHaskell/blob/414602b9510a92dfac647bc68d6afadfb594502c/core/service/Integration/Timer.hs), [Application integration runtime](https://github.com/neohaskell/NeoHaskell/blob/414602b9510a92dfac647bc68d6afadfb594502c/core/service/Service/Application/Integrations.hs) |
| Outbound state and resources | Stateless runners and lifecycle-managed runners are distinct. `Lifecycle.OutboundConfig state` provides initialize, processEvent and cleanup; workers are per entity and can be reaped and initialized again. | [Lifecycle](https://github.com/neohaskell/NeoHaskell/blob/414602b9510a92dfac647bc68d6afadfb594502c/core/service/Integration/Lifecycle.hs), [Runtime types](https://github.com/neohaskell/NeoHaskell/blob/414602b9510a92dfac647bc68d6afadfb594502c/core/service/Service/Integration/Types.hs) |
| Fake mode | `--integrations=fake` and `--integrations=hybrid --fake=Name` select fake outbound adapters. The default is real. `Integration request` has an associated Response type and separate runReal/runFake functions. Application startup binds registered adapters according to that selection. | [Selection](https://github.com/neohaskell/NeoHaskell/blob/414602b9510a92dfac647bc68d6afadfb594502c/core/service/Service/Integration/Selection.hs), [Adapter](https://github.com/neohaskell/NeoHaskell/blob/414602b9510a92dfac647bc68d6afadfb594502c/core/service/Service/Integration/Adapter.hs), [Application](https://github.com/neohaskell/NeoHaskell/blob/414602b9510a92dfac647bc68d6afadfb594502c/core/service/Service/Application.hs) |

## Evidence boundaries for the next session

The source tests cover fake dispatch, hybrid selection, explicit fake implementations and generated fixture responses. They were inspected, not executed in this session. The next session must verify the actual application launch syntax and a representative no-third-party-effects exercise before teaching it as a working walkthrough.

The fake switch is specifically wired to registered outbound adapters. Source inspection here does not establish that it automatically replaces every legacy `ToAction`, lifecycle effect or inbound worker. Do not turn that partial finding into a promise that an arbitrary application's every side effect is mocked. Investigate the complete path and state its supported boundary precisely in internal evidence.

The exact schema-derived mocking API, inbound state/resource lifecycle, and the intended meaning of awaiting inbound work still need investigation. Typed request/response and allowed-output contracts are present; an equivalent explicit stateful inbound API or universal schema-to-fake generator was not established by the targeted reads. Distinguish waiting for a source, worker lifetime, command submission and completion of later effects.

The owner's application architecture places external business effects in integrations and keeps decisions separate. Preserve that teaching direction while distinguishing it internally from framework-controlled storage, transports, UUID generation and protected-value operations. Do not make an unsupported compiler-enforcement claim about all possible IO.

Generic integration modules should not import shop-domain modules. The owner wants reusable input/output types with application-owned mapping functions from event to integration input and from integration output to a follow-up command. Resolve idiomatic module paths while preserving the separation illustrated by `src/myshop` and `src/integrations`.

Current ecosystem contributions belong in the NeoHaskell monorepo. Neo packages is owner-stated future direction, not a verified current package command or published distribution workflow. Keep that distinction when mapping contribution and discovery pages.
