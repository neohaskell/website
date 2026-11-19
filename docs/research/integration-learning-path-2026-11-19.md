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

## Accepted curriculum decisions

Nick accepted the early placement and then the three remaining curriculum recommendations in the live planning conversation on 19 November 2026. The planning ticket is the canonical decision record; this section preserves the teaching rationale and source boundaries, not a second execution queue.

Using supplied integrations follows Add a feature safely, before the transport, storage and access guides. Call an external service teaches application configuration and mappings rather than provider implementation. Run an inbound worker uses a supplied timer. Test integration behavior follows those two guides. First use already includes a controlled success check; the dedicated testing page develops provider failure, receiving-command rejection and observable completion.

The continuing example follows a mug order into a separate shipment entity. A supplied timer checks pending shipments. The example must introduce the necessary command and entity context locally and add new identities and slices without modifying deployed shop behavior. The application maps events to generic integration inputs and integration results to commands. Reusable integration modules live outside src/MyShop and do not import application-domain modules. The native IDE and focused figures explain actual event inputs, integration nodes and command targets.

Pure decisions and effects owns the application business-effect boundary and the distinction between Decision and Task. Workflows across entities deepens the same order-to-shipment example: two decisions, separate histories, asynchronous outcomes and failure boundaries. These concepts remain self-contained enough for direct entry. They precede two advanced guides, Create a reusable integration and Manage integration resources. The advanced guides explain the Task concepts they need locally and link to the later Tasks and effects reference rather than requiring a full reference detour.

The first advanced guide owns generic request/response contracts, app-owned mapping functions, schemas, explicit fakes and monorepo contribution. The second owns stateless/stateful execution, resource initialization, cleanup, cancellation and restart in both directions where verified. Integration contracts provides focused API lookup. Tasks and effects, JSON and schemas, runtime configuration and IDE reference pages retain their existing jobs while supplying the exact supporting contracts. Neo packages remains future direction.

## Additional source inspection during planning

All findings below concern framework revision 414602b9510a92dfac647bc68d6afadfb594502c and are source inspection, not newly executed runtime evidence.

* integrations/Integration/Http/Internal.hs defines ToAction (Request command). It calls executeRequest directly and maps onSuccess/onError to a command. No fake selection appears in this execution path. Searches of supplied integrations did not find registered-adapter runFake implementations. The presence of the global fake flag therefore does not establish isolation for supplied HTTP/email integrations.
* core/service/Service/Integration/Adapter.hs requires explicit runReal and runFake implementations and an associated Response. The class has no ToSchema constraint or default schema-derived generator. core/test/Service/Integration/TestFixtures.hs generates SendEmailResponse with a handwritten QuickCheck Arbitrary instance; ChargeIntent returns a scripted response. These fixtures are not evidence of automatic schema-derived fake generation. Schema-backed generation remains a specific research question.
* core/service/Service/Application.hs loads configuration and builds the integration registry separately. core/config/Config.hs passes raw process arguments to OptEnvConf.runParserOn. Exact launch argv and StarterConfig interaction require a representative native check; the inspected wire-up tests explicitly bypass argv parsing.
* core/service/Service/Application/Integrations.hs starts each inbound worker as an AsyncTask. Typed outputs are checked for registered receivers. Failure restarts the worker with backoff starting at 1000 ms and capped at 60000 ms; normal return ends it. An inbound worker's emit callback invokes dispatchCommandWithSource.
* core/service/Service/Integration/Dispatcher.hs waits for the receiving handler's acceptance callback; rejection, failure, missing callback or unavailable handler causes an error. The callback returns Unit, not a typed business result. This does not await downstream outbound effects or query convergence. The teaching must distinguish waiting for source input, submitting/accepting a command, observing a business result, and stopping/joining a worker.
* Integration.Lifecycle exports OutboundConfig with initialize, processEvent and cleanup. It describes per-entity workers and resource reinitialization after reaping. The inspected inbound config exposes a run function rather than equivalent lifecycle hooks. Application shutdown cancels inbound workers and uses AsyncTask.waitCatch. Safe resource acquisition/release inside a worker needs its own verified example; symmetric APIs must not be invented.

A focused native verification prerequisite must resolve the reader's complete safe testing path before the beginner integration walkthrough is authored. It must exercise real application argv, configuration, supplied provider wiring, deterministic failure/success and follow-up commands without third-party effects. It must report precisely what is substituted, including inbound and lifecycle exclusions. If the pinned framework cannot support the requested path, record the concrete framework gap for Nick rather than asking a beginner to implement a provider or claiming the flag covers it. No native fake-mode app was executed in this planning conversation.

## Tracker reconciliation

The [planning decision](https://plane.p.egregore.io/egregore/projects/5535680e-e3f6-4023-b400-2b27544130e2/issues/247875fa-677c-4a7d-9537-8ee9e12cee93/) reconciles nine existing page scopes and adds four focused page tickets plus [native isolation and completion verification](https://plane.p.egregore.io/egregore/projects/5535680e-e3f6-4023-b400-2b27544130e2/issues/90c94d7e-d23a-4e61-adc4-72a9cd5fdeff/). Full saved ticket bodies, parentage, open/unassigned state for new work and sort order were checked. Ten prerequisite additions and four obsolete predecessor removals were read back through the native relation API. The latter used the signed-in browser route because the public REST API exposes no removal operation. Saved receipts remain under ignored .agents-state/integration-map.

This delivery changes internal research and tracker planning only. Public page plans and sidebar navigation will be aligned as the pages are authored, under the reconciled map Notes. Persist and rebuild queries remains assigned and open for its separate source/scope decision. The native prerequisite remains open and unassigned; completing curriculum planning does not establish runtime success or start article authoring.

## Native verification outcome, 19 November 2026

The prerequisite was claimed and inspected at clean website checkpoint
`2b8ade5caad6c7dec69b539c2b6f45ec2ffc1ffa`. The framework clone remains clean at
`414602b9510a92dfac647bc68d6afadfb594502c`. The requested safe beginner exercise
is blocked by concrete launch and substitution gaps. This is a failed safety
gate, not completed runtime verification. Dependent articles must remain blocked.

### Executed launch checks

Ignored receipts are in `.agents-state/integration-map/native-verification/`.
`argv-results.json` records exact argv, working directories, exit codes and both
output streams. A copy of the retained storage-project executable ran only in a
new empty `isolated-launch` directory. No retained project, history or example
was edited. This executable was not rebuilt in this session; its SHA-256 and
that provenance limitation are recorded in `receipt.json`.

1. `neo-0.18.0 run --integrations=fake` exits 2 with an unexpected argument.
2. `neo-0.18.0 run -- --integrations=fake` also exits 2. The CLI supplies no
   application-argument forwarding route here.
3. `./app --integrations=fake` exits 1 during configuration loading with
   `Unrecognised args: --integrations=fake`.
4. `./app --integrations=hybrid --fake=Name` exits 1 during configuration loading
   with both arguments unrecognised.

Source agrees with these observations. `Config.loadWithVersion` feeds raw argv
to OptEnvConf, while StarterConfig declares port and upload settings rather than
integration switches. Application configuration loading precedes integration
context construction. The selection parser existing later in startup does not
make these flags usable through this StarterConfig path. A supported launch
contract needs framework-owned flag handling compatible with application
configuration and a documented CLI forwarding or direct application route.

### Substitution boundary and provider gap

`Service.Integration.Adapter.mkDispatcher` selects explicit `runReal` or
`runFake` for registered request adapters. Hybrid compares the reflected request
type name with the supplied names. This substitutes that dispatch operation,
not every Task, network call, worker or resource initializer in the process.

Supplied `Integration.Http.Internal.toAction` ignores its action context and calls
`executeRequest` directly. That function expands environment values, builds auth,
performs HTTP with retries and maps responses/errors to commands. It never
consults the selectable adapter registry. Enabling a fake flag therefore cannot
be used as a safety boundary for this supplied HTTP path. No safe supplied
provider mapping through that registry was established. Do not fill this gap by
making beginners implement an adapter.

The adapter class has an associated Response and requires both effect functions.
It imposes no schema or codec constraint and provides no default generator.
`Schema.ToSchema` derives structural descriptions through Generic; this is not
response generation or JSON decoding. JSON encoding/decoding contracts remain
separate from the associated type. The inspected SendEmail QuickCheck instance
is handwritten, and ChargeIntent is scripted. Neither proves schema-derived
fake generation. A schema-only automatic mock promise remains unsupported.

### Completion, timer and resources

Source inspection of `Integration.Timer.every` shows tick 1 is emitted
immediately, then the worker sleeps before each later tick. It is inaccurate to
promise an initial interval wait. Its mapping receives a tick count, not a
shipment query result. Application-owned work would still need to find pending
shipments and submit their commands.

The inbound emit path uses `dispatchCommandWithSource`, which checks the
receiving handler's acceptance callback. Its Unit result is not a shipment
business result and does not await a query update or downstream outbound work.
Worker failure can trigger restart/backoff; normal worker return ends it.
Application shutdown cancels workers and joins with `waitCatch`. This proves the
source's cancellation/join mechanism, not release of arbitrary resources.
OutboundConfig exposes initialize/processEvent/cleanup with per-entity worker
lifetimes. No symmetric inbound resource record or executed resource-release
proof was established. Cleanup still requires a concrete cancellation-safe
resource example before teaching guarantees.

### Explicitly incomplete acceptance

No new mug-order/provider/shipment runtime scenario was run. Provider success,
provider failure, receiving-command rejection, timer behavior, query convergence
and resource release have no new runtime proof. No compilation was substituted
for these checks. Native IDE source contains an IntegrationNode component with
inbound/outbound kind data, but actual rendered nodes and both-direction
connections were not verified. Those remain acceptance checks after the launch
and supplied-provider isolation contracts are repaired.

The prerequisite stays open, preserving all dependent blocks. Resume with a
source-matched native project after a supported supplied-provider fake path and
StarterConfig launch contract exist. Re-run the full ticket, including failure,
completion, cleanup and rendered IDE checks. Framework implementation and article
authoring were not started. Existing ignored evidence and the separate query
persistence claim remain untouched.
