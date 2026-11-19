# Product

<!-- impeccable:product-schema 1 -->

## Platform

web

## Stack

Astro with Starlight is the website framework. The repository contains a custom homepage and static documentation pages in nine languages. The GitHub Pages workflow publishes at `https://nickseagull.dev/neohaskell.org/` using the `/neohaskell.org/` base path. Local development uses `http://127.0.0.1:4323/` without that base path.

## Users

The primary user is a person using AI agents to create an application. They want to build quickly and avoid accidental technical errors. They should not need senior-level knowledge of software architecture or infrastructure to succeed with NeoHaskell.

Prior Haskell expertise is not an established audience requirement. Do not assume that visitors already understand event sourcing or Event Modeling.

## Product Purpose

neohaskell.org introduces NeoHaskell as an **AI-first language**. It should help people build applications with AI agents while keeping the intended behavior understandable, changes traceable, and technical checks available. The language's framework and tooling supply a common technical structure.

The homepage is a benefits-led sales page for NeoHaskell as an AI-first language. The owner replaced the earlier code-first homepage on 11 November 2026. On 16 November, the owner requested the visual IDE, specification as code, and code freezing as homepage benefits. Subsequent ordering feedback places the IDE and freezing immediately after the hero, then history, pure logic and transports, followed by specification as code directly before the included capabilities. Subsequent feedback requires each addition to sell the developer's gains and a compact Event Model DSL example with both a command slice and a query slice. The specification section's prose addresses stale Markdown files and separate documents, without naming the DSL or IDE. The owner subsequently specified the target-product promise that an outdated specification prevents the app from running. State that directly in the homepage pitch; it is an owner-authorized final-product requirement, not a claim of newly executed runtime verification. The IDE copy should motivate developer agency, and the freezing heading should frame being set in stone as a benefit. The owner rejected the large ice/star datatype composition as out of place and reaffirmed the existing sections from "Your data has a memory" onward as the visual baseline. Show the freezing benefit through existing callers retaining their contract while changed requirements use a separate contract. The current replacement follows an independent Astra critique; the owner authorized implementation and presentation rather than a proposal-reading handoff. Detailed teaching remains in documentation. The homepage also covers event history and recovery, pure logic and type checks, convention over configuration, interchangeable HTTP/MCP transports, generated OpenAPI, OAuth provider connections, PostgreSQL and file storage. The owner explicitly requested final-state product content for a future publication, rather than a release-readiness report. Keep claims concrete without invented benchmarks or universal error-free guarantees.

Documentation begins with developer judgment, meaningful business questions, and enjoyment and control while building with AI or by hand. The Start here sequence then explains why business events preserve meaning beyond current values or snapshots, why changes pass through commands and decisions, how queries supply answers, and how Event Modeling connects those parts. Installation and the first project have their own navigation group. The owner requested this progression on 13 November 2026, superseding the previous immediate jump into a shop example. Substantive motivation is required; generic slogans remain excluded. The example is a small online shop and requires no installation or prior Haskell knowledge.

Documentation follows conventional guide pages inspired by Stripe Docs: topic navigation, a continuous article, inline code and comparison tables, and an on-page contents outline. The owner rejected the previous guided content and widget on 2026-11-09. That decision supersedes section snapping, viewport-height sections, vertical centering, scroll-linked examples, and persistent teaching companions. Keep the established NeoHaskell identity, 18px reading text, and at least 16px essential example text. Examples support the article and must not turn into a separate tool the reader has to operate. Native scrolling, deep links, search, code copying, and keyboard/mobile navigation support reading.

## Positioning

The owner's latest correction on 13 November 2026 establishes **language** as NeoHaskell's primary identity, as with Elm. Refer to the product as NeoHaskell or the language. It includes framework capabilities, but framework is not the product category or shorthand for its name. This supersedes the earlier language-and-framework pitch. The named One Person Framework reference remains a philosophy, not NeoHaskell's category. Preserve exact API names and technical references to actual framework components.

The owner confirmed on 13 November 2026 that the One Person Framework philosophy expresses NeoHaskell's purpose: one developer should be able to understand and build a whole application, using integrated conventions and AI assistance while retaining judgment, control, and enjoyment. This is an editorial conviction and design priority, not a staffing requirement or a claim that every application is a solo project. Developer value lies in understanding processes, asking difficult questions, and deciding what the software should do. The documentation should express that position firmly and explain the language's integrated choices that support it. Rails' [doctrine](https://rubyonrails.org/doctrine) and DHH's [The One Person Framework](https://world.hey.com/dhh/the-one-person-framework-711e6318) are philosophical references, not sources for NeoHaskell capability claims.


The owner confirmed on 2026-11-10: “I do not want to sell NeoHaskell as a dialect of Haskell, but rather to be an AI-first language.” This supersedes Haskell-first product framing. The reusable principles live in `$neohaskell-positioning`, referenced by `AGENTS.md`.

Lead with building applications with an AI agent and understanding the result. Explain the supporting mechanisms: explicit application behavior, pure decision logic, recorded history, bounded changes, and checks against intended behavior. Haskell remains accurate technical context where relevant, rather than the opening identity or implied prerequisite.

Ease of use, fast application development, and preventing accidental technical errors are the owner-confirmed product priorities. Explain those priorities through the language's integrated capabilities. Any absolute guarantee of error-free software or quantified productivity improvement requires separate evidence.

## Operating Context

Users work with AI agents to create applications. NeoHaskell is intended to let them concentrate on what an application should do while the language and its tools supply technical structure. Event Modeling is central to expressing that behavior; event sourcing supplies its recorded history.

Use **commands, decisions, events, and queries** to explain application behavior, following [GLOSSARY.md](GLOSSARY.md). The owner confirmed this vocabulary on 2026-11-06: command replaces request, decision replaces rule, and query replaces view. The selected Event ledger diagram keeps recorded events separate from query results.

The existing public site offers paths to the GitHub organization and Discord community. The local onboarding flow teaches the core concepts first, then introduces environment preparation, agent-skills installation, a copyable first task, and checking expected behavior. The skills command and supported agents were checked against the upstream skills repository on 2026-11-09; the guide links to its source. Framework environment instructions remain at the upstream project. Recheck version-dependent setup details before publication.

## Capabilities and Constraints

- Introduce NeoHaskell as an AI-first language, with integrated capabilities and a shared structure for humans and agents.
- Treat pure application logic and preselected technical decisions as central to the product's intended ease of use.
- The homepage may present the owner-authorized final product direction for future publication. Keep current source evidence and final-state intent distinct in this internal record; no release-readiness caveats are required in the sales copy.
- The current site distinguishes technical checks from determining whether application behavior matches the business's intent. Preserve that distinction when explaining error prevention.
- No website scaffold, deployment configuration, or local asset library existed when this product record was initialized. Inspect any subsequently supplied implementation before making design or migration decisions.

## Brand Commitments

Preserve the name **NeoHaskell**, the domain **neohaskell.org**, and the existing [NeoHaskell logo](https://neohaskell.org/img/logo.svg). Preserve the accepted workshop palette, typography and logo. The 11 November 2026 sales-page request supersedes the single opening diagram and prior homepage composition; use varied benefit illustrations within that identity. Write approachable, specific product copy that connects AI-assisted building with understandable behavior and inspectable evidence.

## Evidence on Hand

- Owner interview, 2026-11-06: confirms the primary AI-agent user, Haskell foundation, Event Modeling focus, pure application logic, and the aim of removing technical decisions and accidental errors.
- [Existing public website](https://neohaskell.org/), reviewed 2026-11-06: provides conceptual explanations and illustrative account-history examples. It is supporting evidence, not a replacement for the owner's confirmed audience and purpose.
- [NeoHaskell GitHub organization](https://github.com/neohaskell), reviewed 2026-11-06: provides technical heritage and repository links. Its dialect framing is not the marketing authority after the owner's 2026-11-10 correction. The specific artifacts below were subsequently inspected.
- No customer testimonials, production case studies, comparative benchmarks, or measured error-reduction claims were verified during init. Obtain evidence before using them.

## Product Principles

1. Motivate visitors with benefits on the homepage; teach application behavior and building with agents in documentation.
2. Make useful application development accessible without requiring senior architectural or infrastructure expertise.
3. Keep Event Modeling and pure domain behavior central to explaining the product.
4. Reduce avoidable technical decisions and opportunities for accidental mistakes.
5. Make claims concrete and distinguish intended benefits from demonstrated capabilities.

## Homepage evidence and positioning update : 10 November 2026

The owner authorized implementing the Ruby comparison's spacing, content, and readability recommendations. The homepage keeps the existing concepts-first journey and one ecommerce animation, with clearer prerequisite text and a compact development/community section. The current upstream README explicitly states that NeoHaskell is under active development; the public GitHub repository is not archived. The existing public site's Discord invite `wDj3UYzec8` was checked through Discord's invite endpoint and resolves to the NeoHaskell community. These support the homepage's status and contact routes, not production-readiness claims.

The owner accepted the visual refinement (“much better now”) and requested the missing evidence and AI-first language positioning. Copy now connects AI-assisted building to the public site's emphasis on understandable behavior, recorded history, and reviewing changes. The existing illustration remains conceptual, not a product execution recording.

Primary sources checked on 2026-11-10:

- [Agent skills v1.1.0](https://github.com/neohaskell/skills/releases/tag/v1.1.0), published 2026-11-06; the [feature workflow at revision 0ff0872](https://github.com/neohaskell/skills/blob/0ff0872d554e79d955f4c92f9b905056b908e9ae/skills/neohaskell-feature-pipeline/SKILL.md) guides backend, API/client, frontend, and verification work. The repository README names Claude Code and Codex. These establish published instructions, not measured delivery performance.
- [Counter application starter at revision cacc806](https://github.com/neohaskell/NeoHaskell/tree/cacc8069070721539ae0aac53286ce9492e906a5/neo/starter) contains commands, events, a query, and an HTTP workflow. Its README provides execution instructions. We inspected the source; we did not execute the NeoHaskell application during this homepage change.
- [Increment decision tests](https://github.com/neohaskell/NeoHaskell/blob/cacc8069070721539ae0aac53286ce9492e906a5/neo/starter/tests/Decider/Counter/IncrementCounterSpec.hs) specify a missing counter, a non-positive amount (with a zero input), and an accepted positive amount. The [replay property](https://github.com/neohaskell/NeoHaskell/blob/cacc8069070721539ae0aac53286ce9492e906a5/neo/starter/tests/Property/CounterReplaySpec.hs) checks that the value equals the total of replayed increments. These are inspectable test definitions; their upstream suite was not run here.
- [Neo CLI and framework 0.17.1](https://github.com/neohaskell/NeoHaskell/releases/tag/neo-v0.17.1), published 2026-11-06, confirms a dated release and links its change. GitHub's release API reports it as published, non-draft, and not a prerelease. It is distinct from the agent-skills version and is not a production-readiness assertion.

No testimonials, adoption counts, or productivity benchmarks have been verified. Keep proof specific to the capability and artifact it supports.

## Homepage source evidence : 11 November 2026

The homepage initially displayed the unchanged Counter decision body from the inspected `cacc806` revision. The owner subsequently supplied a simplified body: ignore entity and context, reject non-positive amounts, and use `cmd.counterId`. Bind the accepted event with a `let` statement inside `else do` before passing it to `Decider.acceptExisting`. The example therefore demonstrates amount validation, without an existence check.

The owner's next request expands the example into two static sections: command, decision and event payload; then entity, update and query. Omit deriving clauses, type instances, the event sum and setup boilerplate. Main prose establishes the Counter starter's HTTP server and the existing-counter precondition, names the command and query routes, and links the source inline. The simplified query shows its projection functions without the instance wrapper. These adapted excerpts are not a complete standalone program or runtime execution. The shop diagram remains illustrative. Provenance, omitted wiring and the original Apache 2.0 license are recorded in `src/examples/README.md`.

## Resource discovery : 11 November 2026

A fresh Ruby comparison at checkpoint `68a9493` found an incomplete first-run journey and useful resources missing from local navigation. `/resources/` now links to the actual CLI prerequisites, project creation guidance, syntax, application topics, testing, storage, authorization, releases, license, and contribution guidance. These destinations were read at the same `cacc806` source revision. The general upstream README contains no setup instructions, so the setup guide now links to the specific CLI and starter instructions and distinguishes agent skills from the application toolchain.

The pinned starter's `src/App.hs` configures `SimpleEventStore` with `persistent = False`; its storage guide confirms events disappear at process exit and distinguishes event and query persistence. The resources page explains these actual defaults. The GitHub release API for `neo-v0.17.1` confirms the 6 November release adds owned file storage for generated documents. The new homepage summary describes that scoped change without claiming a production guarantee.

The homepage retains the source section fragment ID, a direct agent-setup link, and resource discovery. The owner’s later content-restraint correction removed the auxiliary hero link to the source section. The docs introduction and metadata use AI-first positioning. A verified empty-machine-to-running-app walkthrough, newcomer explanation of the decision syntax, and executed intent-to-agent-to-result demonstration remain gaps. No runtime installation, generated application, adoption claim, or support commitment was verified in this website pass.


## Benefits-led homepage direction and evidence: 11 November 2026

The owner rejected the homepage's flat lower sections and source-first persuasion, requested a complete sales landing, and supplied Ruby, Phoenix and Laravel screenshots as design references. Preserve the palette, typography and original logo. Present NeoHaskell as both an AI-first language and framework, lead with benefits, and motivate documentation as the place to learn concepts and syntax. The owner explicitly authorized final-state content for future publication, including capabilities still being completed. This supersedes the earlier homepage source-excerpt, single-diagram, teaching-first and readiness-report requirements. It does not change the documentation experience.

Checkpoint `801f592ba063bab31de01f5fdc75da5383632815` preserves the prior project state. Three agents researched graphical direction, responsive composition and technical content. The implementation leads with "Build with AI. Keep control.", then recorded history and recovery, pure business logic, HTTP/MCP reuse, six integrated framework capabilities, agent workflow and a documentation invitation. The history illustration appends a cancellation and restores the available stock; it does not delete events or promise to reverse external effects. The page has no source code or example controls.

Technical research checked clean local checkouts against remote main at framework revision `cacc8069070721539ae0aac53286ce9492e906a5` and agent-skills revision `0ff0872d554e79d955f4c92f9b905056b908e9ae`. The [research record](.impeccable/reviews/homepage-sales-2026-11-11/research.md) links the exact primary sources for pure decisions, outbound effects, replay, conventions, HTTP/MCP/CLI transports, OpenAPI, PostgreSQL, OIDC authorization, external OAuth, uploads, retained documents and agent workflows. Source inspection supports these mechanisms; no upstream runtime suite or productivity benchmark was executed. OAuth token storage is implemented in `Auth/OAuth2/Routes.hs`, and refresh/retry/storage in `Auth/OAuth2/TokenRefresh.hs`.

The one-line transport claim expresses the owner-authorized final product direction. Current transport registration is one line for a command set already permitting the selected transport; the source also has command-level `TransportsOf` declarations. The private evidence record retains that distinction without turning the sales page into a configuration guide. "99% pure" and an infallible AI were not used as unmeasured or universal claims; the copy instead names the enforced separation of pure decisions from effects and recoverable derived data.

Graphic, layout and content reviews were satisfied after one correction batch. Independent finish and example-review evidence is recorded beside the research. The owner subsequently accepted the completed homepage and its general pitch on 11 November 2026, asked to commit it, and requested a reusable skill for this direction. The approved implementation is `49d3206a6cd21d0057e1da34899d42308b4a0f55`. The installed `$neohaskell-positioning` skill now preserves the language-and-framework pitch and benefits-first homepage principles. This is acceptance of the product/design direction, not authorization to deploy; the site remains a local preview.

## Event Modeling entry direction: 13 November 2026

The owner accepted the more concrete documentation navigation and requested a short, motivating introduction before technical setup. Its primary value is helping developers understand and agree business behavior. The owner subsequently requested a motivating design-and-understanding title and a visual tour inspired by the current neohaskell.org homepage. Four diagrams explain one shop story, commands, decisions, events, queries, and bounded behavior slices. Linear growth is an aim supported by stable boundaries and explicit contracts, not a universal cost guarantee. The public article omits author mentions and the paper exercise; primary-source attribution stays in the research records. The next step is learning how NeoHaskell carries that model into an application.

Source revision `033ae4a570686cd885ca14c6595997d9498d5287` contains the visual IDE server, model canvas, feature navigation, and code-to-model synchronization. The introduction can explain diagram inspection without treating future diagram-to-code generation as shipped behavior. Exact source links and boundaries are in [the introduction research](docs/research/event-modeling-introduction-2026-11-13.md). This is source evidence, not an executed runtime or a guarantee that a model captures every business requirement.

The homepage's accepted copy and visual direction remain; its primary documentation links now enter the Event Modeling introduction. Translation now uses explicit page requests and native Starlight language folders; the owner selected this workflow on 13 November after stopping bulk translation. The owner subsequently renewed the request to translate and deploy, reusing the completed translations.

The subsequent 13 November documentation correction removes promotional lead sentences and teaches directly through a concrete shop scenario. Separate figures explain conceptual change effort and an Event Model. `GLOSSARY.md` now distinguishes the owner-facing query and integration names from source identifiers such as `StateView` and `OutboundTranslation`, and records the single-primary DSL slice boundary. The accepted homepage pitch remains scoped to the homepage.

## Progressive introduction: 13 November 2026

The owner accepted the recent diagrams and requested a fuller conceptual progression with adversarial reader review. Developer happiness is an explicit design aim: reduce recurring assembly work so attention can go to understanding, expressing, and checking business behavior. It applies whether code is written with an AI agent or by hand. The four-page sequence preserves the shop diagrams and conceptual growth comparison while moving setup into a separate group.

The [current source review](.impeccable/reviews/progressive-intro-2026-11-13/source-review.md) distinguishes framework conventions from generic architectural benefits. Snapshots are implemented and complement retained events. Command decisions exclude arbitrary external business actions, but the decision interpreter does perform controlled runtime work such as generating identifiers and handling protected values. Query state is maintained separately from retrieval. Retention, erasure, query catch-up, application policies, and durability remain real choices. The visual IDE reads the source model; diagram-to-code generation is not claimed.

## Event Model evolution direction: 13 November 2026

The owner explicitly authorized documenting the target deployment behavior on the Model the whole process page: deployed command, event, and query contracts and implementations freeze; a changed requirement or corrected decision gets a new identity in a copied feature; consumers can migrate independently; deprecated entry points can retire once unused. This avoids whole-API version branches for a local behavior change. It does not erase old event history or require faulty entry points to remain available.

This is approved target behavior for the current documentation authoring phase, not verified enforcement in revision 033ae4a. The owner confirmed that distinction after source inspection found deployed-slice identity/versioning still open in the design document. Keep that evidence boundary in review and release instructions. The existing DSL, typed rules, and source-derived IDE support are separately source-backed. See [the evidence record](.impeccable/reviews/model-evolution-2026-11-13/source-evidence.md).


## Deployed-code tutorial correction: 19 November 2026

The owner rejected tutorial examples that change existing deployed commands, events or queries. The reserve lesson must freeze the deployed baseline with the verified Neo CLI lock command, demonstrate normal build refusal for a modified locked file, and add a new command slice in a copied feature instead of changing PlaceOrder. Deliberate duplication preserves existing contracts and supports the accepted linear-complexity direction. Keep old behavior and tests, add separate new-behavior checks, and explain caller migration. This also governs later transport declarations and agent prompts. Neo 0.18.0 verifies dirty locked paths in neo build; neo test and neo run do not invoke that gate. The source-backed guard is narrower than automatic deployment immutability.


## Visual teaching direction: 19 November 2026

The owner requires purpose-built graphical explanations throughout substantive documentation, centered on each page's hardest idea. The reviewed introductory Events and history, Commands and queries, and Model the whole process pages establish the visual-teaching baseline. Make backend behavior visible through data flow, dependencies, transformations and observable outcomes. Screenshots and code are useful but do not replace explaining the mechanism graphically. Use animation when sequence clarifies causality, retain useful static and reduced-motion states, and avoid decorative filler. Figures belong immediately beside the concepts they explain, with premise figures near the opening. Use connected shapes and a clear visual hierarchy rather than grids of text cards. Danger color and a non-color break cue should identify actual failure while preserving semantic domain colors. The first application is the deployed-contract lesson: show existing caller expectations, new command identities, and how an incompatible event-reader change can prevent history replay. This direction applies to future page authoring and substantive revisions; it does not authorize an immediate rewrite of every completed page.
