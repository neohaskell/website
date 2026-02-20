# NeoHaskell Documentation Work Plan

> Revised after full review synthesis from Steve Klabnik, Julia Evans, Dan Abramov, Martin Fowler, Sarah Drasner, Tania Rascia, and Kent C. Dodds. All 33 consensus changes incorporated.
> Domain: **NeoBank** (banking/fintech) — consensus from expert panel.
> Language teaching strategy: **Implicit-first** — NeoHaskell syntax taught through the tutorial, not before it.

---

## Guiding Principles

1. **Learner before structure.** Understand who you're teaching and what transformation they need before deciding on sidebars.
2. **The mental model IS the documentation.** Every page exists to move the reader from "thinks in CRUD" to "thinks in events."
3. **Ship value early, iterate constantly.** No 5-phase waterfall. Build the first real page before perfecting the outline.
4. **If it doesn't compile, it doesn't ship.** Code verification infrastructure comes before content, not after.
5. **The example IS the argument.** NeoBank isn't a decoration — it's the lived proof that event sourcing is how the world already works.
6. **Multiple doors, one house.** Different audiences enter differently, but they all arrive at understanding.
7. **The tutorial teaches the language implicitly. Concept pages teach it explicitly.** NeoHaskell syntax is introduced through what the reader is building, never in isolation. New syntax is bolded on first use with a brief inline explanation; concept pages provide the full treatment. No one reads a grammar chapter before cooking dinner.
8. **Delight is a teaching strategy.** Surprise and "wait, really?!" moments aren't decoration — they're how insights stick. The emotional arc of the tutorial (curiosity → confidence → pride) is designed, not accidental.

---

## Audience Model

Three distinct entry points. Each reader arrives with different goals, different patience, and different prior knowledge.

### Developer ("I want to try this")
- Comes from: JavaScript/TypeScript, Python, Go, or Haskell
- Brings: CRUD mental model, ORM habits, REST/GraphQL assumptions
- Wants: working code in 15 minutes, then progressive depth
- Needs to unlearn: mutation-first thinking, "just update the record"
- Success state: can build and deploy an event-sourced app with confidence
- Note: Fintech developers (Stripe, Plaid, banking startups) are a high-value sub-audience. They already think in transactions and will progress fastest through the tutorial.

### Architect ("I want to evaluate this")
- Comes from: enterprise systems, microservices, existing event sourcing with Kafka/EventStoreDB
- Brings: DDD vocabulary, CQRS experience, justified skepticism
- Wants: technical depth, tradeoff analysis, transaction guarantees, comparison with existing event sourcing infrastructure (EventStoreDB, Kafka, Axon)
- Needs to learn: how NeoHaskell's compiler enforces what they currently enforce with discipline
- Success state: can make an informed adoption decision

### Decision-Maker ("I want to understand why this matters")
- Comes from: business context, team management, cost awareness
- Brings: frustration with engineering velocity decay
- Wants: the "why" without the "how", proof points, risk assessment
- Needs to learn: why event sourcing reduces long-term cost
- Success state: understands the value proposition well enough to greenlight a pilot

---

## The Mental Model Journey

This is the actual spine of the documentation. Every page must place the reader somewhere on this path and move them forward.

```
Stage 0: "I store data in rows and update them"                    (CRUD-thinker)
Stage 1: "Banks don't update balances — they record transactions"  (Event-curious)
Stage 2: "Events are immutable facts. State is derived"            (Event-thinker)
Stage 3: "The type system prevents me from cheating"               (NeoHaskell-aware)
Stage 4: "I think in commands, events, and projections"            (NeoHaskell-fluent)
Stage 5: "I can model any domain this way — not just banking"      (NeoHaskell-native)
```

Every page in the docs should have a metadata annotation (internal, not reader-facing):
- **Entry stage**: what the reader must already understand
- **Exit stage**: what the reader understands after this page
- **Key misconception addressed**: the specific wrong assumption this page corrects

---

## The 20 Misconceptions (Design Before Structure)

Before writing a single page, enumerate and validate these. They drive content decisions.

| # | Misconception | Truth | Where to address |
|---|--------------|-------|-----------------|
| 1 | "I need a database with tables and columns" | Events are stored in an append-only log; read models are derived | Getting Started, Concepts |
| 2 | "Event sourcing is a library I import" | It's the language's default — you'd have to work to avoid it | First tutorial page |
| 3 | "I can just update a field" | State changes are expressed as events; the compiler enforces this | Early tutorial |
| 4 | "This is just Haskell with different names" | NeoHaskell has different defaults, stdlib, and compiler behavior | "Coming from Haskell" |
| 5 | "I need to learn category theory" | NeoHaskell hides the math; you use it without naming it | Concepts intro |
| 6 | "Event sourcing means eventual consistency everywhere" | Depends on the bounded context; NeoHaskell gives you choices | Architecture guide |
| 7 | "My events are my API" | Commands (deposit slips) are the API; events (transactions) are internal facts | Tutorial part 2 — Account Rules |
| 8 | "I'll need a message broker (Kafka, RabbitMQ)" | Built-in event store handles this at language level | Getting Started |
| 9 | "Schema evolution will be a nightmare" | Type system + versioning strategy (address honestly) | Core Concepts page (not Advanced — this is too important to defer) |
| 10 | "Functional programming means no state" | There is state — it's just derived from events, not mutated | Concepts |
| 11 | "Haskell is slow" | NeoHaskell compiles to native code; concurrency is built-in | FAQ or Concepts |
| 12 | "I need to understand monads first" | NeoHaskell's stdlib is designed so you don't need to | Getting Started |
| 13 | "Event sourcing only works for banking/finance" | Banking is the natural fit, but it works for any domain — the logistics guide proves this | Second example domain (advanced guides), Coming From... pages |
| 14 | "I can't do simple CRUD-style operations" | Read models give you familiar query patterns — checking your balance IS a read model | Tutorial part 3 — Transaction History |
| 15 | "The compiler errors will be cryptic" | NeoHaskell explicitly focuses on friendly error messages | Getting Started |
| 16 | "I need Docker/Kubernetes to run this" | `nix develop` gives you everything | Installation |
| 17 | "This won't integrate with my existing system" | Integrations package, PostgreSQL event store | Guides |
| 18 | "I have to go all-in or not at all" | You can start with one bounded context | Architecture guide |
| 19 | "Testing event-sourced systems is hard" | It's actually easier — replay events, assert projections | Testing guide |
| 20 | "There's no community/support" | Discord, contributors, going full-time | Throughout |

> **Action**: Validate these with real developers before finalizing structure. Post in Discord, ask in the Haskell subreddit, talk to 5 people who looked at NeoHaskell and didn't adopt it.

---

## NeoBank: The Tutorial Spine

Design this FIRST. The tutorial structure emerges from the example's progression.

### The Domain

You're building NeoBank — a simple banking service. The domain is universally understood (everyone has a bank account), maps perfectly to event sourcing (ledgers are the original event store), and produces output that feels like real fintech with minimal complexity.

### The Character

The tutorial uses second person ("you") from Part 1 onward. Alex appears only on the tutorial index page as brief framing ("Meet Alex — a developer like you who wants to build a bank"). Inside the tutorial itself, product requirements are framed as "The requirement:" not "Alex wants to..." — this keeps the reader immersed as the builder, not an observer.

### The Scope

| Feature | Sounds like | Actually is |
|---------|------------|-------------|
| Multi-account management | Enterprise banking platform | A map of account IDs to event streams |
| Real-time transaction ledger | Bloomberg terminal | A list of events, rendered chronologically |
| Instant balance reconciliation | FinTech infrastructure | A fold over deposit/withdrawal events |
| Overdraft protection | Risk management system | A type-level constraint the compiler enforces |
| Inter-account transfers | Payment settlement network | Two commands across two aggregates |
| Complete audit trail | Regulatory compliance engine | The event store, which you already have |
| Historical replay | Time-travel debugging | Replaying events to a point in time |

> The magic: the left column sounds like it requires a team of 20. The right column is what the reader actually builds. NeoHaskell makes the right column produce the left column.

### The Progression

| Part | Title | Your goal | What the reader builds | Events introduced | The "wow" moment |
|------|-------|------------|----------------------|-------------------|-----------------|
| 1 | **Your First Transaction** | "I want to deposit money" | Event type, command handler, event store, balance as fold. Opens with brief CRUD example that fails ("what was the balance before the dispute?") — then introduces events as the solution. | `AccountOpened`, `MoneyDeposited` | "That's 12 lines of code?" |
| 2 | **Account Rules** | "Don't let me overdraw" | Validation, declined commands (return `Result.err`, not events), compiler catching illegal states. First Given-When-Then test introduced. | `MoneyWithdrawn` | "The compiler just prevented a real bug" |
| 3 | **Transaction History** | "Show me what happened" | TWO projections from the same event stream: formatted bank statement + monthly summary. This IS the CQRS insight — same events, different views. Projection test introduced. | (consuming existing events) | "Two completely different views from the same events?" |
| 4 | **Multiple Accounts** | "I need a savings account too" | Multiple aggregates (NOT bounded contexts — checking/savings are different aggregates in the same context), account-scoped events | `AccountOpened` (savings variant, same event with `accountType` field) | "Adding a new account type was trivial?" |
| 5 | **Transfers** | "Move money between accounts" | Cross-aggregate commands, saga/process manager with full state machine: happy path → failure → compensation. Cross-aggregate test. | `TransferRequested`, `TransferCompleted`, `TransferFailed` | "It handles the failure case too?" |
| 6 | **Audit Everything** | "Show me everything that ever happened" | Event replay, time-travel queries, full audit trail. Property-based test. | (replaying all events) | "I accidentally built an audit trail" |

### The Rosetta Stone (appears in Concepts, referenced from Tutorial Part 1)

| What you'd say | Banking term | NeoHaskell term | What it actually is |
|---------------|-------------|-----------------|-------------------|
| "Something happened" | Transaction | Event | An immutable record of a fact |
| "Do something" | Deposit slip | Command | A request that may be accepted or rejected |
| "What's my balance?" | Account statement | Projection | State derived by folding over events |
| "My account" | Account | Aggregate | A consistency boundary (not the entity itself — the boundary that protects it) |
| "Show me everything" | Audit log | Event Store | The append-only source of truth |
| "Retail vs. Lending" | Business division | Bounded Context | An independent model with its own events |

### Design Constraints

- Each part builds ONE concept on top of the previous
- Each part results in running code the reader wrote themselves
- Each part starts with a product requirement ("The requirement: ..."), not "Alex wants to..."
- Part 1 must produce visible output within 10 minutes of starting
- Part 6 deliberately echoes NeoHaskell.org's pitch: "Banks don't UPDATE your balance"
- Each tutorial page includes estimated reading/coding time (e.g., "~20 minutes")
- Each tutorial page opens with "What you'll learn" (3-5 bullet points) and closes with a "Recap" section
- Progress indicator shows "Part N of 6" on every tutorial page — completion visibility is motivation
- Tests are introduced from Part 2 onward; by Part 6 the reader has written 10-15 tests

### Production Scope Note (appears in Part 6 conclusion, NOT Part 1)

> Leading with "this isn't real" undermines the emotional strategy. Instead, the Part 6 conclusion reframes scope as forward-looking: "You've built accounts, transactions, transfers, and a complete audit trail. For production, you'd add: multi-currency support, regulatory compliance, concurrent access controls, and interest calculation. Those are covered in the advanced guides. What you've proven is that event sourcing makes complex-sounding features genuinely simple."
>
> If scope-setting is needed early, do it with confidence ("We'll keep this focused on the core patterns"), not defensiveness.

### The Opening Line of the Tutorial

> *"Every bank in the world runs on the same idea: don't change the number, record what
> happened. A deposit isn't 'set balance to $150' — it's 'recorded: $50 deposited.'
> NeoHaskell is built on this same idea. Let's prove it works by building a bank.*
>
> *By the end of this tutorial, your NeoBank will handle accounts, transactions, transfers,
> and a complete audit trail. It'll feel like enterprise software. It was 200 lines of code."*

### Second Example Domain (for advanced guides)

**Logistics / shipment tracking.** A package moves through locations, status changes are events, delivery is a projection. Different enough from banking to prove the pattern generalizes. Universal enough that everyone understands it.

### Installation Friction Mitigation

If Nix is the only installation path, this is the single highest-friction point in the entire documentation. Design the Nix experience as if it's the product.

- **Zero-install path if possible**: Offer a playground or GitHub Codespaces option so readers can start the tutorial without any local setup
- **If Nix is required**: The installation page must make Nix feel effortless — not "install Nix, then install NeoHaskell" but "run this one command"
- **Installation time estimates**: Tell the reader how long each step takes ("This downloads ~500MB and takes 2-5 minutes")
- **Platform-specific instructions**: Use Starlight Tabs for macOS / Linux / Windows (WSL) — no "figure out which one applies to you"
- **Troubleshooting section**: The 5 most common installation failures and their fixes, based on Discord reports

---

## Tutorial Layer System

Every tutorial page operates on three simultaneous layers. The reader engages with layer 1 (the narrative) and dips into layers 2 and 3 as curiosity demands.

### Layer 1: The NeoBank Narrative (mandatory, always visible)

This is the tutorial. You want to build a feature, you write the code, you see the result. NeoHaskell syntax is used but never formally introduced — the reader absorbs it by doing.

### Layer 2: Inline Syntax Explanation (replaces the dropped sidebar specification)

> The original plan specified a collapsible "New NeoHaskell on this page" sidebar. Reviewer consensus (5/7) found this overengineered and fighting the implicit-first philosophy. Julia Evans' annotated code blocks solve compound-unfamiliarity (Martin's concern) without shifting the reader's attention from "I'm building a bank" to "I'm learning a language."

The tutorial handles new syntax through three mechanisms:

1. **Bold on first use**: The first time a construct appears, it's **bolded** with a one-clause inline explanation. Example: "We use the **pipe operator (`|>`)** — it passes the left side as input to the right side, like Unix pipes for code."

2. **Annotated code blocks**: For the first major code example in Parts 1-3, use Julia Evans-style annotated code blocks where arrows or callouts point to unfamiliar syntax within the code itself. This teaches syntax IN the context of what the reader is building, not separate from it.

3. **Standalone syntax quick reference page**: `getting-started/cheat-sheet.mdx` (already planned) serves as the go-to reference for NeoHaskell syntax. Linked once from Part 1 ("bookmark this — it's your NeoHaskell phrasebook").

Rules:
- Maximum 3-5 new constructs per page (if more, the page is doing too much — split it)
- No separate syntax sidebar or collapsible aside — syntax lives in the narrative flow
- First occurrence of each construct is bold in the tutorial text with a brief explanation
- Subsequent occurrences are not explained — the reader has seen it before

### Layer 3: Concept Deep-Dive Links (inline, unobtrusive)

When the tutorial uses a concept that has a dedicated concept page, provide a non-disruptive link:

```markdown
The balance is a *projection* — state derived from events.
[→ Deep dive: What is a projection?](/concepts/projections)
```

Rules:
- Never interrupt the narrative flow — links go at the end of the paragraph, not mid-sentence
- Each concept is linked **once** (first meaningful use), not every time
- Links are optional — the tutorial MUST make sense without clicking any of them

### Layer Interaction Map

| Tutorial Part | Layer 2: Bold-on-first-use syntax | Layer 3: Concept Links |
|--------------|--------------------|-----------------------|
| 1. First Transaction | `do`, `\|>`, type declarations, `Task`, `Module.yield` (annotated code block) | Events, Commands, Event Store |
| 2. Account Rules | `case..of`, `Result`, pattern matching on types (annotated code block) | Aggregates, Validation, Testing |
| 3. Transaction History | Record syntax, `Array.map`, `Array.foldl` (annotated code block) | Projections, Read Models |
| 4. Multiple Accounts | Module structure, qualified imports, `Map` | Multiple Aggregates |
| 5. Transfers | `Task.andThen`, error propagation, `[fmt\|...\|]` | Sagas, Process Managers, Bounded Contexts |
| 6. Audit Everything | Event replay functions, time-based queries | Event Store internals |

> **Principle**: If a reader skips every link, they still complete the tutorial and have working code. The layers add depth, not dependencies.

---

## Code Verification Infrastructure

Build this BEFORE writing content. Non-negotiable.

### Requirements

1. Every code block in the docs must be extractable and compilable
2. CI runs on every PR that touches content files
3. Broken code blocks fail the build
4. Code blocks reference source files from the NeoHaskell repo with git hashes for drift detection

### Implementation

```
# In the website repo
scripts/
  extract-code-blocks.sh    # Extracts fenced code from .mdx files
  verify-code-blocks.sh     # Compiles each extracted block
  check-source-hashes.sh    # Verifies referenced source files haven't drifted

# In CI (GitHub Actions)
.github/workflows/
  verify-docs-code.yml      # Runs on PR, blocks merge if code is broken
```

### Approach Options (decide during implementation)

- **Option A**: Code blocks are inline in MDX, extracted and compiled by CI
- **Option B**: Code blocks live as separate `.hs` files, imported into MDX (Rust Book approach)
- **Option C**: Hybrid — tutorial code is standalone files, concept examples are inline

> Option B is recommended. It makes testing trivial and prevents code/prose from drifting apart.

---

## Site Structure (Diataxis-Informed)

Using the [Diataxis framework](https://diataxis.fr/) explicitly. Four documentation modes, three entry paths.

```
                    PRACTICAL
                       |
          Tutorials    |    How-to Guides
          (learning)   |    (goals)
                       |
  ACQUISITION ---------+--------- APPLICATION
                       |
          Explanation  |    Reference
          (understanding)   (information)
                       |
                   THEORETICAL
```

### Top-Level Sections

```yaml
sidebar:
  # Entry path: Developer
  - label: "Quick Start"
    purpose: "Zero to running code in 10 minutes"
    audience: Developer
    stage: 0 -> 1

  # Entry path: Developer (deep)
  - label: "Tutorial: Build NeoBank"
    purpose: "Build an event-sourced banking service, learn every concept"
    audience: Developer
    stage: 1 -> 5

  # Entry path: Everyone
  - label: "Core Concepts"
    purpose: "Mental models — why NeoHaskell works this way"
    audience: All (but especially Architect)
    stage: Varies (each page has its own entry/exit)

  # Entry path: Developer (practicing)
  - label: "Guides"
    purpose: "Solve a specific problem (standalone, non-linear)"
    audience: Developer building something
    stage: 3+

  # Entry path: Developer (practicing)
  - label: "Reference"
    purpose: "Look up API details (generated where possible)"
    audience: Developer building something
    stage: 3+

  # Entry path: Architect, Decision-Maker
  - label: "Coming From..."
    purpose: "Translation layers for specific backgrounds"
    audience: Anyone with existing mental models
    stage: 0 -> 2
```

### Boundary Definitions (Kent's Rule)

| Section | Linear? | Builds one project? | Standalone pages? | Reader goal |
|---------|---------|--------------------|--------------------|-------------|
| Quick Start | Yes | Minimal example | No | "Does this work on my machine?" |
| Tutorial | Yes | NeoBank | No | "Teach me everything in order" |
| Core Concepts | No | No | Yes | "Help me understand why" |
| Guides | No | No | Yes | "Help me do this specific thing" |
| Reference | No | No | Yes | "What are the exact details?" |
| Coming From... | No | No | Yes | "Map my existing knowledge" |

### Litmus Tests (for deciding where content belongs)

| Section | Litmus Test |
|---------|-------------|
| Tutorial | "Does this page require previous pages?" — if yes, it's tutorial. |
| Concept | "Could the reader understand this without a keyboard?" — if yes, it's explanation. |
| Guide | "Does this solve a problem the reader already knows they have?" — if yes, it's a guide. |
| Reference | "Would a reader come here to look up a forgotten detail?" — if yes, it's reference. |

---

## Reference Documentation Strategy

Reference docs should be **generated from source code** where possible, **hand-written** only for narrative explanation.

### Auto-Generated

- Module API docs (types, functions, signatures)
- Event store API
- Standard library function reference
- CLI tool reference (`neo` commands)

### Hand-Written

- "How to read this reference" guide
- Cross-cutting patterns (error handling conventions, naming conventions)
- Architecture decision records (ADRs — already in the sidebar)

### Source Linking

Every code example in the docs references the NeoHaskell source:

```markdown
<!-- source: neohaskell/neohaskell/core/src/Event/Store.hs@a1b2c3d -->
```

CI checks these hashes. When upstream changes, a bot opens a PR flagging stale examples.

---

## Just-in-Time Concept Page Strategy

Concept pages are NOT pre-written. They are written **when the tutorial first needs them**, ensuring every concept page has a concrete anchor in the reader's experience.

### The Rule

> No concept page exists until a tutorial page links to it. The tutorial drives concept page creation, not the other way around.

### Concept Page Schedule (driven by tutorial progression)

| Tutorial Part | Concept Pages to Write | Why now (the tutorial needs it) |
|--------------|----------------------|-------------------------------|
| 1. First Transaction | `events-not-state.mdx`, `commands-and-handlers.mdx`, `from-crud-to-events.mdx` | Reader just stored their first event, issued their first command, and saw CRUD fail |
| 2. Account Rules | `type-safety.mdx`, `testing-event-sourced-systems.mdx` | Reader saw the compiler reject an invalid withdrawal and wrote their first test |
| 3. Transaction History | `projections.mdx` | Reader just built two projections from the same events — the CQRS concept is fresh |
| 4. Multiple Accounts | `schema-evolution.mdx` | Reader has two account types — schema evolution is concrete now, not theoretical |
| 5. Transfers | `effects.mdx`, `bounded-contexts.mdx` | Cross-aggregate transfers genuinely need bounded context explanation; effects needed for saga |
| 6. Audit Everything | `concurrency.mdx`, `thinking-in-events.mdx`, `trade-offs.mdx` | Capstone: replay at scale needs concurrency; wrap-up needs "Thinking in Events" (the react.dev equivalent); honesty about trade-offs |

> **New pages from review consensus**: `from-crud-to-events.mdx` (elevated from Coming From subpage), `testing-event-sourced-systems.mdx`, `thinking-in-events.mdx` (Dan's "Thinking in React" equivalent — 5-step process: Identify events → Define commands → Design aggregates → Build projections → Connect bounded contexts, using non-banking domain), `trade-offs.mdx` (honest about when event sourcing adds unnecessary complexity), `schema-evolution.mdx` (moved from Advanced to Core — too important to defer).

### Concept Page Template (internal)

Every concept page follows this structure. **The template is flexible** — some concepts need 2 sections, some need 7. Required sections are marked; all others are optional based on what the concept demands.

```markdown
---
title: [Concept Name] (use concept-name headings for searchability)
description: [One sentence]
entry_stage: [Mental model stage required]
exit_stage: [Mental model stage after reading]
misconception: [Which misconception from the 20 this addresses]
tutorial_anchor: [Which tutorial part first links here]
---

## What You Might Expect
[Start from the reader's CRUD assumptions. "If you're used to SQL, you'd expect..."]

## Why That Breaks
[Show the specific failure. Make the reader feel the pain of the old model.]

## [Concept Name]: The Mental Model Shift (REQUIRED)
[The core explanation — what changes in how you think, not just what you do]

## What Happens When... (trace a concrete scenario)
[Make invisible runtime behavior visible. "When you deposit $50, here's what happens step by step..."]

## In NeoBank Terms (REQUIRED)
[Explain using the banking domain they already know from the tutorial]

## The Full Picture (REQUIRED, with code)
[Complete explanation with code examples]

## How NeoHaskell Enforces This
[What the compiler/linter does to keep you on the right path]

## Testing This Concept
[How to verify your understanding with a test — connects to testing-first philosophy]

## Going Deeper (optional)
[Academic references, advanced patterns, links to external resources]
```

> Required sections: **Mental Model Shift**, **In NeoBank Terms**, **The Full Picture**. All others are included when they serve the concept — flexibility over rigidity.

### Why Just-in-Time?

Pre-written concept pages suffer from three problems:
1. They use abstract examples because no tutorial context exists yet
2. They can't reference "remember when you built X?" — the reader hasn't
3. Writers guess what needs explaining instead of knowing from tutorial authoring

---

## "Coming From..." Section

Translation layers for specific backgrounds. These become the highest-traffic pages.

### Pages

| Page | Reader's background | Key translations |
|------|-------------------|-----------------|
| Coming from JavaScript/TypeScript | `let x = ...; x = newValue` | Immutability, type inference, algebraic data types |
| Coming from Python | Django ORM, Flask routes | Events vs. models, handlers vs. views |
| Coming from Go | Structs, interfaces, goroutines | Type classes, algebraic types, channels |
| Coming from Haskell | Monads, IO, cabal/stack | **Full Rosetta Stone page** — see expanded spec below |
| Coming from CRUD | REST endpoints, SQL updates | `UPDATE balance` → record `MoneyDeposited`; `SELECT balance` → fold over transactions; REST endpoint → command handler. **NOTE: This is elevated to a standalone Core Concepts page (`from-crud-to-events.mdx`), not just a Coming From subpage.** This is the core teaching document for the primary audience. Structure: What You Might Expect → Why That Breaks → The Mental Model Shift → Side-by-Side Code → When CRUD Is Fine. The Coming From section retains a short version that links to the full concept page. |
| Coming from Event Sourcing | Kafka, EventStoreDB, Axon | No schema registry needed (types ARE the schema), no consumer groups (projections are functions), event versioning via algebraic data types |

### Pattern for Each Page

```
## In [your language], you'd write...
[familiar code]

## In NeoHaskell, the equivalent is...
[NeoHaskell code]

## But here's what's actually different...
[the mental model shift — NOT just syntax translation]
```

### "Coming from Haskell" — Expanded Specification

This page is the most important "Coming From..." entry. Haskell developers will assume NeoHaskell is "just Haskell with different names" (Misconception #4). This page must disabuse them of that immediately and completely.

#### Structure

1. **Opening**: "NeoHaskell is a Haskell dialect, not a Haskell skin. It makes deliberate, opinionated choices that differ from GHC Haskell. These aren't bugs — they're the point."

2. **The Full Rosetta Stone** (every enforced divergence):

| Haskell | NeoHaskell | Why NeoHaskell chose differently | Enforced by |
|---------|-----------|--------------------------------|-------------|
| `IO a` | `Task err val` | Typed errors — every effectful computation declares its failure mode | Compiler |
| `Either a b` | `Result err val` with `Ok`/`Err` | Names describe intent, not position | Compiler |
| `pure` / `return` | `Module.yield` (e.g., `Task.yield`, `Result.ok`) | Says what it does, not what it satisfies | Linter (hlint) |
| `$` | `\|>` (pipe operator) | Left-to-right data flow, like reading English | Linter |
| `let..in` / `where` | `do` blocks for ALL bindings | One way to bind, always. Reduces cognitive load | Linter |
| Pattern matching in function defs | `case..of` only | Patterns are always visible at the use site | Linter |
| Point-free style `map f . filter g` | Explicit lambdas always | Readability over cleverness | Linter |
| `import Module (func)` | Qualified imports: `Array.map`, `Text.length` | Always know where a function comes from | Convention (enforced by linter) |
| `forall a b.` | `forall input output.` | Type variables describe their role | Convention |
| `Functor` / `Monad` / `Semigroup` | `Mappable` / `Thenable` / `Appendable` | Names describe behavior, not math heritage | Stdlib |
| Stack / Cabal | Nix + Hix | Reproducible builds by default | Tooling |
| `putStrLn` / string concatenation | `[fmt\|Hello {name}!\|]` | String interpolation via quasiquoters | Stdlib |

3. **"Why would you do this?"** — For each row, a one-paragraph rationale. Not defensive — explanatory. The tone is: "We know you know Haskell. Here's why we made a different choice."

4. **Common AI mistakes for Haskell developers** — When a Haskell dev uses AI (Copilot, ChatGPT, Claude), the AI will generate standard Haskell. This section lists the top 5 patterns the AI will get wrong and how to spot/fix them. Cross-references the "Using AI" guide.

5. **"What stayed the same"** — ADTs, type classes (called Traits), `deriving`, records, modules, GHC under the hood. Reassure the reader that their Haskell knowledge isn't wasted — it's redirected.

---

## Exercise Strategy

Every tutorial page and most concept pages include exercises. Passive reading doesn't build understanding.

### Exercise Types

1. **Modify**: "Change the event type to include X. What happens when you compile?"
2. **Predict**: "What will this code output? Think first, then run it."
3. **Extend**: "Add a `DailyWithdrawalLimitSet` event. Wire it through the handler so withdrawals are declined above the limit."
4. **Break**: "Remove the type annotation. What error do you get? Why?"
5. **Compare**: "Write this same feature in [language you know]. Which version is clearer?"
6. **Audit**: "Replay all events from scratch. Does the derived balance match the current balance? What would happen if an event was missing?"
7. **Verify**: "Write a test that proves this feature works." Creates a natural build→verify feedback loop. Event sourcing's Given-When-Then pattern IS the testing grammar — use it.

### Exercise Placement

- **Tutorial pages**: 1 Modify + 1 Extend per page (mandatory), plus 1 Verify from Part 2 onward
- **Concept pages**: 1 Predict or 1 Break per page (optional but encouraged)
- **Guides**: 0 exercises (guides are task-oriented, not learning-oriented)

### Exercise Solutions (non-negotiable)

Every exercise must have:
- 2-3 progressive hints in `<details>` components (graduated difficulty)
- Full solution behind a "Show solution" toggle
- Link to the tutorial companion repo branch for that section

> Exercises without solutions are frustrating. "Stuck? Here's the full code" sections dramatically improve tutorial completion rates.

### Exercise Difficulty Progression

Exercises follow a deliberate progression — the reader moves from copying to creating across the tutorial.

- **Part 1** (Copy & tweak): Copy-paste the code, run it, see the output, change one value. "Change the deposit amount to $200. Run it. What's the new balance?" The reader proves the system works.
- **Part 2** (Modify within structure): Change behavior within existing patterns. "Add a `MinimumBalance` rule — withdrawals below $10 remaining are declined." First `Verify` exercise: write a test for the rule.
- **Part 3** (Replicate a pattern): Build a similar feature from scratch. "Create a `MonthlyStatement` projection that groups transactions by month." The reader demonstrates they can apply the projection pattern independently.
- **Part 4** (Decompose & decide): Identify which pattern to apply and argue why. "Should `AccountType` be an event or a field on `AccountOpened`? Argue both sides." The reader is thinking like a designer, not a copier.
- **Part 5** (Critique & fix): Judge a given solution and find the flaw. "This transfer handler doesn't check the source balance. What goes wrong? Fix it." Write a cross-aggregate test.
- **Part 6** (Compose something new): Build something that combines multiple concepts. "Build a `FraudAlert` projection that flags accounts with 3+ declined withdrawals in 24 hours." Write a property-based test.

> By Part 6, the reader isn't following instructions — they're making design decisions. This is how we know they've internalized the mental model.

### Checkpoint Progression

Verification grows with the reader's confidence:
- **Part 1**: Manual verification — "run it, see the output"
- **Part 2**: Semi-automated — introduce the test runner, write first Given-When-Then test
- **Part 4**: Automated tests as primary verification — reader runs test suite
- **Part 6**: Reader writes their own verification from scratch — property-based testing

---

## Stuck Reader Strategy

Every tutorial page must design for failure, not just success. The #1 reason people don't finish tutorials isn't that the content is bad — they got stuck and had no way forward.

### Per-Tutorial-Page Requirements

1. **"If You're Stuck" section**: Escape hatches at the bottom of every tutorial page with 3-5 "If you see X, check Y" troubleshooting items specific to that page's content
2. **Complete code for each section**: Collapsible `<details>` with the full working code at each checkpoint — not just the final version
3. **Tutorial companion repo**: Tagged branches per part (`part-1-start`, `part-1-complete`, `part-2-start`, etc.). Reader can `git checkout part-2-start` to get a clean starting point for any section.
4. **Progressive hints**: Every exercise has 2-3 hints before the full solution (see Exercise Solutions above)

> The emotional safety net: at no point should a reader feel they have no options. Even "start this section over from the companion repo" is better than "I guess I'll quit."

---

## Visual Design Requirements

Diagrams are not optional. They're a core teaching mechanism, designed FIRST with prose written to support them.

### Required Diagrams (Phase 0)

1. **Event flow diagram**: Command → Event → Store → Projection → Read Model. The single most important diagram — shows how one action flows through the entire system. Clean SVG, not hand-drawn.
2. **Event timeline visualization**: Static SVG that grows through the tutorial. Part 1 shows 2 events; by Part 6 it shows the full history.
3. **CRUD vs. Events comparison visual**: Side-by-side showing "UPDATE balance SET 150" vs. "record MoneyDeposited $50." Used in tutorial intro and `from-crud-to-events.mdx`.

### Per-Concept-Page Visuals

Each concept page should include a "Visual Design" consideration: what would a diagram of this concept look like? Not every concept needs a diagram, but the decision should be explicit.

### Style

- Clean SVG (not hand-drawn)
- Consistent color language: commands = blue, events = green, projections = orange
- Accessible: must work in both light and dark mode
- Every diagram has alt text

---

## Starlight Component Strategy

Starlight provides components that are not decorations — they're the difference between documentation that works and documentation that merely exists. Adopt from Phase 0.

| Component | Purpose | Where to use |
|-----------|---------|-------------|
| **Tabs** | Platform-specific instructions, "Your Language → NeoHaskell" comparisons | Installation page (OS tabs), Coming From pages |
| **Asides** (note, tip, caution, danger) | Contextual callouts without breaking narrative flow | Tutorial: `tip` for shortcuts, `caution` for common mistakes, `note` for "you'll learn more about this in Part N" |
| **CardGrid** | Visual navigation for section index pages | Core Concepts index, Guides index |
| **Code block labels** | Name what the code IS, not just its language | Every code block: `title="src/Bank/Account.hs"` |
| **Steps** | Numbered sequential instructions | Installation, Getting Started |
| **Badges** | Status indicators | "New in Part 3", difficulty level on guides |

> Adopt these from the first page written. Retrofitting component usage is harder than starting with it.

---

## Tutorial Page Template

Every tutorial page follows this structure:

```markdown
---
title: "Part N: [Title]"
description: [One sentence]
time_estimate: "~20 minutes"
---

## What You'll Learn
- [3-5 bullet points — the reader knows what they're signing up for]

## The Requirement
[Product requirement framing — what you're building this part]

## [Main content sections...]

## If You're Stuck
- If you see [X], check [Y]
- If you see [Z], try [W]
- [Full code for this section in <details> toggle]
- [Link to companion repo branch]

## Recap
- [3-5 bullet points — what you just learned]
- [Link to next part]
```

---

## Missing Patterns (to be scheduled)

Patterns flagged by Martin Fowler as critical but not yet placed in the plan:

- **Idempotency**: Handling duplicate commands/events. Schedule as concept page or guide.
- **Snapshotting**: Performance optimization for long event streams. Schedule as guide.
- **GDPR / Tombstones**: Right to erasure in an append-only store. Schedule as guide.
- **Event ordering / Causality**: Ensuring events are processed in the correct order. Schedule as concept page with Part 5.

> These should be placed during Phase 2-3 as the tutorial content makes them concrete. Don't write them in the abstract.

---

## Cross-Reference Strategy

Documentation sections are not silos. Every page should connect the reader to related content.

### Cross-Reference Requirements

- Every tutorial page links to the concept pages it introduces (Layer 3)
- Every concept page links back to the tutorial part where the concept was first encountered
- Every "Coming From..." page links to the relevant concept pages for deeper understanding
- Every guide links to prerequisite concept pages
- The `from-crud-to-events.mdx` concept page is linked from: Tutorial Part 1, Getting Started, and the Coming From CRUD page

### Implementation

Use consistent link patterns:
- Tutorial → Concept: `[→ Deep dive: What is a projection?](/concepts/projections)`
- Concept → Tutorial: `[See this in action: Tutorial Part 3](/tutorial/03-transaction-history)`
- Coming From → Concept: `[Learn more: Events, not state](/concepts/events-not-state)`

---

## Compiler-as-Teacher Pattern

NeoHaskell's compiler is an active teaching tool. Every convention the language enforces becomes a learning opportunity — the reader deliberately triggers the guard rail, reads the error, and internalizes the rule.

### The Pattern

For each enforced NeoHaskell convention, the tutorial includes a **"Break It"** micro-exercise:

1. **Show the correct code** (reader has already written it)
2. **Instruct the reader to break it** — introduce the forbidden pattern intentionally
3. **Show the compiler error** — the exact message they'll see
4. **Explain what the error means** — translate compiler-speak to concept understanding
5. **Reader fixes it** — restoring the correct pattern with understanding, not just compliance

### Break-It Exercises by Convention

| Convention | What to break | Expected error (paraphrased) | What the reader learns |
|-----------|--------------|-----------------------------|-----------------------|
| `do` blocks only (no `let..in`, no `where`) | Replace a `do` block with `let..in` | "NeoHaskell uses do blocks for all bindings" | Why uniform binding syntax reduces cognitive load |
| Qualified imports (`Array.map`, not `map`) | Use an unqualified function name | "Ambiguous occurrence — did you mean Array.map or Map.map?" | Why qualification prevents name collisions |
| `Result Ok/Err` (not `Either Left/Right`) | Write `Left "error"` | "Not in scope: Left. Did you mean Result.err?" | NeoHaskell's vocabulary is intentional, not arbitrary |
| `\|>` pipe (not `$` or nested parens) | Use `$` for function application | "NeoHaskell uses \|> for function piping" | Data flows left-to-right, like reading English |
| No point-free style | Write `Array.map show` instead of `Array.map (\x -> show x)` | "Eta-reduce — NeoHaskell requires explicit lambdas" | Readability over cleverness |
| `Module.yield` (not `pure`/`return`) | Write `pure value` | "Not in scope: pure. Did you mean Task.yield?" | NeoHaskell names things for what they do, not what they are |

### Placement Rules

- One Break-It exercise per tutorial part (not per page — don't overdo it)
- Always AFTER the correct pattern has been used successfully at least once
- The broken code should be something AI tools commonly generate (see "Using AI" guide)
- Break-It exercises are distinct from the Exercise Strategy exercises — they target language conventions, not domain concepts

> **Why this works**: Readers who've seen the error once will recognize AI-generated wrong patterns immediately. The compiler becomes their code reviewer.

---

## Interactive Elements (Future, But Design For Them Now)

Even if not built immediately, structure content so these can be added later.

### Phase 1 (with Starlight)
- Copy buttons on all code blocks (Starlight default)
- Syntax highlighting (Starlight default)
- Search (Starlight default)
- Dark mode (Starlight default)

### Phase 2 (enhancements)
- "Run this code" buttons linking to an online REPL or playground
- Expandable "deep dive" sections for advanced details (progressive disclosure)
- Side-by-side "CRUD vs. Event Sourced" comparisons with toggle
- Interactive transaction timeline — click a deposit/withdrawal, see balance recalculate in real-time (mirrors how real banking apps display transactions)
- Balance reconciliation demo — show that replaying events from scratch produces the same balance (the "audit" concept, visualized)

### Phase 3 (ambitious)
- Embedded playground (like Svelte's tutorial)

> Structure the MDX so that code blocks tagged with `interactive` can be wired up later without rewriting content.

---

## Work Phases (Iterative, Not Waterfall)

### Phase 0 — Foundation (Week 1)

**Goal**: Buildable infrastructure and Tutorial Part 1 shipped. Streamlined per reviewer consensus — ship fast, learn from real readers.

Tasks:
- [ ] Set up code block extraction + compilation CI
- [ ] Finalize NeoBank domain model (account types, events, commands)
- [ ] Update `astro.config.mjs` sidebar to match the new section plan
- [ ] Design core visuals: event flow diagram (command → event → store → projection → read model), CRUD vs. Events comparison visual
- [ ] Set up tutorial companion repo with `part-1-start` branch
- [ ] Write `getting-started/installation.mdx` — Nix setup with installation friction mitigations (see below), first `neo` command, "Hello Transactions"
- [ ] Write Tutorial Part 1 (`01-first-transaction.mdx`) — this IS the priority deliverable
- [ ] Write `getting-started/cheat-sheet.mdx` — "I want to... / Write this..." quick reference. Two-column format. Printable. Linked from Part 1 as "your NeoHaskell phrasebook."
- [ ] Test Part 1 with someone who's never seen NeoHaskell (observe, don't help). Fix everything.

> Moved to Phase 1: "Validate 20 misconceptions" (happens during writing), "Study reference docs" (already done as part of review process), "Create ARCHITECTURE.md" (can happen alongside content). The priority is: **ship Tutorial Part 1, watch someone use it, fix everything.**

**Output**: CI pipeline, confirmed example domain, updated site config, Installation page, Tutorial Part 1, cheat sheet, core diagrams, companion repo.

### Phase 1 — Quick Start + First Concept Pages (Week 2)

**Goal**: Complete Quick Start flow and first concept pages driven by Tutorial Part 1.

Tasks:
- [ ] Write `getting-started/reading-neohaskell.mdx` — 5-minute annotated code walkthrough ("Reading NeoHaskell"). Julia Evans style — visual, friendly, zero prerequisites.
- [ ] Write `getting-started/first-events.mdx` — Define an event type, deposit money, see balance derived from events
- [ ] Write `guides/using-ai.mdx` — Copy-paste NeoHaskell prompt for AI tools, the 5 most common AI mistakes, how to spot wrong patterns
- [ ] Write first JIT concept pages: `events-not-state.mdx`, `commands-and-handlers.mdx`, `from-crud-to-events.mdx`
- [ ] Validate the 20 misconceptions list with 5+ real developers (moved from Phase 0)
- [ ] Create `ARCHITECTURE.md` documenting the Diataxis-based structure decision (moved from Phase 0)
- [ ] Every code block compiles in CI
- [ ] Verify the Layer System works — first-events.mdx must have all 3 layers (narrative, bold-on-first-use syntax, concept links)

**Output**: Complete Quick Start path, first concept pages, AI guide, validated misconceptions.

**Why this is Phase 1, not Phase 4**: You discover 80% of structural problems by writing the first real pages. Writing early is research.

### Phase 2 — Tutorial Spine (Weeks 3-5)

**Goal**: NeoBank tutorial, parts 1-4. The complete beginner journey.

Tasks:
- [ ] Write tutorial parts 1-4 following the NeoBank progression table
- [ ] Each part has: narrative, code examples, 2 exercises, checkpoint
- [ ] Each part's code builds on the previous (reader has a growing project)
- [ ] Test each part with a fresh reader after writing
- [ ] Write the "Core Concepts" pages that the tutorial pages reference (just-in-time)

**Output**: A developer can go from zero to "I built a working event-sourced bank with transaction history, overdraft protection, and transfers."

**Note on concept pages**: Write concept pages AS NEEDED by the tutorial, not ahead of time. If tutorial part 2 requires understanding "what is an aggregate," write the aggregate concept page then. The Account is the natural first aggregate — it's a concept every reader already intuitively understands. Don't pre-write concepts you haven't needed yet.

### Phase 3 — Breadth (Weeks 6-8)

**Goal**: Fill in the remaining sections so the docs feel complete.

Tasks:
- [ ] Write tutorial parts 5-6 (cross-account transfers, audit trail)
- [ ] Write "Coming From..." pages (JS/TS and Haskell first, others based on traffic)
- [ ] Write 3-5 standalone guides (testing, deployment, PostgreSQL event store, error handling, integrating with existing systems)
- [ ] Generate reference docs from source code
- [ ] Write "Common Errors" page (error message as heading, fix as content)
- [ ] Write "FAQ" addressing remaining misconceptions not covered elsewhere

**Output**: A documentation site that serves all three audience entry paths.

### Phase 4 — Polish and Feedback (Ongoing)

**Goal**: Continuous improvement based on real usage.

Tasks:
- [ ] Add analytics to identify high-bounce pages (people who land and leave)
- [ ] Add "Was this page helpful?" feedback mechanism
- [ ] Monitor Discord for recurring questions — each one is a documentation failure
- [ ] Write the second example domain (logistics/shipment tracking) for advanced guides
- [ ] Add interactive elements (Phase 2 of interactive strategy)

**Output**: Docs that measurably improve over time.

---

## Iteration Protocol

Documentation is never done. Build these feedback loops in from day one.

### After Every Page

1. Does the code compile? (CI verifies)
2. Can a fresh reader follow it without help? (test with 1 person)
3. Does it address at least one misconception from the list?
4. Is the entry/exit mental model stage clear?

### After Every Phase

1. What questions are people still asking in Discord?
2. Which pages have high bounce rates?
3. What did the test readers struggle with?
4. What structural assumptions turned out to be wrong? (update the plan)

### Monthly

1. Check source hash drift (are code examples still current?)
2. Review and update misconceptions list
3. Assess whether the "Coming From..." pages need new entries

---

## File Structure

```
src/content/docs/
  index.mdx                          # Landing page (existing)

  getting-started/
    index.mdx                        # Section overview
    installation.mdx                 # Nix, tooling, "hello world"
    reading-neohaskell.mdx           # 5-min annotated code walkthrough ("Reading NeoHaskell")
    cheat-sheet.mdx                  # "I want to... / Write this..." quick reference (printable)
    first-events.mdx                 # Define event, store it, read it

  tutorial/
    index.mdx                        # "Meet Alex — a developer like you." (Alex only here, "you" from Part 1)
    01-first-transaction.mdx         # CRUD failure moment, then events. Deposit money, derive balance.
    02-account-rules.mdx             # Overdraft protection, declined commands (Result.err), first test
    03-transaction-history.mdx       # TWO projections from same events (statement + monthly summary)
    04-multiple-accounts.mdx         # Multiple aggregates (NOT bounded contexts)
    05-transfers.mdx                 # Cross-aggregate transfers, saga with failure/compensation
    06-audit-everything.mdx          # Event replay, time-travel, audit trail, production scope note

  concepts/
    index.mdx                        # "How to read this section"
    events-not-state.mdx             # Core mental model shift
    commands-and-handlers.mdx        # Input processing
    from-crud-to-events.mdx          # ELEVATED: standalone page (was Coming From subpage) — the core teaching doc
    projections.mdx                  # Deriving state from events
    type-safety.mdx                  # Why the compiler is your friend
    testing-event-sourced-systems.mdx # Given-When-Then, the testing trophy for ES
    schema-evolution.mdx             # MOVED from Advanced to Core — too important to defer
    bounded-contexts.mdx             # System decomposition (linked from Part 5, not Part 4)
    effects.mdx                      # How NeoHaskell handles side effects
    thinking-in-events.mdx           # The "Thinking in React" equivalent — capstone mental model
    trade-offs.mdx                   # Honest: when NOT to use event sourcing
    concurrency.mdx                  # Channels, locks, async

  guides/
    index.mdx                        # "Pick what you need"
    using-ai.mdx                     # AI prompt for NeoHaskell, common AI mistakes, how to spot wrong patterns
    testing.mdx                      # Testing event-sourced systems
    deployment.mdx                   # From code to running service
    postgresql-event-store.mdx       # Production event storage
    error-handling.mdx               # Error patterns in NeoHaskell
    integrating-existing-systems.mdx # Incremental adoption
    common-errors.mdx                # Error message -> fix lookup table

  coming-from/
    index.mdx                        # "Find your background"
    javascript.mdx
    python.mdx
    go.mdx
    haskell.mdx
    crud.mdx                         # Short version — links to concepts/from-crud-to-events.mdx for full treatment
    event-sourcing.mdx

  reference/
    index.mdx                        # "How to read this reference"
    # (generated from source code, hand-written intro only)

  adrs/
    index.mdx                        # Architecture Decision Records
```

---

## What We're NOT Doing (And Why)

| Rejected approach | Why |
|-------------------|-----|
| Analyzing more documentation sites beyond the 12 already reviewed | We analyzed 12 sites during research. Diminishing returns from further analysis. Time is better spent on learner research and shipping content. |
| Separate `research/` and `structure/` directories | Intermediate artifacts that rot. Ship real content instead. |
| Writing all skeletons before any prose | Creates false sense of progress. Write real pages, discover structure. |
| One linear flow for all audiences | Decision-makers, architects, and developers have different needs. |
| Hand-writing all reference docs | Generate from source. Hand-write only what needs narrative. |
| Phase-gated approach (finish Phase N before starting N+1) | Iterate. Write → test → learn → restructure → write. |
| Separate "Learn NeoHaskell" section before the tutorial | Forces readers through a grammar chapter before they have context. Language is learned by using it, not by studying it. The tutorial teaches syntax implicitly; concept pages teach it explicitly. |
| Formal grammar reference page | NeoHaskell is a dialect, not a new language. A grammar spec signals "this is academic" instead of "this is practical." The cheat sheet and concept pages cover everything a developer needs. |
| Beginner / Intermediate / Advanced language tiers | Artificial divisions that create anxiety ("Am I intermediate yet?"). Instead, the tutorial progression naturally moves from simple to complex syntax. The exercise difficulty progression handles scaling without labeling readers. |

---

## Success Metrics

How we know the docs are working:

**Behavioral metrics:**
1. **Time to first deposit**: A new developer goes from zero to depositing $100 and seeing their balance derived from events — under 10 minutes.
2. **Tutorial completion rate**: >50% of people who start tutorial part 1 finish part 4. Part 4 is the "bank statement" moment — where projections click. This is the critical retention point.
3. **Discord question reduction**: Recurring questions decrease month-over-month.
4. **Contributor PRs**: At least 1 external docs PR per month (evidence that docs are contributor-friendly).
5. **Search satisfaction**: People who use site search find what they need (low "search → bounce" rate).
6. **"I built a bank" effect**: Tutorial readers describe NeoBank as "impressive" or "real" in feedback — evidence that the domain creates the right emotional response.

**Understanding metrics** (not just behavior):
7. **Mental model transfer**: After completing the tutorial, can the reader model a non-banking domain in events without guidance? (Test with 5 users post-tutorial.)
8. **Explanation ability**: Can the reader explain to someone else why events are stored instead of state? If they can teach it, they understand it.
9. **Pattern recognition**: When shown a new feature request, does the reader instinctively think "what events does this produce?" rather than "what table do I update?"

---

## Open Questions (To Resolve During Phase 0)

1. **Is Nix the only installation path?** If yes, the "Getting Started" page must make Nix feel effortless. If no, document alternatives.
2. **What's the CLI tool?** The tutorial assumes a `neo` CLI. What's the actual developer command?
3. **What's compilable today?** The tutorial can only teach what actually works. Scope NeoBank to current NeoHaskell capabilities.
4. **How mature is the PostgreSQL event store?** The website says "in progress." If it's not ready, the tutorial should use the in-memory store and the guide should say "coming soon" honestly.
5. **What does NeoHaskell's error output actually look like?** The claim is "friendly errors." The docs should showcase real error messages. Are they actually friendly yet?
6. **Decimal precision**: Does NeoHaskell have a Decimal/Money type, or will the tutorial use integers (cents)? Using floats for money in a banking tutorial would undermine credibility instantly.
7. **Concurrency in transfers**: Tutorial part 5 (transfers) implies two aggregates updating. Does NeoHaskell currently support sagas or process managers? If not, scope part 5 to what's available.
8. **The "compliance" claim**: Part 6 says "I accidentally built a compliance engine." Is this defensible? If NeoHaskell's event store doesn't have tamper-proof guarantees, soften this to "audit trail" rather than "compliance."
