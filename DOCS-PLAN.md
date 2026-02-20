# NeoHaskell Documentation Work Plan

> Revised after review by Steve Klabnik, Julia Evans, Dan Abramov, Martin Fowler, Sarah Drasner, Tania Rascia, and Kent C. Dodds.
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
7. **The tutorial teaches the language implicitly. Concept pages teach it explicitly.** NeoHaskell syntax is introduced through what the reader is building, never in isolation. Each tutorial page lists new syntax in a sidebar aside; concept pages provide the full explanation. No one reads a grammar chapter before cooking dinner.

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
| 9 | "Schema evolution will be a nightmare" | Type system + versioning strategy (address honestly) | Advanced guide |
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

Alex is a developer building NeoBank — a simple banking service. The domain is universally understood (everyone has a bank account), maps perfectly to event sourcing (ledgers are the original event store), and produces output that feels like real fintech with minimal complexity.

### The Character

Alex is the reader. Not a banker — a developer. Every part starts with what Alex (the developer) wants to build, framed as a product requirement that sounds impressively professional.

### The Scope

| Feature | Sounds like | Actually is |
|---------|------------|-------------|
| Multi-account management | Enterprise banking platform | A map of account IDs to event streams |
| Real-time transaction ledger | Bloomberg terminal | A list of events, rendered chronologically |
| Instant balance reconciliation | FinTech infrastructure | A fold over deposit/withdrawal events |
| Overdraft protection | Risk management system | A type-level constraint the compiler enforces |
| Inter-account transfers | Payment settlement network | Two commands in two bounded contexts |
| Complete audit trail | Regulatory compliance engine | The event store, which you already have |
| Historical replay | Time-travel debugging | Replaying events to a point in time |

> The magic: the left column sounds like it requires a team of 20. The right column is what the reader actually builds. NeoHaskell makes the right column produce the left column.

### The Progression

| Part | Title | Alex's goal | What the reader builds | Events introduced | The "wow" moment |
|------|-------|------------|----------------------|-------------------|-----------------|
| 1 | **Your First Transaction** | "I want to deposit money" | Event type, command handler, event store, balance as fold | `AccountOpened`, `MoneyDeposited` | "That's 12 lines of code?" |
| 2 | **Account Rules** | "Don't let me overdraw" | Validation, declined commands, compiler catching illegal states | `MoneyWithdrawn`, `WithdrawalDeclined` | "The compiler just prevented a real bug" |
| 3 | **Transaction History** | "Show me what happened" | Projections, read models, formatted statement output | (consuming existing events) | "A bank statement IS a projection" |
| 4 | **Multiple Accounts** | "I need a savings account too" | Multiple aggregates, account-scoped events | `AccountOpened` (savings variant) | "Adding a new account type was trivial?" |
| 5 | **Transfers** | "Move money between accounts" | Cross-aggregate commands, saga/process manager | `TransferInitiated`, `TransferCompleted` | "Two bounded contexts, and it just works" |
| 6 | **Audit Everything** | "Show me everything that ever happened" | Event replay, time-travel queries, full audit trail | (replaying all events) | "I accidentally built a compliance engine" |

### The Rosetta Stone (appears in Concepts, referenced from Tutorial Part 1)

| What Alex says | Banking term | NeoHaskell term | What it actually is |
|---------------|-------------|-----------------|-------------------|
| "Something happened" | Transaction | Event | An immutable record of a fact |
| "Do something" | Deposit slip | Command | A request that may be accepted or rejected |
| "What's my balance?" | Account statement | Projection | State derived by folding over events |
| "My account" | Account | Aggregate | A consistency boundary for related events |
| "Show me everything" | Audit log | Event Store | The append-only source of truth |
| "Retail vs. Lending" | Business division | Bounded Context | An independent model with its own events |

### Design Constraints

- Each part builds ONE concept on top of the previous
- Each part results in running code the reader wrote themselves
- Each part starts with a product requirement Alex would write ("I want to...")
- Part 1 must produce visible output within 10 minutes of starting
- Part 6 deliberately echoes NeoHaskell.org's pitch: "Banks don't UPDATE your balance"

### Disclaimer (appears on tutorial page 1)

> NeoBank is a teaching example, not a production banking system. It doesn't handle
> multi-currency, regulatory compliance, concurrent access, or interest calculation.
> Those are real problems — some are covered in the advanced guides. What NeoBank
> demonstrates is how event sourcing makes complex-sounding features trivially simple.

### The Opening Line of the Tutorial

> *"Every bank in the world runs on the same idea: don't change the number, record what
> happened. A deposit isn't 'set balance to $150' — it's 'recorded: $50 deposited.'
> NeoHaskell is built on this same idea. Let's prove it works by building a bank.*
>
> *By the end of this tutorial, your NeoBank will handle accounts, transactions, transfers,
> and a complete audit trail. It'll feel like enterprise software. It was 200 lines of code."*

### Second Example Domain (for advanced guides)

**Logistics / shipment tracking.** A package moves through locations, status changes are events, delivery is a projection. Different enough from banking to prove the pattern generalizes. Universal enough that everyone understands it.

---

## Tutorial Layer System

Every tutorial page operates on three simultaneous layers. The reader engages with layer 1 (the narrative) and dips into layers 2 and 3 as curiosity demands.

### Layer 1: The NeoBank Narrative (mandatory, always visible)

This is the tutorial. Alex wants to build a feature, writes the code, sees the result. The reader follows along. NeoHaskell syntax is used but never formally introduced — the reader absorbs it by doing.

### Layer 2: "New Syntax on This Page" Aside (expandable sidebar)

Each tutorial page includes a collapsible aside listing every new language construct introduced on that page. Format:

```
<details>
<summary>New NeoHaskell on this page</summary>

- **`|>` (pipe operator)** — passes the left side as the last argument to the right side. Like Unix pipes for code.
- **`do` blocks** — how you sequence steps in NeoHaskell. All variable binding happens here.
- **`Result.ok` / `Result.err`** — success or failure, enforced by the compiler.

</details>
```

Rules:
- Maximum 3-5 constructs per page (if more, the page is doing too much — split it)
- Each construct gets a one-sentence explanation, not a full lesson
- Links to the relevant concept page for deeper understanding
- First occurrence of a construct is bold in the tutorial text itself

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

| Tutorial Part | Layer 2: New Syntax | Layer 3: Concept Links |
|--------------|--------------------|-----------------------|
| 1. First Transaction | `do`, `\|>`, type declarations, `Task`, `Module.yield` | Events, Commands, Event Store |
| 2. Account Rules | `case..of`, `Result`, pattern matching on types | Aggregates, Validation |
| 3. Transaction History | Record syntax, `Array.map`, `Array.foldl` | Projections, Read Models |
| 4. Multiple Accounts | Module structure, qualified imports, `Map` | Bounded Contexts |
| 5. Transfers | `Task.andThen`, error propagation, `[fmt\|...\|]` | Sagas, Process Managers |
| 6. Audit Everything | Event replay functions, time-based queries | Event Store internals |

> **Principle**: If a reader skips every aside and every link, they still complete the tutorial and have working code. The layers add depth, not dependencies.

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
| 1. First Transaction | `events-not-state.mdx`, `commands-and-handlers.mdx` | Reader just stored their first event and issued their first command |
| 2. Account Rules | `type-safety.mdx` | Reader saw the compiler reject an invalid withdrawal — explain why types do this |
| 3. Transaction History | `projections.mdx` | Reader just built a bank statement — the concept is fresh and concrete |
| 4. Multiple Accounts | `bounded-contexts.mdx` | Reader has two account types — explain why they're separate |
| 5. Transfers | `effects.mdx` | Cross-aggregate operations need effect explanation |
| 6. Audit Everything | `concurrency.mdx` | Event replay at scale needs concurrency explanation |

### Concept Page Template (internal)

Every concept page follows this structure:

```markdown
---
title: [Concept Name]
description: [One sentence]
entry_stage: [Mental model stage required]
exit_stage: [Mental model stage after reading]
misconception: [Which misconception from the 20 this addresses]
tutorial_anchor: [Which tutorial part first links here]
---

## The One-Sentence Version
[If the reader reads nothing else, this sentence should shift their mental model]

## In NeoBank Terms
[Explain using the banking domain they already know from the tutorial]

## The Full Picture
[Complete explanation with code examples]

## How NeoHaskell Enforces This
[What the compiler/linter does to keep you on the right path]

## Going Deeper (optional)
[Academic references, advanced patterns, links to external resources]
```

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
| Coming from CRUD | REST endpoints, SQL updates | `UPDATE balance` → record `MoneyDeposited`; `SELECT balance` → fold over transactions; REST endpoint → command handler |
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

### Exercise Placement

- **Tutorial pages**: 1 Modify + 1 Extend per page (mandatory)
- **Concept pages**: 1 Predict or 1 Break per page (optional but encouraged)
- **Guides**: 0 exercises (guides are task-oriented, not learning-oriented)

### Bloom's Taxonomy Progression (mapped to tutorial parts)

Exercises follow a deliberate cognitive progression — readers move from rote execution to creative composition.

| Tutorial Part | Bloom's Level | Exercise Pattern | Example |
|--------------|--------------|-----------------|---------|
| 1. First Transaction | **Remember** (Copy) | Copy-paste the code, run it, see the output. Change one value. | "Change the deposit amount to $200. Run it. What's the new balance?" |
| 2. Account Rules | **Understand** (Modify) | Change behavior within existing structure. | "Add a `MinimumBalance` rule — withdrawals below $10 remaining are declined." |
| 3. Transaction History | **Apply** (Replicate) | Build a similar feature from scratch using the same pattern. | "Create a `MonthlyStatement` projection that groups transactions by month." |
| 4. Multiple Accounts | **Analyze** (Decompose) | Identify which pattern to apply and why. | "Should `AccountType` be an event or a field on `AccountOpened`? Argue both sides." |
| 5. Transfers | **Evaluate** (Critique) | Judge a given solution and find the flaw. | "This transfer handler doesn't check the source balance. What goes wrong? Fix it." |
| 6. Audit Everything | **Create** (Compose) | Build something new that combines multiple concepts. | "Build a `FraudAlert` projection that flags accounts with 3+ declined withdrawals in 24 hours." |

> The progression ensures that by part 6, the reader isn't following instructions — they're making design decisions. This is how we know they've internalized the mental model.

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

**Goal**: Buildable infrastructure and validated learner assumptions.

Tasks:
- [ ] Set up code block extraction + compilation CI
- [ ] Validate the 20 misconceptions list with 5+ real developers
- [ ] Finalize NeoBank domain model (account types, events, commands)
- [ ] Study reference docs: Rust Book (progression), Stripe (reference quality), react.dev (teaching method)
- [ ] Create `ARCHITECTURE.md` documenting the Diataxis-based structure decision
- [ ] Update `astro.config.mjs` sidebar to match the new section plan
- [ ] Write `getting-started/reading-neohaskell.mdx` — 5-minute annotated code walkthrough ("Reading NeoHaskell"). Take a real 20-line NeoHaskell snippet and annotate every line with what it does and why it looks like that. Julia Evans style — visual, friendly, zero prerequisites.
- [ ] Write `getting-started/cheat-sheet.mdx` — "I want to... / Write this..." quick reference. Two-column format: left column is intent ("bind a variable", "handle an error", "iterate over a list"), right column is NeoHaskell code. Printable. Fits on 2 pages.

**Output**: CI pipeline, validated misconceptions, confirmed example domain, updated site config, "Reading NeoHaskell" page, cheat sheet.

### Phase 1 — The First Real Page (Week 2)

**Goal**: One complete, published "Quick Start" page. Not a skeleton. Real content.

Tasks:
- [ ] Write `getting-started/installation.mdx` — Nix setup, first `neo` command, "Hello Transactions"
- [ ] Write `getting-started/first-events.mdx` — Define an event type, deposit money, see balance derived from events
- [ ] Write `guides/using-ai.mdx` — Copy-paste NeoHaskell prompt for AI tools, the 5 most common AI mistakes (generates `pure` instead of `Task.yield`, uses `where` blocks, writes point-free, uses `Either` instead of `Result`, uses unqualified imports), how to spot wrong patterns, when to trust vs. override AI suggestions
- [ ] Every code block compiles in CI
- [ ] Test with someone who's never seen NeoHaskell (observe, don't help)
- [ ] Fix everything the test revealed
- [ ] Verify the Tutorial Layer System works — first-events.mdx must have all 3 layers (narrative, syntax aside, concept links)

**Output**: Two real pages that a developer can follow from zero to "I deposited $100 and saw my balance update from events." Plus an AI usage guide and language orientation pages.

**Why this is Phase 1, not Phase 4**: You discover 80% of structural problems by writing the first 2 pages. Writing early is research.

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
    index.mdx                        # "Meet Alex. He's building a bank."
    01-first-transaction.mdx         # Deposit money, see events, derive balance
    02-account-rules.mdx             # Overdraft protection, declined commands
    03-transaction-history.mdx       # Projections, bank statements as read models
    04-multiple-accounts.mdx         # Savings accounts, multiple aggregates
    05-transfers.mdx                 # Cross-account transfers, bounded contexts
    06-audit-everything.mdx          # Event replay, time-travel, audit trail

  concepts/
    index.mdx                        # "How to read this section"
    events-not-state.mdx             # Core mental model shift
    commands-and-handlers.mdx        # Input processing
    projections.mdx                  # Deriving state from events
    bounded-contexts.mdx             # System decomposition
    type-safety.mdx                  # Why the compiler is your friend
    effects.mdx                      # How NeoHaskell handles side effects
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
    crud.mdx
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
| Analyzing 12 documentation sites | Diminishing returns after 3. Time is better spent on learner research. |
| Separate `research/` and `structure/` directories | Intermediate artifacts that rot. Ship real content instead. |
| Writing all skeletons before any prose | Creates false sense of progress. Write real pages, discover structure. |
| One linear flow for all audiences | Decision-makers, architects, and developers have different needs. |
| Hand-writing all reference docs | Generate from source. Hand-write only what needs narrative. |
| Phase-gated approach (finish Phase N before starting N+1) | Iterate. Write → test → learn → restructure → write. |
| Separate "Learn NeoHaskell" section before the tutorial | Forces readers through a grammar chapter before they have context. Language is learned by using it, not by studying it. The tutorial teaches syntax implicitly; concept pages teach it explicitly. |
| Formal grammar reference page | NeoHaskell is a dialect, not a new language. A grammar spec signals "this is academic" instead of "this is practical." The cheat sheet and concept pages cover everything a developer needs. |
| Beginner / Intermediate / Advanced language tiers | Artificial divisions that create anxiety ("Am I intermediate yet?"). Instead, the tutorial progression naturally moves from simple to complex syntax. The Bloom's taxonomy progression handles difficulty scaling without labeling readers. |

---

## Success Metrics

How we know the docs are working:

1. **Time to first deposit**: A new developer goes from zero to depositing $100 and seeing their balance derived from events — under 10 minutes.
2. **Tutorial completion rate**: >50% of people who start tutorial part 1 finish part 4. Part 4 is the "bank statement" moment — where projections click. This is the critical retention point.
3. **Discord question reduction**: Recurring questions decrease month-over-month.
4. **Contributor PRs**: At least 1 external docs PR per month (evidence that docs are contributor-friendly).
5. **Search satisfaction**: People who use site search find what they need (low "search → bounce" rate).
6. **"I built a bank" effect**: Tutorial readers describe NeoBank as "impressive" or "real" in feedback — evidence that the domain creates the right emotional response.

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
