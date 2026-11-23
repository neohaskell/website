# NeoHaskell vocabulary

Canonical website and diagram terminology, confirmed by the owner on 2026-11-06 and extended on 2026-11-13. Use these terms in future copy, diagrams, and domain identifiers.

NeoHaskell is an AI-first **language** with integrated application capabilities. Use NeoHaskell or the language for the product; framework names an actual implementation layer when that distinction matters. The [positioning reference](.agents/skills/neohaskell-content-review/references/positioning.md) owns the approved positioning and the One Person Framework philosophical reference.

| Term | Meaning in the website's explanatory model | Replaces |
| --- | --- | --- |
| Command | An intent to change application state, such as placing an order for one mug. | Request |
| Decision | Evaluates a command against current state and domain conditions; determines whether to accept or reject it. | Rule |
| Event | A recorded fact about something that happened, such as `OrderPlaced`. | Unchanged |
| Query | Maintains and exposes answers derived from application state. Distinguish the query definition, its maintained state, and the answer returned to a caller. | Read model, view |
| Integration | Connects application behavior to other work or external systems. Outbound integrations react to recorded events and can perform effects or issue commands; inbound integrations introduce commands from outside. | Translation, translator |

The illustrative online shop follows **command, decision, event, then query**. A customer places an order for one mug. The decision accepts it if stock is available, recording `OrderPlaced`; the query then returns two mugs in stock instead of three. When sold out, the decision rejects the command, no `OrderPlaced` event is recorded, and the query result remains zero. Accepting an order consumes one unit of stock in this teaching model. The ledger displays events from this order; opening stock is provided as the example's initial state.

This illustration does not claim that every command in every application emits exactly one event. It illustrates one order and its stock effect; payments, shipping, and concurrent orders are outside this first example. Explain these concepts in documentation before introducing installation and AI-agent setup. The homepage uses the same vocabulary in one initial animation, without an interactive lesson. The visual study's controls are review tools, not approved website components.

Keep the event record distinct from the query result. The history retains the event; the query returns the current state. The AI agent helps the builder create the application and is not the runtime decision maker in this model.

Archived image prompts and prior visual rounds retain their original wording as provenance. This glossary takes precedence for all new and revised work. HTTP requests, browser APIs, and SVG `viewBox` remain ordinary technical terms.

## Application structure

| Term | NeoHaskell meaning |
| --- | --- |
| Entity | The state and event boundary a command acts on. Use the actual `Entity` and `EntityOf` contracts, rather than introducing aggregate as a competing label. |
| Feature | A named group of chapters in the application's Event Model. It is not automatically a bounded context. |
| Chapter | A named group of slices within a feature. |
| Slice | In the inspected DSL, a named unit with exactly one primary command, query, outbound integration, or inbound integration declaration, plus its rules. A whole user journey can cross several slices. |
| Transport | An interface such as HTTP, MCP, or the application CLI through which a caller accesses application behavior. It is distinct from an integration. |

The introduction's order-and-cancellation journey has four vertical slice columns. Place order groups the state-change slice and the stock-query slice; Cancel order groups the corresponding cancellation slices. Each chapter is shown by a left-to-right arrow spanning its slices, with its name above the arrow. The repeated Available mugs nodes show query answers at different points in the process. A single DSL slice does not contain a command, decision, event, and query as four primary declarations.

## Source names and comparisons

Owner terminology governs explanatory labels. Exact API identifiers retain their spelling. At source revision `033ae4a570686cd885ca14c6595997d9498d5287`, [Service.EventModel](https://github.com/neohaskell/NeoHaskell/blob/033ae4a570686cd885ca14c6595997d9498d5287/core/service/Service/EventModel.hs) exposes `query`, `outboundIntegration`, and `inboundIntegration`, while internal constructors still include `StateView`, `OutboundTranslation`, and `InboundTranslation`. Those internal names do not rename the concepts in documentation.

[Query.Core](https://github.com/neohaskell/NeoHaskell/blob/033ae4a570686cd885ca14c6595997d9498d5287/core/service/Service/Query/Core.hs) defines `Query`, `QueryOf`, and `combine`. Query projection describes the mechanism that maintains query state; it is not a replacement domain label. [Integration](https://github.com/neohaskell/NeoHaskell/blob/033ae4a570686cd885ca14c6595997d9498d5287/core/service/Integration.hs) defines inbound and outbound APIs. These sources were inspected, not executed.

Keep ordinary meanings such as HTTP request, browser view, SQL view, language translation, and business rule when they name those actual things. When comparing another method, introduce its term explicitly and then use the NeoHaskell term for the application. Do not perform global word substitutions on source, quotations, localization workflows, or historical records.

## Storage and modeling terms

| Term | Meaning and distinction |
| --- | --- |
| Event Sourcing | Derive application state from retained business events. It describes the authoritative history, not a guarantee of infinite or undeletable storage. |
| Snapshot | An internal performance optimization: derived state saved at a known history position. NeoHaskell uses snapshots to avoid replaying earlier events on each fetch. Stock snapshots alone cannot explain the business reason for a change; richer snapshots retain only the meaning actually modeled. |
| CQRS | Separate the handling of requested changes from the handling of questions. NeoHaskell uses commands for the former and queries for the latter. Query maintenance and reading an answer are distinct operations. |
| Event Modeling | Design and examine a process by connecting screens, commands, events, and queries. It is a modeling method, distinct from event-based storage and separate read/write responsibilities. |

The introductory history lesson explains why growing history does not require full replay for routine work, without requiring readers to learn the internal snapshot mechanism. Its animated example records stock received, order placed, and order cancelled, then recalculates the same history. Replaying does not append new events.

The [decision algebra](https://github.com/neohaskell/NeoHaskell/blob/033ae4a570686cd885ca14c6595997d9498d5287/core/service/Decider.hs) defines the operations available to a decision. Describe decisions as separating business logic from arbitrary external actions, without claiming their interpreter performs no effects at all.
