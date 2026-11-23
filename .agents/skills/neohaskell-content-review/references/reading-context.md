# Reading context

Resolve paths from the repository root. Inspect the current source before relying on this map; navigation and explanations can change.

| Stage | Sources | What to establish |
| --- | --- | --- |
| Homepage | `src/components/Home.astro`, `src/components/HomepageHighlights.astro`, `src/i18n/en/site.json` | The language's developer gains and a reason to learn more. No architectural taxonomy is prerequisite. Follow actual component order, not catalog key order. |
| The work behind the code | `src/content/docs/introduction/index.mdx` | Developer judgment, the mug-shop scenario, and a plain-language introduction to connecting intentions, outcomes and displayed information. A brief Event Modeling introduction is not instruction in its detailed structure. |
| Events and history | `src/content/docs/introduction/history/index.mdx` | Events as meaningful recorded facts; current state derived from history; replay as recalculation rather than repeated business actions. Inspect the section before counting a term as explained. |
| Commands and queries | `src/content/docs/learn/index.mdx` | Requested changes, decisions, pure application logic, integrations and maintained answers are introduced in the page's own order. Later sections can depend on earlier ones. |
| Model the whole process and subsequent paths | `src/content/docs/introduction/event-modeling/index.mdx`, `astro.config.mjs`, relevant page frontmatter | Connect the previously explained parts, then introduce chapters, slices, features and evolving contracts. Setup, tutorial, guides and reference have different task-specific prerequisites; do not treat every sidebar entry as one required linear course. |

The homepage's main documentation action starts at `/introduction/`. Feature actions can enter later pages or sections directly, including `/introduction/event-modeling/`. Check those targets for context when reviewing the corresponding action, while keeping edits within the requested surface.

Use `GLOSSARY.md` to check whether a term is correct, not to infer that the visitor has read it. Use [product positioning](positioning.md) to preserve the approved developer-centered promise and distinguish product intent from verified behavior. The visual-language baseline and `AGENTS.md` own reading layout, direct-entry requirements and translation scope.

Three different evidence levels matter: a term was visible, its purpose was briefly introduced, or its behavior was explained sufficiently for the next argument. Record only the level supported by the actual prose and example. An agent's expectation that a visitor will understand a passage is an editorial judgment until tested with readers.
