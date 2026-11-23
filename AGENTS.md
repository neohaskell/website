# Repository instructions

The homepage and documentation contents are approved for publication. Preserve their wording, examples, navigation and appearance unless the owner requests a change to that surface. Write repository artifacts and commits in English, translated passages in their target language, and replies in the user's language.

## Work by task

- For page creation, revision or removal, use [content review](.agents/skills/neohaskell-content-review/SKILL.md). Its authoring and voice references govern investigation, progression and independent review. Use [page delivery](.agents/skills/neohaskell-content-review/references/page-delivery.md) for the Plane queue, one-page sessions and verified Orca handoff.
- For product claims, use the installed neohaskell-positioning skill and [source-backed positioning](.agents/skills/neohaskell-content-review/references/positioning.md). Use the neohaskell-terminology skill and [glossary](GLOSSARY.md) for domain language.
- For visual work or reference comparisons, use [visual language](.agents/skills/neohaskell-visual-language/SKILL.md). Use design-feedback when reconciling a rejected or ambiguous direction.
- For teaching examples, use minimal-teaching-visuals, neohaskell-code-examples, neohaskell-diagram-colors and neohaskell-event-model-diagrams as applicable. Materially changed examples require the learning-example-review skill and a fresh independent visual review.
- Translate only explicitly requested pages and languages using the [translation workflow](.agents/skills/neohaskell-content-review/references/translation.md). Report missing translations when CI detects them.

## Copy and delivery

Use nick-plain-copy. Keep substantive explanations in the article and sources beside their claims. Use necessary labels, accessible control names and domain language. Omit footnotes, widget badges, caption bars and redundant text around actions. Avoid em dashes, middle dots and emojis in authored text and replies.

Run the applicable tests, copy check and build, restart the single development server, and inspect the affected pages at desktop and phone widths using the [preview commands](README.md#local-preview). Finish with a local English commit. Store temporary projects, captures and run evidence under ignored `.agents-state/`.

Publish only on an explicit owner request. Then push the current branch to its remote and verify the push and deployment. If blocked, preserve the local commit and report the cause.
