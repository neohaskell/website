---
name: neohaskell-content-review
description: Author and review NeoHaskell documentation within the reader's learning path. Use when creating, revising, or deleting documentation, or reviewing homepage explanations for untaught concepts. Covers source investigation, page purpose, direct-entry context, and pedagogical review while preserving the requested scope.
---

# Write within the reader's learning path

For documentation creation, revision, or deletion, follow [authoring a page](references/authoring.md) and [documentation voice](references/documentation-voice.md). Use the voice reference for both conceptual lessons and ordered procedures. For installation pages, use its terminal walkthrough to review the rendered instructions. For a bounded copy review, use the prerequisite review below without expanding the edit scope.

## Teach the difficult part visually

Every substantive page should make its hardest mechanism or relationship visible in a purpose-built figure, following the authoring reference. Review the visual explanation as part of the page's teaching, not as optional decoration after the prose. Screenshots and code alone do not satisfy a data-flow explanation. Use the accepted introductory lessons as the baseline and complete the fresh teaching-example visual review.

## Review the reader's prerequisites

Review what the reader must understand to follow the argument, not just which technical words appear. Use the actual page order and preceding explanations as evidence of what has been taught. A glossary entry, later page, heading, source excerpt, or screenshot label does not by itself teach a concept.

## Establish what the reader knows

Read the changed passage in its rendered reading order, then the relevant preceding sections and pages. Start the default journey at the homepage and follow the documentation's introductory sequence. [Reading context](references/reading-context.md) locates the current sources and distinguishes introduction from mastery. Resolve navigation from current links, `astro.config.mjs`, and page frontmatter; the reference is a starting map rather than permission to reorder pages.

Assume familiarity with ordinary goals such as building an app, asking an AI agent for help, and checking whether the result does what was intended. Do not assume knowledge of Haskell, functional programming, Event Modeling, or NeoHaskell's internal vocabulary. A guide or reference can have a more specific audience when its prerequisites are explicit.

For each dependency needed by the changed argument, identify where the reader gets its meaning: already explained earlier, explained here, or still unintroduced. An explanation should connect the name to a concrete purpose or behavior before using it to explain another new idea. Several familiar-looking nouns in one sentence can still introduce several unfamiliar concepts.

## Match the surface

On the homepage, lead with the problem and developer gain in ordinary language. Introduce a named tool through the job it helps the developer do. The visual IDE helps a person understand and review a growing application; its internal diagram taxonomy is unnecessary to that promise. Keep the approved opinionated pitch and useful specificity, without turning the landing page into the first technical lesson.

Technical names may appear as optional product evidence or in a feature catalogue when the surrounding prose makes their value understandable without decoding them. Exact identifiers inside source excerpts and authentic screenshots remain intact. Inspect their introductions and labels too: essential meaning must not depend on understanding that syntax or recognizing a later concept. Visible terminology and required prior knowledge are different questions.

In documentation, motivate a concept through a concrete situation, explain its meaning, name it, then use it in the next step of the argument. A brief earlier mention does not license an unexplained mechanism later. Reuse established examples with a short reminder where needed. Preserve the project's domain vocabulary once the concept has been introduced; ordinary language is a bridge into precise terms, not a competing permanent terminology.

Sequential reading is the default review path, not an access restriction. For a direct landing or homepage deep link, the opening or targeted section must provide the necessary context or a named prerequisite link. Avoid relying on positional phrases such as "as above" or asking the visitor to complete every earlier page.

For an out-of-scope destination with missing entry context, report the dependency and distinguish an in-scope link change from a destination-copy repair. Do not silently expand the edit scope or assume every feature action should lead to the beginning of the docs.

## Repair the dependency

Prefer the smallest correction that lets the reader follow: express the benefit in ordinary language, introduce the concept beside its first necessary use, move the mechanism to the page that teaches it, or provide a named contextual link. Choose based on the page's purpose. A link alone cannot explain the central promise of a homepage section.

Read the corrected heading and opening sentence together. They should communicate the gain without requiring later vocabulary. Then check the complete argument and its example for accuracy: simplification must preserve meaningful differences such as requested changes, accepted facts, and current answers. Product claims remain governed by `PRODUCT.md` and positioning; term meanings remain governed by `GLOSSARY.md`. Trust-oriented copy should support human understanding and judgment, not imply automatic correctness.

## Return findings within scope

Return at most five material findings, each with the passage location, assumed concept, preceding teaching evidence or its absence, likely reading difficulty, and a concrete revision. Mark missing source context as unverified rather than guessing that a concept was taught. A review can conclude that no change is needed.

When edits are authorized, apply the in-scope corrections and inspect the resulting reading order. A homepage request stays on the homepage; reading documentation for prerequisite evidence does not authorize editing it. Translation needs its own explicit scope. Do not add public prerequisite badges, glossary panels, captions, or process notes to satisfy the review. Keep the concept-dependency notes in the review evidence, and distinguish editorial judgment from observed reader-comprehension results.
