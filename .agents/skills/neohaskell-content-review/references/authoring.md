# Author a complete documentation page

Use the current ticket for scope and `AGENTS.md` for delivery. This reference owns teaching and source practice, not the page queue or server lifecycle.

## Establish the page's job

Read `PRODUCT.md`, `GLOSSARY.md`, the existing page, and the relevant learning path from [reading context](reading-context.md). The four introductory lessons are the teaching and philosophical baseline. Resolve page order from the current navigation and ticket rather than inventing a new curriculum.

Write the reader's immediate goal and the visible result that completes it. Keep the material that serves that result. A creation page ends with a recognizable project; running the application and tracing all its files belong to their own pages. Mention supporting tools only as far as the task needs them. It is useful to say that Neo initializes Git and commits the generated files; a project-creation lesson does not need Git inspection commands, identity configuration, or a version-control tutorial. Apply the same boundary to Nix, build tools, editors, and other supporting tools. Keep detailed verification in internal evidence unless the reader needs it to complete the NeoHaskell task. For a conceptual lesson, state the question the reader should be able to reason about afterward.

When revising, preserve useful explanations and accepted choices outside the correction. When deleting, inspect incoming links, stable anchors, navigation, and the explanation that used to supply a prerequisite. Repair those dependencies within scope or report the concrete gap. Translation remains separately authorized.

## Investigate the actual path

Follow a command or API through its entry point, implementation, generated artifacts, and relevant tests. Read source-repository instructions before inspection. Confirm which source revision the reader's tool or starter actually uses. A local checkout, a published CLI, and the framework pinned by a generated project can all differ. Read generated files rather than treating a template or historical research as their contents.

Run the documented path in an isolated directory when tools permit. Verify every displayed command and claimed result. An interactive route needs its prompts checked; a noninteractive route needs its flags and defaults checked. Avoid environment overrides that make a check pass without exercising the reader's path. Preserve failures as evidence and investigate their cause before choosing a workaround.

Keep revisions, commands, output, review reports, and verification limits under ignored `.agents-state/`. Separate source inspection, executed behavior, and approved product intent. Public prose explains the task confidently without a source-audit sidebar. A real contradiction that prevents honest instructions is a blocker to resolve, not a disclaimer to hide in the article.

## Teach through meaningful inspection

Use [documentation voice](documentation-voice.md) for article tone and composition. Establish the subject and enough context for a direct landing, rather than beginning every page with an instruction. Use named links when reusing a prior example. Prefer a relevant local documentation page over an upstream source link for reader-facing navigation, even when the local page is not yet complete. Link external sources inline when they substantiate a factual claim or the reader needs the upstream artifact itself. Introduce unfamiliar mechanisms through their concrete purpose before using them to explain another mechanism.

AI-assisted building is the normal path. Give the agent a bounded task and the developer something specific to inspect. When manual and agent routes are alternatives, introduce the choice before the first action so sequential readers do not perform both. Trust comes from understanding small changes and checking their behavior, not from telling readers to trust an agent. Counter orients the reader to the real starter. The mug shop remains the continuing application example elsewhere.

Choose the smallest snippet that answers the current question. Explain what to notice, what the snippet omits, and whether the reader should run, edit, or only read it. For example, an illustrative configuration excerpt can distinguish an application's own version from its dependency revision without teaching the build system. Do not turn a project-creation page into a full source tour just to include code.

Preserve the convictions in `PRODUCT.md`: developer judgment, agency, enjoyment, and coherent built-in choices. Each paragraph needs a substantive job, but not every paragraph needs a new mechanism. For conceptual arguments, connect examples to the claim rather than listing capabilities. Comparisons with other approaches are an internal test of the argument, not required public concessions. Native previous/next links own onward navigation; end when the subject is resolved.

## Review and deliver

Request independent technical and pedagogical review when authorized, as required by the page effort. Give reviewers the page goal, accepted baseline, source revisions, and evidence, with a report-only write boundary. The pedagogical reviewer should challenge the argument as a fresh reader, not only check spelling or vocabulary. Ask where the reader must guess, what they can inspect, whether the example supports the intended conclusion, and whether the rendered reading rhythm fits the approved pages. Review connected explanation against the voice reference too: repeated task announcements, a chain of imperative sections, and generic benefit claims need paragraph-level repairs, not just heading substitutions.

For changed teaching examples, combine that fresh review with the installed `learning-example-review` visual loop where practical. Include desktop and phone captures at normal zoom, computed text sizes, and an accepted neighboring section. Judge the raw captures before the implementation. Apply material findings, then verify corrections against the same evidence conditions. Distinguish reviewer approval from owner acceptance and from measured reader comprehension.

Before completion, inspect the rendered article and navigation, remove redundant auxiliary text, and run the repository's applicable checks. A passing build does not establish that instructions work or that an explanation teaches. Complete only when the page's stated result, source claims, review findings, and rendered content have each been checked, or record the exact blocker in its ticket.
