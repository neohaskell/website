# Written language

Write file contents, code comments, documentation, Git commit messages, and other repository artifacts in English, regardless of the conversation language. Translated prose files and localized message catalogs use their target language. Keep conversation replies in the user's language.

# Task completion

Finish each task with the applicable checks and a local commit in English. During the current documentation authoring phase, keep the development preview available and publish only when the owner explicitly requests it. Do not push an iteration when that push would trigger deployment. When publication is requested, push to the current branch's remote and verify the push and deployment. If the remote or credentials are missing, preserve the local commit and report the exact blocker.

Keep exactly one local server for this repository, at `http://127.0.0.1:4323`. After each completed batch of changes, restart it before browser verification and sharing the URL. Stop other servers belonging to this repository; use the process ownership checks and maintained commands in README.md. Verify navigation and changed content on that same server. A build needs no second preview server.

# Copy and auxiliary text

For homepage and documentation writing, use the repository-local [neohaskell-content-review](.agents/skills/neohaskell-content-review/SKILL.md) to check concepts against what the reader has learned, starting from the homepage and following the intended documentation sequence. Keep the review and edits within the requested surface.

Write primary content, necessary labels, and direct actions. Preserve NeoHaskell's opinionated voice and approved developer-centered philosophy through `$neohaskell-positioning`; generic restraint against marketing must not dilute those convictions. Follow `$nick-plain-copy` for substantive prose and endings without duplicated navigation. Keep substantive explanations in the main prose and place source links inline where they support a claim.

Do not add footnotes, caption bars, widget identity labels, language badges, or explanatory rows around examples and actions. Remove redundant reassurance beneath buttons and text that merely describes the component beside it. Use standard visual affordances for scrolling instead of instruction sentences. Keep accessible control names and meaningful domain labels.

Middle dots (U+00B7), em dashes (U+2014), and emojis are forbidden in authored interface text, metadata, documentation, and agent responses. Use ordinary sentence punctuation. An ASCII hyphen is sufficient when a separator is necessary.

Before delivery, inspect the rendered page for auxiliary text and remove anything that does not help the visitor understand the main content or take a necessary action. A passing design review does not override this rule. Run the repository copy check with the normal build; it rejects forbidden characters and explicit footnote markup in authored source.

# Unslop quality standard

For NeoHaskell visual direction, compositions, diagrams, typography, motion, or recurring design inconsistencies across the homepage and documentation, use the repository-local [neohaskell-visual-language](.agents/skills/neohaskell-visual-language/SKILL.md). It separates accepted identity, current implementation, and experiments while the visual language develops.

Apply `$unslop` to every task in this repository. Read `.agents/skills/unslop/SKILL.md` at the start of each task, reusing it when already loaded, and check the result against its relevant guidance before delivery. Use its visual workflow for interface work and its clarity, restraint, and consistency principles for code, documentation, copy, and responses.

Explicit user direction and the accepted NeoHaskell product and design records take precedence over generic style prescriptions. Continue already-authorized work through the workflow below; Unslop's direction step does not require another approval when the user has authorized implementation.

# Compare and build workflow

When the user asks to compare this site with a reference, requests a design critique, or suggests a possible improvement (such as “perhaps missing some code snippets”), carry the work through implementation and presentation. Treat the request as authorization to choose sensible changes within that scope, including when the comparison is delegated to a subagent. An explicit request for review only overrides this default.

Own the design decision and its execution. Read reviewer proposals yourself, select the strongest supported direction, build it, and present the verified result. Do not hand the user a composition document or alternatives as their next task. After three unsuccessful fixes, stop repeating the same approach, name and reassess the doubtful assumption, then continue with the resulting replacement within the authorized scope. Pause only for a genuine blocker or an explicit request to stop; a review handoff is not task completion.

1. **Checkpoint first.** Review the working tree and create a local Git commit preserving the current project state, including relevant untracked source and assets. Exclude secrets, generated output, and unrelated files. Verify and record the commit SHA before comparing or dispatching reviewers. If the tree is already clean, record the current HEAD as the checkpoint.
2. **Compare.** Inspect the current implementation and the actual reference, using desktop and mobile views where relevant. Identify concrete improvements against the user's focus and the accepted product and design direction.
3. **Build.** Choose and implement the useful recommendations in the same task, preserving accepted choices outside the requested scope. Complete the applicable skill reviews and checks. Continue from findings into implementation without asking the user to reauthorize that work.
4. **Show.** Verify the result in the browser and present a working preview URL or screenshots, the visible improvements, relevant check results, and the checkpoint SHA. Finish with something the user can inspect, rather than ending at recommendations or asking them to investigate candidate source code.

# Product positioning

For NeoHaskell homepage work, product explanations, metadata, or capability evidence, load `$neohaskell-positioning`. It owns the approved benefits-first pitch for NeoHaskell as an AI-first programming language, including the accepted homepage direction. If discovery has not refreshed, read `${CODEX_HOME:-$HOME/.codex}/skills/neohaskell-positioning/SKILL.md`; use `PRODUCT.md` for repository-specific facts and `GLOSSARY.md` for terminology.

# Page-by-page documentation effort

The active [documentation map](https://plane.p.egregore.io/egregore/projects/5535680e-e3f6-4023-b400-2b27544130e2/issues/997c4e07-bfa9-4543-92bd-2e415f3e4ed1/) owns the page queue and accepted operating agreement. “Wayfinder continue with the next page” means load that map, address any newly supplied feedback, then claim its first open, unblocked, unassigned page ticket. Use the shared Wayfinder skill at `/Users/nick/egregoreOS/.agents/skills/wayfinder/SKILL.md` and Plane helpers; never infer the next page solely from filesystem order.

This map explicitly includes execution. Complete one page per session in the approved sidebar order, starting with Create a project and skipping Install NeoHaskell while the installer is in development. Preserve the four owner-reviewed introductory pages. Resume the map's assigned `docs-page-loop` worktree and branch so each session inherits completed pages and skill improvements; allow only one writing session at a time.

Each page ticket owns source investigation, complete writing, independent technical and pedagogical review, fixes, applicable checks, rendered desktop/mobile verification, and an English local commit. Independent review agents are authorized for this effort. Deliver at `http://127.0.0.1:4323` before owner review. Publication means this local preview only: no push or hosted deployment. Follow README.md to keep exactly one repository server and verify it serves the active worktree.

Improve existing repository skills through real page work and owner feedback, rather than first building a workflow engine. Guidance must support creating, modifying and deleting content, source investigation, editorial voice, and fit within the reader's learning path. Use the four approved pages as the philosophy and teaching baseline. Help readers trust their AI agent by understanding and reviewing bite-sized code; Counter introduces the starter, and the mug shop remains the recurring application example elsewhere. Keep internal verification findings out of public disclaimer copy without inventing successful evidence.

A finished page does not wait for owner approval to close its ticket. Record checks, review outcomes, source revision, local commit and preview URL in its resolution, verify closure, and add a linked gist to the map. Apply later feedback to the page and its owning skills. Continue the next page in a fresh Orca Codex session using `orca agent launch --agent codex`, carrying the map, next ticket, worktree and latest progress; stop ownership after verified delivery. Use one literal prompt argument, preserve configured gpt-6.1-sol and medium effort, verify the workspace/session/prompt receipt, and inspect an uncertain receipt before using its operationId retry route. Do not use Herdr or another provider. Store run receipts and review evidence under ignored `.agents-state/`. Tracker tickets remain the queue, not a duplicated local checklist.

The owner renewed autonomous execution for all remaining documentation, including the agreed new pages, on 19 November 2026. Carry this authorization into every continuation. Complete one claimed page per session, then launch and verify the next session without waiting for another owner prompt. Continue until all in-scope pages are written, reviewed and verified, or a genuine blocker leaves no independently actionable page. Keep Install NeoHaskell skipped while its installer is in development. Fake/hybrid mode remains deferred; integration behavior can be taught and tested against a controlled local service. Prefer supported current behavior over unsupported optional tutorial promises, record scope adjustments and remaining framework gaps on the owning tickets, and never fabricate runtime success or implement the framework to complete a documentation task. Preserve genuine dependency blocks and other claims. The queue includes Test integration behavior, Create a reusable integration, Manage integration resources and Integration contracts; do not lose these pages when reconciling the sidebar.

# Documentation direction

For documentation authoring, revision, deletion, and conceptual review, load the repository-local [neohaskell-content-review](.agents/skills/neohaskell-content-review/SKILL.md), its authoring reference, and its [documentation voice](.agents/skills/neohaskell-content-review/references/documentation-voice.md). Use the Rust Book's explanatory progression and descriptive headings, not a chain of imperative sections or repeated marketing benefits. Prefer relevant local documentation links over upstream links for reader navigation, even when the destination is still a page plan. Link upstream sources inline when they support a factual claim. This documentation-specific voice overrides generic technical-writing prescriptions for imperative tutorial headings and uniformly clipped sentences.

Keep supporting tools in proportion to the NeoHaskell lesson. Mentioning that Neo uses Git and makes an initial commit is enough on a creation page; Git inspection commands and similar detours into Nix, build tools, or editors belong only where the reader's task requires them.

The content-review skill owns page purpose, source investigation, direct-entry context, prerequisite progression, and independent pedagogical review. This replaces the unavailable `neohaskell-docs`, `documentation-entry-points`, and `neohaskell-progressive-explanations` skill routes without discarding their accepted teaching direction.

For documentation styling or navigation design, use [neohaskell-visual-language](.agents/skills/neohaskell-visual-language/SKILL.md) and the existing documentation surface record. For a rejected direction, corrected feedback scope, conflicting constraints, or ambiguous design reference, load `$design-feedback`. Later explicit user direction supersedes the baseline within its stated scope.

# Translation

Translate only when the user explicitly requests translation, and only for the requested pages and languages. English authoring, glossary edits, shared-component changes, publication requests, and failed CI checks do not authorize translation work. Preserve existing translations during those tasks and report missing files without generating translations or English placeholders to make CI pass.

For translation requests, follow the page-based workflow in README.md. Ask one scope question if the requested pages or languages are unclear. Edit the translated MDX directly, preserving human corrections. Page plans are excluded unless explicitly requested. Update shared catalog keys only when their text is in scope. Preserve relative links and explicit heading anchors. Review changed passages, code, links, and affected rendering within the requested scope. Translation does not authorize deployment.

CI must fail when any configured language is missing a translated documentation file or required catalog file. Use `npm run check:translations` to report the missing paths. Native Starlight fallback remains available for local authoring, but does not satisfy the deployment check. Existing translations may lag behind English edits; file presence is required, freshness is not.

# Diagram colors and terminology

For NeoHaskell diagrams, load `$neohaskell-diagram-colors`. For inline Event Models connecting screens and domain nodes, load `$neohaskell-event-model-diagrams`. For domain prose, diagrams, examples, or API explanations, load `$neohaskell-terminology`; `GLOSSARY.md` owns the vocabulary. If discovery has not refreshed, read `${CODEX_HOME:-$HOME/.codex}/skills/<skill-name>/SKILL.md`. Reuse `src/styles/diagram-colors.css` for diagram colors across documentation and the homepage.

# Teaching examples

For every substantive documentation page, identify its hardest relationship or mechanism and add a purpose-built visual explanation beside the relevant prose. Show data flow, dependencies, state transitions, or failure and recovery as appropriate. Screenshots and code excerpts do not replace that explanation. Use the reviewed introductory lessons as the teaching baseline. Static diagrams are appropriate for stable relationships; animation should reveal sequence or transformation, with reduced-motion and static alternatives. Place figures immediately beside the explanation; put a page-premise figure near the opening. Use connected shapes, a clear focal point and reading path, and danger color plus a non-color cue for failures. Avoid text-card grids, decorative filler or mandatory interaction. The content-review authoring reference and visual-language skill own this practice; complete the fresh teaching-example review and correction loop.

When choosing or simplifying a teaching graphic, load `$minimal-teaching-visuals` for representation choice and the test of what each element contributes to the explanation.

For NeoHaskell code examples, load `$neohaskell-code-examples`. It owns record layout with opening braces beside constructors and commas at field ends, plus local bindings inside `do` blocks.

Whenever creating or materially changing a teaching example, use `$learning-example-review` from the active Codex skills directory. Read its `SKILL.md` and complete its fresh-subagent visual review and simplification loop before finishing. If discovery has not refreshed, resolve it at `${CODEX_HOME:-$HOME/.codex}/skills/learning-example-review/SKILL.md`.

Keep examples focused on the concept being taught, with readable text and a visible cue when scrolling changes their state. Preserve the approved NeoHaskell identity and the terminology in `GLOSSARY.md`.
