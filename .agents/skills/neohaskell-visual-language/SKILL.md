---
name: neohaskell-visual-language
description: Develop and review this repository's NeoHaskell visual language across the homepage and documentation. Use for visual direction, new compositions, diagrams, typography, motion, or recurring design inconsistencies. Separate accepted identity from experiments and review each surface for its purpose.
---

# NeoHaskell visual language

This repository owns this skill and its references. The visual language is developing. Preserve accepted choices while testing the parts that have not earned a reusable rule.

## Establish the brief

Read [authority and baseline](references/baseline.md), then the current implementation for the affected area. Follow the applicable skills in the repository's `AGENTS.md`; this skill coordinates visual decisions rather than replacing product positioning, terminology, or teaching review.

State the visitor's intended outcome and the visual question in one sentence each. The homepage persuades a developer to explore NeoHaskell; documentation helps a reader understand or do something. Both express the same identity, with different density and reading rhythm.

Keep calibration inside the requested surface. Homepage landing work uses homepage examples and ends with the landing page. Consult or change documentation only when the task explicitly includes it; a reusable visual-language rule does not expand the assignment.

For homepage headings, lead with the concrete benefit or subject. Read the full heading sequence together and remove repeated possessive openings such as "Your app", "Your data", and "Your ideas". Use "your" when ownership changes the meaning, rather than as a default way to address the visitor. Preserve natural direct address in body copy and necessary domain labels. This is a heading-rhythm rule, not a word blacklist or permission to translate other locales.

Classify each decision used by the task as owner-accepted, implemented, or proposed. Record its source and scope. An existing CSS value establishes what is rendered, not that the owner wants it preserved forever. A reviewer verdict establishes an inspected result, not owner acceptance. Resolve conflicting records against the latest applicable owner direction before treating them as review criteria.

## Develop a rule

1. Name the reader's visual question. A figure can help someone recognize an authentic result, inspect a relationship they would otherwise reconstruct, or compare meaningful states. For substantive documentation, make the page’s hardest mechanism or relationship visible by default. Omit a figure only for a short operational page with no useful relationship to show, recording the reason internally. Screenshots and code do not replace a causal or data-flow figure. Choose the smallest real section that exposes the question, and reuse the site's actual content and components. The prose establishes why the figure matters; don't narrate details already visible in a screenshot. Use the least elaborate representation that preserves the useful information, and remove any element that doesn't help answer the reader's question.
2. Inspect the nearest accepted examples and extract the relevant qualities: hierarchy, spacing, shapes, meaningful connections, type, and material. Choose a concrete treatment within the authorized scope. Keep an unfamiliar idea provisional until it has rendered evidence. Capture product screenshots in dark mode by default when the interface supports both themes, unless the task specifically needs light mode. Preserve the authentic interface rather than recoloring a light capture.
3. Implement the treatment where implementation is requested. For a rule intended for both surfaces, compare one homepage use and one documentation use, including the narrow layout and both themes. Preserve semantic meaning while adapting composition; scaling a desktop diagram down is not a phone composition.
4. Apply [the review lenses](references/review.md). Repair findings by cause and inspect the affected result again. Follow the repository's three-failed-fixes rule when the premise needs replacement. Finish with a working, verified result when implementation is in scope.
5. Record the resulting rule in its existing owner: design rationale and accepted composition in the baseline reference, values in the owning styles, and behavior in its component. Include scope, reason, evidence, and decision status. Consolidate superseded active guidance within the authorized edit. Keep experiment notes distinct from the canonical rule.

For a new language-wide decision, select the examples within the authorized scope from the calibration set in the baseline reference. For a local refinement, select only the affected lenses and occurrences. Broader testing follows shared impact, not the number of critique roles available. A structural skill review does not establish visual quality or complete the calibration step.

Place each figure immediately beside the idea it explains. When a dependency or risk motivates the whole page, show it near the opening before implementation details. Design a visible focal point and reading path: use connected shapes, meaningful symbols, position and visual hierarchy to reveal the mechanism. Do not substitute a grid of text cards for a diagram. Use danger color and a broken connection or other non-color cue for an actual failure; keep command/event/query semantic colors intact. Remove repeated labels and secondary text that compete with the main relationship. Review the figure at a glance before reading its prose.

## Return a decision

Give the selected treatment, visible evidence, unresolved uncertainty, and the next concrete experiment if exploration remains. For an implemented change, report the preview, affected checks, and local commit under `AGENTS.md`. Name the finding and its effect on the visitor instead of returning an unexplained aesthetic score.

Skill-only changes need structural validation and an instruction walkthrough; rendered acceptance applies when the interface changes. Keep all required references, future instruments, and fixtures inside this skill's directory. Add a measurement tool only when an observed recurring defect warrants it, and prove it detects the defect before relying on it.

For a requested comparison or critique, record a clean Git checkpoint, inspect the actual reference on the affected viewports, implement the supported changes, and show the verified result and checkpoint. An explicit review-only request ends with findings. After three failed fixes, reassess the assumption before continuing.
