# Documentation voice

Use the Rust Book's explanatory teaching voice for NeoHaskell documentation. This governs article prose and headings, not the homepage pitch, CLI messages, or this skill's operational instructions. It takes precedence over generic advice to make every tutorial heading an imperative or every paragraph a short action step.

## The reference

Read [Hello, Cargo!](https://doc.rust-lang.org/book/ch01-03-hello-cargo.html) for a project-creation lesson and [Hello, World!](https://doc.rust-lang.org/book/ch01-02-hello-world.html) for the first encounter with code. Follow their teaching moves rather than copying their sentences or product-specific details.

The Cargo chapter begins by saying what Cargo is and what work it does. It connects that tool to a program the reader recognizes. Under "Creating a Project with Cargo", the authors make a project, inspect the files, and explain what those files mean. Later, a change in build output becomes the reason to explain rebuilding. The explanation grows from something the reader has just seen.

That is the model: a knowledgeable person working through an example with the reader. It is neither a checklist supervisor nor a salesperson. Instructions still belong in the prose when there is an action to perform.

## Installation and recovery procedures

Read the [Rust Book installation chapter](https://doc.rust-lang.org/book/ch01-01-installation.html) for the progression from a terminal command to recognizable output and diagnosis. The [Go installation guide](https://go.dev/doc/install) puts the operating system choice before the commands that depend on it. Adapt those moves to the actual installer.

Use short numbered headings for mandatory stages when their order matters. These headings should make sense in the contents outline on their own. Give optional setup, troubleshooting, and later maintenance descriptive headings outside that sequence. Use an ordered list within a stage when accepting a prompt or opening a new shell must happen at a particular time.

Walk through the rendered guide with a terminal in mind. Can someone entering at the first screen identify their platform and first action? After a prerequisite check, give each materially different result its own line with the next action. A reader who installs a missing prerequisite needs a way back to the check. Place the success criterion beside example output, identify any values that vary, and give failed checks a specific recovery destination. Use copyable snippets for commands and visually distinguish the output the reader compares.

During review, scan the phone layout and the desktop contents outline. Trace one successful installation and one failure back to the point where the reader can resume. Record where a reader has to guess what to run, where to run it, or whether they have finished.

## How to write the lesson

- Use descriptive headings that name the subject or learning activity. "The project settings" and "Creating a project with Neo" fit a lesson. A sequence of "Create", "Review", "Confirm", and "Keep" headings turns the article into a task board. Gerunds alone do not fix that structure; each section needs an explanation worth reading.
- Introduce the thing before directing the reader to use it. Explain what the tool does, then make something with it. After a command or snippet, describe the actual result and what the reader should notice.
- Let the example carry the progression. Move from the generated file to its setting, from the setting to the reason it exists, or from an observed result to the mechanism behind it. Avoid restarting every section with another generic instruction.
- When a screenshot shows an interface, let it supply visual recognition. Prose should explain what the reader can do there or why the result matters, rather than inventorying the heading, navigation groups, and labels already visible. Name a control when an action depends on finding it; omit a tour of unrelated controls.
- Use "we" for reasoning and observations shared with the reader, and "you" for their environment or choices. Contractions and ordinary connective words are welcome. Do not manufacture friendliness with congratulations, exclamation marks, or repeated "Let's" openings.
- Vary sentence length as the explanation requires. Keep a longer sentence when its clauses make a relationship easier to understand. Split a sentence that carries unrelated ideas, not one that crosses an arbitrary word count. Short fragments are not a substitute for voice.
- Keep named concepts and file names consistent. Vary the sentence structure, not the technical vocabulary. Introduce a term beside its concrete meaning instead of using synonyms to disguise repetition.
- Explain the concrete reason for a choice without attaching a benefit slogan to every step. Once a Git commit has been explained as a reference for later edits, the next paragraph can discuss its output without repeating that the reader is in control.
- Use lists for a real sequence or a set the reader needs to compare. Keep connected explanation in paragraphs. A series of terminal questions often reads more naturally as prose than as a five-item checklist repeating the interface.
- Preserve meaningful limitations and exact evidence. Confidence comes from showing what happens, not from boosters such as "seamless", "essential", or "powerful". Avoid adding qualifiers merely to sound cautious when the behavior is known.
- Stop when the subject has been explained. Do not add a generic summary, a reassurance paragraph, or a duplicate of native next-page navigation.

## Editorial warning signs

These are revision prompts, not tests of who wrote a text. [VU Amsterdam's guide](https://vu.nl/en/about-vu/more-about/alp-guide-spotting-ai-writing) explicitly distinguishes AI-writing tendencies from proof of authorship. [Daytona State's checklist](https://library.daytonastate.edu/GenerativeAI/checklist) also warns that no single indicator proves AI use.

Look for these patterns across the whole article:

- Repeated sentence openings, uniform sentence lengths, and the same paragraph shape throughout.
- Heading followed by a sentence that repeats the heading, then a restatement at the end.
- Generic openings and conclusions that could describe any programming tool.
- A list of facts without a developing explanation or connection to the example.
- Excess headings, bold phrases, lists, or parallel mini-sections that break the reading into isolated instructions.
- Stock transitions, inflated vocabulary, vague sources, and assertions stronger than their evidence.
- Repeated contrasts such as "not just X, but Y", forced groups of three, and benefit slogans after ordinary operations.
- A flat, overcompressed voice with no reasoning, or decorative informality that substitutes for reasoning.
- Source claims, outputs, and examples that sound plausible but have not been checked against the actual artifact.

Correct the underlying passage. Do not insert mistakes, random synonyms, anecdotes, or artificial sentence-length variation to imitate a human or satisfy a detector. Correct grammar is desirable. Imperatives, headings, repeated technical terms, and plain short sentences are useful when the context calls for them; their mechanical repetition is the problem.

## Voice review

Read the headings together, then the opening and closing sentence of each section. Check whether they form a connected explanation or a chain of commands and promises. Read a complete section beside the Rust reference. Ask whether the text tells the reader what the example means, or merely tells them what to do next.

For owner-rejected prose, use a fresh reviewer and provide the owner's actual objection and the inspected reference. A prior technical or visual pass does not establish that the writing sounds right. Revise the actual page alongside the owning guidance, then review the new rendered reading rhythm.
