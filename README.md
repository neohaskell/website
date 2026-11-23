# NeoHaskell website

Astro + Starlight, with a custom homepage and static documentation. Local builds target `https://neohaskell.org`. GitHub Actions publishes a preview at `https://nickseagull.dev/neohaskell.org/`. GitHub redirects `https://nickseagull.github.io/neohaskell.org/` there because the account's Pages site uses a custom domain.

## Run locally

Use Node.js 24 (or Node.js 22.23.4+).

```sh
npm ci
npm run dev -- --port 4323 --background --force
```

`npm run build` checks Astro/TypeScript and generates `dist/`. `npm run preview` serves that production build. `npm test` verifies campaign-context handling, the illustrative order model, and stable documentation links and anchors.

During documentation authoring, keep one server at `http://127.0.0.1:4323`. Restart it after every completed batch of changes, before browser verification and sharing the URL. An existing process can serve updated MDX while retaining an older Starlight configuration.

Use the project's maintained commands to stop the registered servers, run applicable checks, then start the one development server:

```sh
npm run preview -- stop
npm run dev -- stop
npm run build
npm run dev -- --port 4323 --background --force
```

`npm run dev -- status` identifies the active process and URL. Check listening processes as well when cleaning up duplicates: confirm a process belongs to this repository by its working directory and command before terminating it. For an unregistered server without a maintained stop command, use SIGTERM on its verified PID. Preserve other projects' servers. If port 4323 is occupied, resolve ownership instead of accepting a new port or using `--ignore-lock`.

Check the rendered sidebar against `astro.config.mjs` and inspect the changed content on the same development URL. A build needs no second preview server, and verification on a different origin cannot establish what the owner sees. If production serving is explicitly needed, stop development first and restore the single development server afterward.

## Main files

- `src/components/Home.astro`: shared homepage, navigation, and theme control for every language.
- `src/components/HomepageVisual.astro`: static application, event-history, pure-decision, and transport illustrations.
- `src/styles/home.css`: selected workshop identity in both themes.
- `src/content/docs/`: MDX guides for core concepts and installation/agent setup.
- `src/scripts/attribution.ts`: optional, screened five-field UTM context; same-origin navigation only, per-tab storage with a 30-minute expiry. No analytics service or durable backend events.

The homepage has no tutorial widget, replay control, scenario switcher, or concept-step interaction. Documentation teaches the vocabulary before AI-agent setup. The setup page includes a source-verified agent-skills command and links to the upstream framework environment instructions.

## Documentation

Start at `/introduction/`, then read `/introduction/history/`, `/learn/`, and `/introduction/event-modeling/` for the four introductory lessons. Installation and agent setup live separately under `/getting-started/`. The pages use Starlight's native topic sidebar, title, search, contents outline, anchor navigation, and code copying. Sidebar sections start expanded. The article scrolls normally, with examples and diagrams beside the relevant explanation.

Author English content in `src/content/docs/`. Reading styles live in `src/styles/docs.css`; prose is 18px and code/table examples are 16px. Shared lesson components live in `src/components/learn/`, including the event-history replay, Event Model, growth charts, and IDE screenshot. Code snippets illustrate the lesson and are not standalone applications. Use `learning-example-review` as required by `AGENTS.md` when creating or materially changing examples.

## Translation workflow

Use Starlight's native language folders. English lives in `src/content/docs/`; a Spanish translation of `introduction/history/index.mdx` lives in `src/content/docs/es/introduction/history/index.mdx`. The configured languages are listed in `src/i18n/locales.mjs`. Missing pages use Starlight's English fallback during local authoring, but block deployment.

Translation happens only on explicit request, for example: “Update the Spanish and French translations of Events and history using the latest English changes.” English edits, glossary changes, shared-component work, publication requests, and CI failures leave translations untouched. Report missing files instead of automatically translating or copying English into them. There is no bulk translation command, manifest, or freshness gate.

1. Read the English page, its relevant Git diff, `GLOSSARY.md`, and the existing translations. Limit the work to the requested pages and languages. Page plans are outside the default translation scope. A glossary edit is guidance for relevant passages, not an instruction to regenerate every language.
2. Edit each translated file directly. Preserve human corrections and translate only the changed passages, with the full page as context. Keep executable code, imports, component props, URL destinations, and stable anchors intact. Translate prose, headings, titles, descriptions, and sidebar labels naturally; grammar and emphasis need not follow English node order. Update only the affected keys in shared catalogs when the request includes diagram or interface text.
3. Review the diff, including code blocks and links, then run `npm run build`. Inspect the affected page and any changed shared component. Technical checks do not certify linguistic accuracy. A copy edit does not require another review of every language.

Documentation links are relative, such as `../history/#record-what-happened`. They preserve the selected language and deployment base, including on fallback pages. Headings with shared links keep an explicit anchor:

```mdx
## Recorded facts <a id="record-what-happened" />
```

The small `remark-stable-headings.mjs` plugin moves that marker's ID onto the heading. It does not read another document or compare heading counts. Preserve the ID when translating, renaming, or moving a section; new headings without a marker use Astro's normal generated IDs.

Homepage and diagram strings remain in `src/i18n/<locale>/site.json`; custom documentation controls live in `src/content/i18n/<lang>.json`. Missing site-catalog keys fall back to English. Reuse existing keys and the `@components/` imports. Keep literal IDE labels and code identifiers when describing the actual English screenshot or code.

Run `npm run check:translations` to check file coverage. Every English `.md` or `.mdx` documentation file, including page plans, requires the same relative file in every configured target-language folder. Each target language also requires `src/i18n/<code>/site.json` and `src/content/i18n/<lang>.json`, using the configured code and language tag respectively. The check reports missing paths and exits unsuccessfully; it never writes translations. Local development and `npm run build` remain available while files are missing.

Existing translations may lag behind English changes. Git diffs and editorial review track updates; the coverage check does not assess freshness, catalog keys, or linguistic accuracy. Removing a translated page blocks deployment until its replacement is explicitly requested and supplied. Existing translated page plans are retained, but are not automatically refreshed.

## Deployment

`.github/workflows/deploy.yml` checks translation file coverage, runs tests and the normal build on every push to `main` or manual dispatch, then publishes to the GitHub Pages project URL. Missing translated documentation or catalog files fail CI before deployment. CI does not translate content or require existing translations to match the latest English revision. Push only when the owner explicitly requests publication. The preview build uses `/neohaskell.org/` as its base path and omits the production `CNAME` and `robots.txt` files from the artifact. Local builds still use `https://neohaskell.org` without a base path. To reproduce the preview build locally, run `GITHUB_PAGES_PREVIEW=true npm run build`.

Framework references: [Starlight custom pages](https://starlight.astro.build/guides/pages/) and [Astro on GitHub Pages](https://docs.astro.build/en/guides/deploy/github/).

## Assets and design

The original NeoHaskell logo is preserved in `public/logo.svg`. Sansation, Manrope, and Pixelify Sans are self-hosted; their open-font licenses are included in `public/fonts/`. Font sources: Google Fonts and its `google/fonts` repository. The homepage uses native diagrams and an authentic IDE screenshot. The screenshot and its provenance remain documented in the model-evolution source evidence.

`GLOSSARY.md` defines canonical vocabulary. The approved scope and direction contract are in `.impeccable/surfaces/src-pages-index-astro.md`. Earlier studies under `.impeccable/diagrams/` are review artifacts, not website features.

See [the design artifact policy](.impeccable/README.md) for what is committed from `.impeccable/`. Shared design records and approved references are versioned; generated captures, caches, logs, and unused explorations stay local.

## Agent quality standard

`AGENTS.md` applies the project-local [Unslop skill](.agents/skills/unslop/SKILL.md) to every task. Unslop is by [Damir Sokolovsky (@heydamir)](https://github.com/heydamir/unslop), licensed under CC BY 4.0. Its [installation record](.agents/skills/unslop/UPSTREAM.md) pins the source revision and documents browser tooling. After `npm ci`, run `npx playwright install chromium` to enable its snapshots.
