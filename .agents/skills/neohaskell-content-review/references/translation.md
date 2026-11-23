# Translate a requested page

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
