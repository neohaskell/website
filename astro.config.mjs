import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
import { unified } from '@astrojs/markdown-remark';
import { readFileSync, existsSync } from 'node:fs';
import { locales, targetLocales } from './src/i18n/locales.mjs';
import { remarkStableHeadings } from './scripts/remark-stable-headings.mjs';
import english from './src/i18n/en/site.json' with { type: 'json' };

const catalogs = Object.fromEntries(targetLocales.map(({ code }) => {
  const path = new URL(`./src/i18n/${code}/site.json`, import.meta.url);
  return [code, existsSync(path) ? JSON.parse(readFileSync(path, 'utf8')) : english];
}));
const group = (key, items) => ({
  label: english[key],
  translations: Object.fromEntries(targetLocales.map(({ code, lang }) => [lang, catalogs[code][key] ?? english[key]])),
  collapsed: false,
  items: items.map((item) => typeof item === 'string' ? { slug: item } : item),
});

export default defineConfig({
  site: process.env.GITHUB_PAGES_PREVIEW === 'true' ? 'https://nickseagull.dev' : 'https://neohaskell.org',
  base: process.env.GITHUB_PAGES_PREVIEW === 'true' ? '/neohaskell.org' : '/',
  output: 'static',
  trailingSlash: 'always',
  devToolbar: { enabled: false },
  markdown: { processor: unified({ remarkPlugins: [remarkStableHeadings] }) },
  integrations: [starlight({
    title: 'NeoHaskell',
    description: 'An AI-first programming language for applications with clear behavior and recorded history.',
    logo: { src: './public/logo.svg', replacesTitle: false },
    favicon: '/logo.svg',
    social: [{ icon: 'github', label: 'GitHub', href: 'https://github.com/neohaskell/NeoHaskell' }],
    customCss: ['./src/styles/docs.css'],
    defaultLocale: 'root',
    locales: Object.fromEntries(locales.map(({ code, label, lang }) => [code === 'en' ? 'root' : code, { label, lang }])),
    components: { Head: './src/components/DocsHead.astro', LanguageSelect: './src/components/DocsLanguagePicker.astro' },
    sidebar: [
      group('docs.start', ['introduction', 'introduction/history', 'learn', 'introduction/event-modeling']),
      group('docs.first-project', ['getting-started/installation', 'getting-started/create-project', 'getting-started/run', 'getting-started/first-response', 'getting-started/project-files', 'getting-started', 'getting-started/troubleshooting']),
      group('docs.tutorial', ['tutorial', 'tutorial/first-feature', 'tutorial/decide-orders', 'tutorial/stock-query', 'tutorial/http-orders', 'tutorial/test-behavior', 'tutorial/change-safely']),
      group('docs.guides', [
        group('docs.guides.interfaces', ['guides/ai-workflow', 'guides/transports', 'guides/transports/mcp', 'guides/transports/cli']),
        group('docs.guides.storage', ['guides/storage', 'guides/storage/query-rebuilds', 'guides/files', 'guides/files/generated-documents', 'guides/storage/erase-protected-values']),
        group('docs.guides.access', ['guides/authentication', 'guides/authorization', 'guides/authorization/query-access']),
        group('docs.guides.integrations', ['guides/integrations', 'guides/integrations/inbound-workers', 'guides/integrations/oauth-connections']),
        group('docs.guides.operations', ['guides/deployment', 'guides/deployment/readiness-and-failures']),
      ]),
      group('docs.concepts', ['concepts/event-modeling', 'concepts/command-outcomes', 'concepts/entities-and-streams', 'concepts/history-and-state', 'concepts/query-projections', 'concepts/pure-decisions', 'concepts/event-driven-workflows', 'concepts/identity-and-permissions', 'guides/integrations/create-reusable', 'guides/integrations/resources']),
      group('docs.reference', [
        group('docs.reference.language', ['reference/language', 'reference/language/types']),
        group('docs.reference.core', ['reference/core', 'reference/core/containers', 'reference/core/results', 'reference/core/tasks', 'reference/core/json']),
        group('docs.reference.framework', ['reference/framework', 'reference/framework/commands', 'reference/framework/entities', 'reference/framework/queries', 'reference/framework/transports', 'reference/framework/integrations']),
        group('docs.reference.tooling', ['reference/cli', 'reference/cli/model-tools', 'reference/cli/agent-commands', 'reference/configuration', 'reference/configuration/project']),
        group('docs.reference.help', ['reference/errors', 'reference/errors/runtime', 'resources', 'docs']),
      ]),
    ],
    editLink: undefined,
  })],
});
