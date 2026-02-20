// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';

// https://astro.build/config
export default defineConfig({
	integrations: [
		starlight({
			title: 'NeoHaskell',
			defaultLocale: 'root',
			locales: {
				root: { label: 'English', lang: 'en' },
				es: { label: 'Español' },
				ru: { label: 'Русский' },
				hy: { label: 'Հայերեն' },
				fr: { label: 'Français' },
				ja: { label: '日本語' },
			},
			social: [{ icon: 'github', label: 'GitHub', href: 'https://github.com/neohaskell/neohaskell' }],
			sidebar: [
				{
					label: 'Getting Started',
					items: [
						'getting-started/installation',
						'getting-started/first-events',
						'getting-started/reading-neohaskell',
						'getting-started/syntax-warmup',
						'getting-started/using-ai',
						'getting-started/cheat-sheet',
					],
				},
				{
					label: 'Tutorial: Build NeoBank',
					items: [
						{ slug: 'tutorial', label: 'Introduction' },
						'tutorial/01-first-transaction',
						'tutorial/02-account-rules',
						'tutorial/03-transaction-history',
						'tutorial/04-multiple-accounts',
						'tutorial/05-transfers',
						'tutorial/06-audit-everything',
						'tutorial/whats-next',
					],
				},
				{
					label: 'Core Concepts',
					collapsed: true,
					autogenerate: { directory: 'concepts' },
				},
				{
					label: 'Guides',
					collapsed: true,
					autogenerate: { directory: 'guides' },
				},
				{
					label: 'Coming From...',
					collapsed: true,
					autogenerate: { directory: 'coming-from' },
				},
				{
					label: 'Reference',
					collapsed: true,
					autogenerate: { directory: 'reference' },
				},
				{
					label: 'ADRs',
					collapsed: true,
					autogenerate: { directory: 'adrs' },
				},
			],
		}),
	],
});
