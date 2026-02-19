// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';

// https://astro.build/config
export default defineConfig({
	integrations: [
		starlight({
			title: 'NeoHaskell',
			defaultLocale: 'en',
			locales: {
				en: { label: 'English' },
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
					autogenerate: { directory: 'getting-started' },
				},
				{
					label: 'Core Concepts',
					autogenerate: { directory: 'concepts' },
				},
				{
					label: 'Guides',
					autogenerate: { directory: 'guides' },
				},
				{
					label: 'API Reference',
					autogenerate: { directory: 'reference' },
				},
				{
					label: 'ADRs',
					autogenerate: { directory: 'adrs' },
				},
			],
		}),
	],
});
