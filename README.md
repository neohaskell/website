# NeoHaskell website

Astro and Starlight serve the NeoHaskell homepage and documentation. [AGENTS.md](AGENTS.md) is the entry point for authoring and review.

## Run and verify

Use Node.js 24 or 22.23.4+. Install dependencies with `npm ci`. Install Chromium for browser checks with `npx playwright install chromium`.

```sh
npm test
npm run build
npm run check:translations
```

The build runs the copy check and Astro diagnostics and writes the site to `dist/`. Translation coverage is checked separately by CI before publication.

## Local preview

Keep one repository server at <http://127.0.0.1:4323>. Restart after a batch of changes, including sidebar changes.

```sh
npm run preview -- stop
npm run dev -- stop
npm run dev -- --port 4323 --background --force
npm run dev -- status
```

Inspect listening processes and confirm the command and working directory before stopping another server. Run the maintained stop command from its owning checkout. For an unregistered server belonging to this repository, send SIGTERM to the verified PID. Preserve other projects' processes. Verify the sidebar and affected routes at port 4323.

The [Pages workflow](.github/workflows/deploy.yml) publishes `main` from [neohaskell/website](https://github.com/neohaskell/website) to [neohaskell.org](https://neohaskell.org/) on push or manual dispatch. Run `npm run build` to reproduce the production build locally.
