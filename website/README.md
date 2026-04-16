# Quone website

The landing page for [quone-lang](https://github.com/armcn/quone-lang), built
with [Elm](https://elm-lang.org/).

## Layout

```
website/
├── index.html         HTML shell + CSS (loads main.js)
├── main.js            Compiled Elm output (committed so the site works
│                      locally without any build step)
├── elm.json           Elm package manifest
├── package.json       Wraps the Elm build for Netlify (and convenience)
├── src/
│   └── Main.elm       The site as an Elm program
├── dist/              Production build output (git-ignored)
└── README.md
```

## View it locally

The simplest option — `main.js` is already committed:

```sh
open index.html          # macOS (use xdg-open on Linux)
```

If your browser won't load `main.js` from `file://`, serve over HTTP:

```sh
npm run serve            # python3 -m http.server 8000
```

## Edit and rebuild

Install [Elm 0.19.1](https://guide.elm-lang.org/install/elm.html) globally,
or use the project's npm wrapper:

```sh
npm install              # installs the elm compiler via npm
npm run build:dev        # writes main.js next to index.html
```

## Deploying to Netlify

The repo includes a `netlify.toml` at the root and this folder's
`package.json` so Netlify can build the site straight from GitHub — no UI
configuration required.

### One-time setup

1. Push this repo to GitHub (if you haven't already).
2. Go to <https://app.netlify.com/start>, click **Import from Git**, and
   pick the `quone-lang` repo.
3. Netlify reads `netlify.toml` and fills in every field for you:
   - **Base directory**:   `website`
   - **Build command**:    `npm run build`
   - **Publish directory**: `website/dist`
   - **Node version**:     `20`
4. Click **Deploy site**. The first build installs `elm` from npm (~30 s)
   and produces an optimized bundle in `website/dist/`.

Every push to the default branch triggers a new deploy. Pull requests get
their own preview URLs automatically.

### Custom domain (`quone-lang.org`)

In Netlify: **Site settings → Domain management → Add custom domain**, enter
`quone-lang.org`, and Netlify will walk you through DNS. The two common
paths:

1. **Netlify DNS (easiest)**: change your domain registrar's nameservers
   to the four Netlify servers Netlify gives you. SSL, apex + `www`, and
   the Netlify subdomain redirect are all handled automatically.

2. **Keep your registrar's DNS**: create an `ALIAS`/`ANAME` record for the
   apex `quone-lang.org` pointing at `apex-loadbalancer.netlify.com`, and a
   `CNAME` for `www.quone-lang.org` pointing at your Netlify site name
   (`<your-site>.netlify.app`). Netlify provisions a Let's Encrypt cert
   after DNS propagates.

Primary domain should be `quone-lang.org`; Netlify will redirect
`www.quone-lang.org` and the default `*.netlify.app` URL to it.

### Manual deploys

If you prefer not to connect GitHub, you can ship a local build with the
[Netlify CLI](https://docs.netlify.com/cli/get-started/):

```sh
cd website
npm install
npm run build
npx netlify deploy --dir=dist --prod
```

## Production build details

`npm run build` runs:

```sh
rm -rf dist
mkdir -p dist
elm make src/Main.elm --output=dist/main.js --optimize
cp index.html dist/index.html
```

That's the entire deploy payload — two files. No framework, no router, no
client-side fetching.

The `netlify.toml` also sets a handful of response headers:

- `index.html`: `Cache-Control: max-age=0, must-revalidate` (always fresh)
- `main.js`:   `Cache-Control: max-age=3600` (rebuilt on every deploy)
- Everything: `X-Frame-Options`, `X-Content-Type-Options`,
  `Referrer-Policy`, and a `Permissions-Policy` that opts out of Google's
  Federated Learning of Cohorts.

## Design notes

- **Type**: Space Grotesk (display) + Inter (body) + JetBrains Mono (code).
- **Colour**: cream background, ink-near-black text, violet primary, amber
  accent.
- **Interactive code explorer**: pick an example, click any group on the
  left to see exactly what the Quone compiler emits on the right, with a
  short explanation of the transformation.
- **Placeholder links**: anything that doesn't exist yet (playground,
  Discord, tutorial…) renders with a muted `soon` badge so the nav stays
  honest.

## Adding a new example to the explorer

The interactive panels are driven by `dplyrExample` and `rmseExample`
values in `src/Main.elm`. Each is an `Example` containing a list of
`Chunk`s:

```elm
type alias Chunk =
    { id : Int
    , tag : String                      -- badge text in the explanation
    , quone : List (List Token)         -- Quone lines for this chunk
    , r : List (List Token)             -- R lines this chunk compiles to
    , explain : String                  -- markdown-ish prose (backticks
                                        -- for `code`)
    }
```

To add a new example:

1. Define a new `ExampleId` variant and add a case for it in `exampleData`.
2. Build the `Example` record with your chunks.
3. Add an `exampleTab` entry to the tab row in `viewExplorer`.

Token constructors for the mini highlighter:

```elm
Plain String    -- untouched text
Comment String  -- grey + italic
Keyword String  -- violet
Ty String       -- teal (types)
Str String      -- amber (string literals)
Num String      -- orange (number literals)
Op String       -- pink (operators)
Fn String       -- blue (function names)
```
