
## Local development
For local dev development of the halogen client, it's recommended to run a local dev server. This gets you automatic page reloading, and skips the slow bundling step.

Open a terminal shell in the spago repo, then run:
```console
# setup, only need to do this when spago or docs-search-index changes
spago build && spago docs 

# start the dev server
cd docs-search/client-halogen
npm install
npm run dev
```

