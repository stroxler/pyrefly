# Contributing to Pyrefly Website

Please see CONTRIBUTING.md in the root of the repository for more details on contributing.
This file shows details on how to develop the Website locally, as well as deployment instructions.

## Development

### Prerequisites

Install dependencies:

```bash
yarn install-with-wasm-deps
```

This builds the wasm dependencies and installs the yarn dependencies.
If you are running into issues with compiling zstd on your mac when running `build.sh` or `start.sh`, see `pyrefly_wasm/README.md` for more details.

### Running the Website

**Running the External Site**

```bash
yarn start-with-wasm
```

If you don't need to rebuild the wasm, you can use the following command instead:

```bash
yarn start
```

To see the website, go to the following URI in your browser:

```
localhost:3000
```

Most changes are reflected live without having to restart the server.

**Running the Internal Docs Site (for Meta Employees)**

If you want to run the static docs website, run the following command:

```bash
yarn start-internal-docs
```

Similar to above, if you don't need to rebuild the wasm, you can use the following command instead:

```bash
yarn start-internal-docs-with-wasm
```

## Writing Docs

The docs are written in markdown (with JSX) and are located in the `docs` directory. Note that you should not explicitly add H1 headers to the docs, because docusaurus does not support this by default (https://github.com/facebook/docusaurus/issues/5036).

## Deployment

The website is currently deployed on a daily basis (14 UTC) to pyrefly.org. You can also choose to manually run this if needed.
It is triggered by the [deploy website workflow](https://github.com/facebook/pyrefly/actions/workflows/deploy_website.yml), which first [builds and test the website](https://github.com/facebook/pyrefly/actions/workflows/build_and_test_website.yml).

If there's a major issue on the website and you want to quickly roll it back to a stable state, you can use the [rollback workflow](https://github.com/facebook/pyrefly/actions/workflows/rollback_website.yml).

For details on how to manually trigger a github workflow, see [this link](https://docs.github.com/en/actions/managing-workflow-runs-and-deployments/managing-workflow-runs/manually-running-a-workflow).

**Internal Docs Site (for Meta Employees)**
The internal docs page is deployed whenever diffs that touches the site lands. A manual deploy can also be triggered using the static docs hub: https://www.internalfb.com/staticdocs/hub.
