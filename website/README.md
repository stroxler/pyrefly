# Website

The website is a combination of static content (built using
[Docusaurus 2](https://docusaurus.io/)) and a sandbox built with Rust/WASM.

## Development

### Prerequisites

Install dependencies:

```bash
yarn install-with-wasm-deps
```

This builds the wasm dependencies and installs the yarn dependencies.
If you are running into issues with compiling zstd on your mac when running `build.sh` or `start.sh`, you'll need to install
clang following the instructions here: https://github.com/briansmith/ring/issues/1824.

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
