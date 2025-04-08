# Website

The website is a combination of static content (built using
[Docusaurus 2](https://docusaurus.io/)) and a sandbox built with Rust/WASM.

## Development

### Prerequisites

Install dependencies:

```bash
scripts/install.sh
```

If you are running into issues with compiling zstd on your mac when running `build.sh` or `start.sh`, you'll need to install
clang following the instructions here: https://github.com/briansmith/ring/issues/1824.

### Running the Website

Compile it:

```bash
./scripts/start.sh
```

This would automatically install all rust dependencies and start the server.

To see the website, go to the following URI in your browser:

```
localhost:3000
```

Most changes are reflected live without having to restart the server.

### For Meta Employees

If you want to run the static docs website, run the following command:

```
./static_docs_build_script.sh
```
