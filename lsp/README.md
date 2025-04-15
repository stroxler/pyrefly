# Pyrefly VS Code LSP extension

:warning: **Extension is very work in progress**, please use at your own risk!

A VSCode LSP extension that talks over stdin/stdout to a binary.

If using another binary, the settings to be aware of are `pyrefly.lspPath` (the
binary path) and `pyrefly.lspArguments` (the arguments to that binary). These
are available in the VSCode extension settings UI.

Based on a combination of:

- Tutorial at
  https://code.visualstudio.com/api/language-extensions/language-server-extension-guide
- Code for the tutorial at
  https://github.com/microsoft/vscode-extension-samples/tree/master/lsp-sample
