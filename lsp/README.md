# Pyrefly VS Code Extension

The Pyrefly extension uses Pyrefly to provide language server features for
Python in VS Code. Please see [pyrefly.org](https://pyrefly.org/) for more
information.

## Configuration

The following configuration options are IDE-specific and exposed as VSCode
settings:

- `python.pyrefly.disableLanguageServices` [boolean: false]: by default, Pyrefly
  will provide both type errors and other language features like go-to
  definition, intellisense, hover, etc. Enable this option to keep type errors
  from Pyrefly unchanged but use VSCode's Python extension for everything else.
- `python.pyrefly.disableTypeErrors` [boolean: false]: by default, Pyrefly will
  provide type errors in your project. Enable this setting to disable type error
  squiggles appearing in the editor.

For other options, see
[configuration](https://pyrefly.org/en/docs/configuration/).
