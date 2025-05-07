import { defineConfig } from '@vscode/test-cli';

export default defineConfig({
	files: 'dist/test/**/*.test.js',
    workspaceFolder: "../pyrefly/lib/test/lsp/test_files"
});
