import * as assert from 'assert';
import * as vscode from 'vscode';

suite('Extension Test Suite', () => {
	const extension: vscode.Extension<unknown> | undefined = vscode.extensions.getExtension('meta.pyrefly');

	test('Test activation', async function () {
		this.timeout(5000);
		await extension?.activate();
		assert.ok(true);
	});
});
