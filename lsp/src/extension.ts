/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import { ExtensionContext, workspace } from 'vscode';
import * as vscode from 'vscode';
import {
    CancellationToken,
    ConfigurationItem,
    ConfigurationParams,
    ConfigurationRequest,
    DidChangeConfigurationNotification,
    LanguageClient,
    LanguageClientOptions,
    LSPAny,
    ResponseError,
    ServerOptions,
} from 'vscode-languageclient/node';
import {PythonExtension} from '@vscode/python-extension';

let client: LanguageClient;

/// Get a setting at the path, or throw an error if it's not set.
function requireSetting<T>(path: string): T {
    const ret: T = vscode.workspace.getConfiguration().get(path);
    if (ret == undefined) {
        throw new Error(`Setting "${path}" was not configured`)
    }
    return ret;
}

 /**
   * This function adds the pythonPath to any section with configuration of 'python'.
   * Our language server expects the pythonPath from VSCode configurations but this setting is not stored in VSCode
   * configurations. The Python extension used to store pythonPath in this section but no longer does. Details:
   * https://github.com/microsoft/pyright/commit/863721687bc85a54880423791c79969778b19a3f
   *
   * Example:
   * - Pyrefly asks for a configurationItem for {scopeUri: '/home/project', section: 'python'}
   * - VSCode returns a configuration of {setting: 'value'} from settings.json
   * - This function will add pythonPath: '/usr/bin/python3' from the Python extension to the configuration
   * - {setting: 'value', pythonPath: '/usr/bin/python3'} is returned
   *
   * @param pythonExtension the python extension API
   * @param configurationItems the sections within the workspace
   * @param configuration the configuration returned by vscode in response to a workspace/configuration request (usually what's in settings.json)
   * corresponding to the sections described in configurationItems
   */
 async function overridePythonPath(
    pythonExtension: PythonExtension,
    configurationItems: ConfigurationItem[],
    configuration: (object | null)[],
  ): Promise<(object | null)[]> {
    const getPythonPathForConfigurationItem = async (index: number) => {
      if (configurationItems.length <= index || configurationItems[index].section !== 'python') {
        return undefined;
      }
      return await pythonExtension.environments.getActiveEnvironmentPath(vscode.Uri.file(configurationItems[index]?.scopeUri)).path;
    };
    const newResult = await Promise.all(configuration.map(async (item, index) => {
      const pythonPath = await getPythonPathForConfigurationItem(index);
      if (pythonPath === undefined) {
        return item;
      } else {
        return {...item, pythonPath};
      }
    }));
    return newResult;
  }

export async function activate(context: ExtensionContext) {
    const path: string = requireSetting("pyrefly.lspPath");
    const args: [string] = requireSetting("pyrefly.lspArguments");

    const bundledPyreflyPath = vscode.Uri.joinPath(
        context.extensionUri,
        'bin',
        'release',
        // process.platform returns win32 on any windows CPU architecture
        process.platform === 'win32' ? 'pyrefly.exe' : 'pyrefly'
    );

    let pythonExtension = await PythonExtension.api();

    // Otherwise to spawn the server
    let serverOptions: ServerOptions = { command: path === '' ? bundledPyreflyPath.fsPath : path, args: args };
    let rawInitialisationOptions = vscode.workspace.getConfiguration("pyrefly");

    // Options to control the language client
    let clientOptions: LanguageClientOptions = {
        initializationOptions: rawInitialisationOptions,
        // Register the server for Starlark documents
        documentSelector: [{ scheme: 'file', language: 'python' }],
        middleware: {
            workspace: {
                configuration: async (
                    params: ConfigurationParams,
                    token: CancellationToken,
                    next: ConfigurationRequest.HandlerSignature,
                  ): Promise<LSPAny[] | ResponseError<void>> => {
                    const result = await next(params, token);
                    if (result instanceof ResponseError) {
                      return result;
                    }
                    const newResult = await overridePythonPath(pythonExtension, params.items, result as (object | null)[]);
                    return newResult;
                  },
            }
        }
    };

    // Create the language client and start the client.
    client = new LanguageClient(
        'pyrefly',
        'Pyrefly language server',
        serverOptions,
        clientOptions
    );

    // Start the client. This will also launch the server
    client.start();

    context.subscriptions.push(
        pythonExtension.environments.onDidChangeActiveEnvironmentPath(() => {
          client.sendNotification(DidChangeConfigurationNotification.type, {settings: {}});
        })
    );

    context.subscriptions.push(
      workspace.onDidChangeConfiguration(event => {
        if (event.affectsConfiguration("python.pyrefly")) {
          client.sendNotification(DidChangeConfigurationNotification.type, {settings: {}});
        }
      }));

    context.subscriptions.push(
        vscode.commands.registerCommand('pyrefly.restartClient', async () => {
            await client.stop();
            client = new LanguageClient(
                'pyrefly',
                'Pyrefly language server',
                serverOptions,
                clientOptions
            );
            await client.start();
        }),
    );
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
