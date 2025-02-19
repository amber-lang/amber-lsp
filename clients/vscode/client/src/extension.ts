/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import { workspace, ExtensionContext, window } from 'vscode';

import {
	Executable,
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	Trace,
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	const version = workspace.getConfiguration('amber-lsp').get<string>('version', 'auto');
	const command = process.env.SERVER_PATH || "amber-lsp";
	const run: Executable = {
	  command,
	  options: {
			env: {
				...process.env,
				RUST_LOG: "debug",
				RUST_BACKTRACE: 1
			},
	  },
	  args: ["--amber-version", version],
	};
	const serverOptions: ServerOptions = {
	  run,
	  debug: run,
	};
	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
	// Options to control the language client
	let clientOptions: LanguageClientOptions = {
	  // Register the server for plain text documents
	  documentSelector: [{ scheme: "file", language: "amber" }],
	  synchronize: {
		// Notify the server about file changes to '.clientrc files contained in the workspace
		fileEvents: workspace.createFileSystemWatcher("**/.clientrc"),
	  },
	};
	
	// Create the language client and start the client.
	client = new LanguageClient("amber-lsp", "Amber language server", serverOptions, clientOptions);

	client.setTrace(Trace.Verbose)
	// activateInlayHints(context);
	client.start();
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}