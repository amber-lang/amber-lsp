/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import { workspace, ExtensionContext, window, languages, TextDocument, Position, commands, CompletionList } from 'vscode';

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

	let lastEditWasCompletion = false;

	const pathEdits = workspace.onDidChangeTextDocument(async (event) => {
		const document = event.document;
		const changes = event.contentChanges[0];

		// If the edit replaces a range (not just inserts), assume it was a completion
		if (!changes || !changes.range.isEmpty || changes.text.length > 1) {
			return
		}
	
		if (changes && /[a-zA-Z]|\.|\//.test(changes.text)) {
			const position = changes.range.start;
			const lineText = document.lineAt(position.line).text;

			// Check if the cursor is inside a string
			if (isInsideString(lineText, position.character)) {
				if (lastEditWasCompletion) {
					// Reset the flag and skip triggering a new request
					lastEditWasCompletion = false;
					return;
				}

				// Trigger the completion request
				const result = await commands.executeCommand('vscode.executeCompletionItemProvider', document.uri, position)

				if ((Array.isArray(result) && result.length) || (result as CompletionList).items.length) {
					commands.executeCommand('editor.action.triggerSuggest');
				}
			}
		}
	});

	context.subscriptions.push(pathEdits);

	client.setTrace(Trace.Verbose)
	client.start();
}

const isInsideString = (lineText: string, charPosition: number): boolean => {
	const textBeforeCursor = lineText.substring(0, charPosition);
	const quoteCount = (textBeforeCursor.match(/"/g) || []).length;
	return quoteCount % 2 !== 0;
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
