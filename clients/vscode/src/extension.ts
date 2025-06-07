/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import { workspace, ExtensionContext, window, commands, CompletionList } from 'vscode';
import {
	CloseAction,
	ErrorAction,
	Executable,
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	Trace,
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	const version = workspace.getConfiguration('amber-lsp').get<string>('version', 'auto');

	const command = process.env.SERVER_PATH || context.extensionPath + "/out/amber-lsp"

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
		errorHandler: {
			error: (error) => {
				return {
					action: ErrorAction.Continue,
					message: `Amber language server error: ${error.message}. Please report the issue to github.com/amber-lang/amber-lsp/issues`,
					handled: false,
				};
			},
			closed: async () => {
				const errorHandling = await window.showErrorMessage("Amber language server stopped.", "Restart", "Report Issue");

				console.log("Amber language server stopped. Error handling:", errorHandling);

				if (errorHandling === "Restart") {
					return { action: CloseAction.Restart, handled: true };
				}


				// FIXME: Uncomment when endpoint is available
				// const logsDir = join(tmpdir(), 'amber-lsp')
				// const lastLogFileName = await readdir(logsDir, { withFileTypes: true })
				// 	.then((files) =>
				// 			files
				// 				.filter((file) => file.isFile() && file.name.startsWith('amber-lsp.log'))
				// 				.sort()
				// 				.at(-1)
				// 				.name
				// 			);

				// if (!lastLogFileName) {
				// 	window.showErrorMessage("No log file found. Please report the issue to github.com/amber-lang/amber-lsp/issues");
				// 	return { action: CloseAction.DoNotRestart, handled: true };
				// }

				// const lastHundredLogLines = await readFile(join(logsDir, lastLogFileName)).then((data) => {
				// 	const logs = data.toString().split('\n');
				// 	return logs.slice(-100).join('\n');
				// });

				// const res = await axios.post('https://amber-lang.com/api/amber-lsp/crash-report', {
				// 	logs: lastHundredLogLines,
				// })
				
				window.showInformationMessage("Crash report sent successfully. Thank you for helping us improve Amber!");
				return { action: CloseAction.DoNotRestart, handled: true };
			}
		}
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
			if (isInsideImportString(lineText, position.character)) {
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

const isInsideImportString = (lineText: string, charPosition: number): boolean => {
	const textBeforeCursor = lineText.substring(0, charPosition);

	const match = textBeforeCursor.match(/\bfrom\b([^"]*)"([^"]*)$/)

	return !!match.length;
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
