const vscode = require("vscode");
const {
  LanguageClient,
  TransportKind,
} = require("vscode-languageclient/node");
const path = require("path");
const fs = require("fs");

let client;

function findLspBinary() {
  const config = vscode.workspace.getConfiguration("quone");
  const configured = config.get("lspPath");
  if (configured && fs.existsSync(configured)) {
    return configured;
  }

  const home = process.env.HOME || process.env.USERPROFILE || "";
  const cabalBin = path.join(home, ".cabal", "bin", "quone-lsp");
  if (fs.existsSync(cabalBin)) {
    return cabalBin;
  }

  return "quone-lsp";
}

async function activate(context) {
  const outputChannel = vscode.window.createOutputChannel("Quone Language Server");
  context.subscriptions.push(outputChannel);

  const command = findLspBinary();
  outputChannel.appendLine(`Quone LSP binary: ${command}`);

  const serverOptions = {
    run: { command, transport: TransportKind.stdio },
    debug: { command, transport: TransportKind.stdio },
  };

  const clientOptions = {
    documentSelector: [{ scheme: "file", language: "quone" }],
    outputChannel,
  };

  client = new LanguageClient(
    "quone-lsp",
    "Quone Language Server",
    serverOptions,
    clientOptions
  );

  try {
    await client.start();
    outputChannel.appendLine("Quone LSP started successfully");
  } catch (err) {
    outputChannel.appendLine(`Quone LSP failed: ${err}`);
    vscode.window.showErrorMessage(`Quone LSP failed to start: ${err.message}`);
  }
}

async function deactivate() {
  if (client) {
    await client.stop();
  }
}

module.exports = { activate, deactivate };
