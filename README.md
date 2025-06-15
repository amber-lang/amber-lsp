# Amber LSP

This repository implements [LSP server](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/) for [Amber Language](https://amber-lang.com/)

## Using LSP

There are few ways you can use this LSP in your favorite IDE:
* VsCode extension
* Zed extension
* Download LSP server and connect manually

## Developing LSP

In order to develop Amber LSP you will need few things:

* Rust
* Node.JS
* python

In order to use pre-commit hook you have to run following commands:
```bash
pip install pre-commit
```

In the project directory:
```bash
pre-commit install
```

This hook will check code formatting and any improvements you can do to your code.

It will also see if you used "FIXME" keyword that is a helpful way for you, to make sure you do not commit changes that need fixing.
