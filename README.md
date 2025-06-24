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
* Python

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

### Testing

Tests are divided based on Amber compiler version (eg. alpha034 for "0.3.4-alpha").

They are mainly based on snapshots using [cargo insta](https://insta.rs/docs/cli/).

Code coverage is generated with (cargo tarpaulin)[https://crates.io/crates/cargo-tarpaulin].
You can test code coverage via `run_coverage.ab` script, which will display results in a form of an HTML page.

```bash
./run_coverage.ab [<Json, Stdout, Xml, Html, Lcov>...]
```

We require 80% code coverage.

[![codecov](https://codecov.io/gh/amber-lang/amber-lsp/graph/badge.svg?token=DWX5GL9U8O)](https://codecov.io/gh/amber-lang/amber-lsp)

### Running lsp

Server for now communicates only via stdio.

If you're using VsCode, you can test the extension by running pre defined script
"Run Extension (Release Build)" in tests tab.

If you're using Zed, you need to clone [Zed extension repo](https://github.com/amber-lang/amber-zed) and change "cached_binary_path"
to local Amber LSP server binary.
