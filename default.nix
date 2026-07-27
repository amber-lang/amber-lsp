{
  pkgs ? import <nixpkgs> { },
}:

pkgs.rustPlatform.buildRustPackage (finalAttrs: {
  pname = "amber-lsp";
  version = "0.3.0";

  src = pkgs.lib.cleanSource ./.;

  cargoLock.lockFile = ./Cargo.lock;

  __structuredAttrs = true;

  # Running the test suite in parallel exceeds the memory available in the sandbox.
  dontUseCargoParallelTests = true;

  nativeInstallCheckInputs = [ pkgs.versionCheckHook ];
  doInstallCheck = true;

  meta = {
    description = "Language server protocol implementation for Amber";
    homepage = "https://github.com/amber-lang/amber-lsp";
    license = pkgs.lib.licenses.gpl3Only;
    mainProgram = "amber-lsp";
    platforms = pkgs.lib.platforms.unix;
  };
})
