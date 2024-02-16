{ pkgs, ...}@args:
with pkgs;
stdenv.mkDerivation {
  pname = "perf";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildInputs = [ ghc haskell-language-server cabal-install perf ];
  executableHaskellDepends = with haskellPackages; [ base ];
  benchmarkHaskellDepends =  with haskellPackages;[ base containers ];
  mainProgram = "perf";
}
