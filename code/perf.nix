{ pkgs, ...}@args:
with pkgs;
stdenv.mkDerivation {
  pname = "perf";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
  buildInputs = [ ghc haskell-language-server cabal-install linuxPackages.perf ];
  executableHaskellDepends = with haskellPackages; [ base containers array];
  benchmarkHaskellDepends =  with haskellPackages; [ base containers array];
  mainProgram = "perf";
}
