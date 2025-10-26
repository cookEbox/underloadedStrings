{
  description = "Underloaded Strings is a useful set of helper functions for string conversion";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils"; 
  };
  outputs = { nixpkgs, flake-utils, ... }: 
    flake-utils.lib.eachDefaultSystem (system: 
    let 
      pkgs = nixpkgs.legacyPackages.${system};
      hp = pkgs.haskellPackages;
      underloadedStringsPkg = hp.callCabal2nix "underloadedStrings" ./. { };
    in {
      packages = { 
        default = underloadedStringsPkg;
        underloadedStrings = underloadedStringsPkg;
      };

      devShells.default = hp.shellFor {
        packages = p: [ underloadedStringsPkg ]; 
        withHoogle = true;
        buildInputs = [
          pkgs.cabal-install
          pkgs.hlint
          pkgs.ormolu
          pkgs.ghcid
          hp.haskell-language-server 
        ];
      };
      apps.hoogle = let
        serve = pkgs.writeShellApplication {
          name = "underloadedStrings-hoogle";
          runtimeInputs = [
            pkgs.cabal-install
            hp.hoogle
            pkgs.findutils
            pkgs.coreutils
          ];
          text = ''
            set -euo pipefail
            cabal clean

            echo "[underloadedStrings-hoogle] generating HTML + Hoogle docs via cabal…"
            cabal haddock --haddock-html --haddock-hoogle --haddock-quickjump 

            echo "[underloadedStrings-hoogle] locating hoogle txt…"
            # Prefer the package's own txt (e.g. .../doc/html/underloadedStrings/underloadedStrings.txt)
            TXT=$(find dist-newstyle -type f -path "*/doc/html/*/underloadedStrings.txt" | head -n1 || true)

            # Fallback: any *.txt under doc/html (handles component naming differences)
            if [ -z "''${TXT:-}" ]; then
              TXT=$(find dist-newstyle -type f -path "*/doc/html/*/*.txt" | head -n1 || true)
            fi

            if [ -z "''${TXT:-}" ]; then
              echo "ERROR: couldn't find a Hoogle .txt under dist-newstyle/**/doc/html/"
              exit 1
            fi

            DIR=$(dirname "''${TXT}")
            mkdir -p .hoogle
            DB=".hoogle/underloadedStrings.hoo"

            echo "[underloadedStrings-hoogle] building DB from: ''${DIR}"
            hoogle generate --database "''${DB}" --local="''${DIR}"

            PORT="''${HOOGLE_PORT:-8080}"
            echo "[underloadedStrings-hoogle] serving http://localhost:$PORT  (Ctrl+C to stop)"
            # --local is boolean "offline" mode; not a path.
            hoogle server --database "''${DB}" --local --port "$PORT"
          '';
        };
      in {
        type = "app";
        program = "${serve}/bin/underloadedStrings-hoogle";
      };

      apps.hoogle-merge = let
        gen = pkgs.writeShellApplication {
          name = "underloadedStrings-hoogle-merge";
          runtimeInputs = [
            pkgs.cabal-install
            hp.hoogle
            pkgs.findutils
            pkgs.coreutils
          ];
          text = ''
            set -euo pipefail
            cabal clean

            cabal haddock --haddock-html --haddock-hoogle --haddock-quickjump

            TXT=$(find dist-newstyle -type f -path "*/doc/html/*/underloadedStrings.txt" | head -n1 || true)
            if [ -z "''${TXT:-}" ]; then
              TXT=$(find dist-newstyle -type f -path "*/doc/html/*/*.txt" | head -n1 || true)
            fi
            if [ -z "''${TXT:-}" ]; then
              echo "ERROR: couldn't find a Hoogle .txt under dist-newstyle/**/doc/html/"
              exit 1
            fi

            DIR=$(dirname "''${TXT}")
            mkdir -p .hoogle
            DB=".hoogle/underloadedStrings.hoo"

            echo "[underloadedStrings-hoogle-merge] generating local DB at $DB from $DIR"
            hoogle generate --database "''${DB}" --local="''${DIR}"
            echo "Run: hoogle server --database $DB --local --port 8080"
          '';
        };
      in {
        type = "app";
        program = "${gen}/bin/underloadedStrings-hoogle-merge";
      };
    }
  );

}
