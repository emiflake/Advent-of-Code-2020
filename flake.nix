{
  description = "Advent-of-Code-2020";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.03";
  inputs.haskell.url = "github:input-output-hk/haskell.nix";

  outputs = { self, nixpkgs, haskell }:
    let
      supportedSystems =
        [ "x86_64-linux" "i686-linux" "aarch64-linux" "x86_64-darwin" ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system: f system);
    in {

      packages = forAllSystems (system:
        let
          pkgs = haskell.legacyPackages."${system}";
          drv = (pkgs.haskell-nix.project {
            src = pkgs.haskell-nix.haskellLib.cleanGit {
              name = "Advent-of-Code-2020";
              src = ./.;
            };
          });
        in { aoc2020 = drv.aoc2020.components.exes.aoc2020-exe; });

      defaultPackage =
        forAllSystems (system: self.packages."${system}".aoc2020);

      defaultApp = forAllSystems (system: {
        type = "app";
        program = "${self.defaultPackage."${system}"}/bin/aoc2020-exe";
      });
    };
}
