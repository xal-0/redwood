{
  description =
    "Redwood: a simple imperative programming language built to make small games.";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in rec {
        packages.redwood = pkgs.haskellPackages.callCabal2nix "redwood" ./. { };
        defaultPackage = packages.redwood;
      });
}
