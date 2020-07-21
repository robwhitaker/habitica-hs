{ pkgs ? import ./nix/working-packages.nix }:
let
  sources = import ./nix/sources.nix;
  nivPackages = import sources.niv {};

  habitica-hs = pkgs.haskellPackages.callCabal2nix "habitica-hs" ./. {};
in
{
  drv = habitica-hs;
  shell = pkgs.haskellPackages.shellFor {
    packages = _: [habitica-hs];
    buildInputs = with pkgs.haskellPackages; [
      cabal-install
      ghcid
      hlint
      stylish-haskell

      nivPackages.niv
    ];
    withHoogle = true;
  };
}
