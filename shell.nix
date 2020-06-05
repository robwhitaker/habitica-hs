let
  pkgs = import (fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz;
    sha256 = "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq";
  }) {};
  drv = import ./. { inherit pkgs; };
in
  pkgs.haskellPackages.shellFor {
    packages = p: [drv];
    buildInputs = with pkgs.haskellPackages; [
      cabal-install
      ghcid
      hlint
    ];
  }
