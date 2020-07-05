let
  pkgs = import (fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/a5e211dd7f95167ab42066e82bfaa5a65971e67c.tar.gz;
    sha256 = "0f9279l5if8jc8gnyyb722sqssa5h0dszm3fhmj0ndg9d071wl8g";
  }) {};
  drv = import ./. { inherit pkgs; };
in
  pkgs.haskellPackages.shellFor {
    packages = p: [drv];
    buildInputs = with pkgs.haskellPackages; [
      cabal-install
      ghcid
      hlint
      stylish-haskell
    ];
  }
