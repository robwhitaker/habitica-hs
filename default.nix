{ compiler ? null
, pkgs ? import <nixpkgs> {}
}:

let
  haskellPackages =
    if builtins.isNull compiler
      then pkgs.haskellPackages
      else pkgs.haskell.packages.${compiler};
  overriddenPackages = haskellPackages.override {
    overrides = self: super: {
      req = super.req_3_0_0;
    };
  };
in
  overriddenPackages.callCabal2nix "habitica-hs" ./. {}
