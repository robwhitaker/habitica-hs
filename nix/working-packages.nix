let
  sources = import ./sources.nix;
  config = import ./config.nix;
  overlays = [ (import ./overlays.nix) ];
in
import sources.nixpkgs { inherit config overlays; }
