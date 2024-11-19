{ pkgs ? import <nixpkgs> {}
, hsPkgs ? pkgs.haskellPackages
, returnShellEnv ? false
}:
hsPkgs.developPackage {
  name = "srt-tools";
  root = pkgs.nix-gitignore.gitignoreSourcePure
    [ "dist-newstyle"
      ".*#"
      ".git"
    ] ./.;
  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs:
    let
      prevTools = attrs.buildTools or [];
      requiredTools = with hsPkgs; [fourmolu hlint];
    in
      { buildTools = prevTools ++ requiredTools;
      }
  );
  inherit returnShellEnv;
}
