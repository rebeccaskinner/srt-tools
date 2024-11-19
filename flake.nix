{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = {self, nixpkgs, flake-utils}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hsPkgs = pkgs.haskellPackages;
        shellEnv = import ./default.nix { inherit pkgs hsPkgs;  returnShellEnv = true; };
        srtTool = import ./default.nix { inherit pkgs hsPkgs; returnShellEnv = false; };
        compressSubtitlesScript = pkgs.writeScriptBin "compressSubtitles" ''
          #!/usr/bin/env bash
          clear;
          echo ${self.packages.${system}.srtTool}/bin/srt-tools "''${1}" "''${2}";
          ${self.packages.${system}.srtTool}/bin/srt-tools "''${1}" "''${2}";
        '';
        compressSubtitles = {
          type = "app";
          program = "${self.packages.${system}.compressSubtitlesScript}/bin/compressSubtitles";
        };
      in {
        packages.srtTool = srtTool;
        packages.default = srtTool;
        packages.compressSubtitlesScript = compressSubtitlesScript;
        apps.compressSubtitles = compressSubtitles;
        defaultApp = self.apps.${system}.compressSubtitles;
        devShells.default = pkgs.mkShell {
          inputsFrom = [shellEnv];
          buildInputs = srtTool.buildInputs;
          shellHook = ''
            cabal update
          '';
        };
      });
  nixConfig.bash-prompt = "\\u@\\h:\\W (nix) λ ";
}
