{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/23.11";
    utils.url = "github:numtide/flake-utils";
    confluent.url = "github:krisajenkins/confluent-niv/00df34f042ef750f3e4d4f32023f5a781c43ea99";
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, utils, confluent, easy-purescript-nix }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = [ ];
        };
        jdk = pkgs.openjdk11;
        easy-ps = import easy-purescript-nix { inherit pkgs; };
      in
      {
        devShell =
          with pkgs;
          with confluent.packages.${system};
          mkShell {
            buildInputs = [
              nodejs_21
              nodePackages.node-gyp
              nodePackages.prettier
              nodePackages.typescript-language-server
              yarn
              websocat
              easy-ps.purs
              easy-ps.spago
              easy-ps.purty
              easy-ps.purescript-language-server

              dhall
            ];

            shellHook = ''
              export JAVA_HOME=${jdk}
            '';
          };
      });
}
