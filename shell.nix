{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, curl, HTTP, http-conduit
      , hxt, hxt-curl, hxt-http, pretty-simple, resourcet, stdenv, text
      , unordered-containers
      }:
      mkDerivation {
        pname = "witsapi";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base bytestring curl HTTP http-conduit hxt hxt-curl hxt-http
          pretty-simple resourcet text unordered-containers
        ];
        homepage = "https://github.com/sahabi/witsapi.git";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
