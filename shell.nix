{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, clckwrks, clckwrks-plugin-media
      , clckwrks-plugin-page, clckwrks-theme-clckwrks, containers
      , happstack-server, hsp, hsx2hs, mtl, network, stdenv, text
      , web-plugins, cabal-install
      }:
      mkDerivation {
        pname = "clckwrks-dot-com";
        version = "0.3.12";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base clckwrks clckwrks-plugin-media clckwrks-plugin-page
          clckwrks-theme-clckwrks containers happstack-server hsp mtl network
          text web-plugins cabal-install
        ];
        executableToolDepends = [ hsx2hs ];
        homepage = "http://www.clckwrks.com/";
        description = "clckwrks.com";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
