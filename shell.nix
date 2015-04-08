with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, clckwrks
             , clckwrks-plugin-media, clckwrks-plugin-page
             , clckwrks-theme-clckwrks, containers, happstack-server, hsp
             , hsx2hs, mtl, network, stdenv, text, web-plugins, magic
             }:
             mkDerivation {
               pname = "clckwrks-dot-com";
               version = "0.3.10";
               src = ./.;
               isLibrary = false;
               isExecutable = true;
               buildDepends = [
                 base clckwrks clckwrks-plugin-media
                 clckwrks-plugin-page clckwrks-theme-clckwrks containers
                 happstack-server hsp mtl network text web-plugins
               ];
               buildTools = [ pandoc ];
               homepage = "http://www.clckwrks.com/";
               description = "clckwrks.com";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
