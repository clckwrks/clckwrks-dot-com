{ mkDerivation, base, clckwrks, clckwrks-plugin-media
, clckwrks-plugin-page, clckwrks-theme-clckwrks, containers
, happstack-server, hsp, hsx2hs, mtl, network, stdenv, text
, web-plugins
}:
mkDerivation {
  pname = "clckwrks-dot-com";
  version = "0.3.12.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base clckwrks clckwrks-plugin-media clckwrks-plugin-page
    clckwrks-theme-clckwrks containers happstack-server hsp mtl network
    text web-plugins
  ];
  executableToolDepends = [ hsx2hs ];
  homepage = "http://www.clckwrks.com/";
  description = "clckwrks.com";
  license = stdenv.lib.licenses.bsd3;
}
