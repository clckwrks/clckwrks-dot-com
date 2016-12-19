{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Data.List as List (concat, map)
import Data.Map as Map (insertWith)
import Data.Monoid (mappend)
import Data.Set as Set (singleton, insert)
import Data.Text as T (lines, pack, Text, unlines)
import Debian.Changes (ChangeLog)
import Debian.Debianize
import Debian.AutoBuilder.Details.Versions (seereasonDefaults)
import Debian.Policy (databaseDirectory, SourceFormat(Native3), StandardsVersion(StandardsVersion))
import Debian.Pretty (ppShow)
import Debian.Relation (BinPkgName(BinPkgName), Relation(Rel))
import Distribution.Compiler (CompilerFlavor(GHC))

main :: IO ()
main = performDebianization (seereasonDefaults >> customize)

customize :: CabalT IO ()
customize =
    do liftCabal inputChangeLog
       (debInfo . execMap) %= Map.insertWith mappend "hsx2hs" [[Rel (BinPkgName "hsx2hs") Nothing Nothing]]
       (debInfo . control . homepage) .= Just "http://www.clckwrks.com/"
       (debInfo . rulesFragments) %= Set.insert (pack (Prelude.unlines ["build/clckwrks-dot-com-production::", "\techo CLCKWRKS=`ghc-pkg field clckwrks version | sed 's/version: //'` > debian/default"]))
       (debInfo . atomSet) %= (Set.insert $ InstallTo (BinPkgName "clckwrks-dot-com-production") "debian/default" "/etc/default/clckwrks-dot-com-production")
       (debInfo . control . standardsVersion) .= Just (StandardsVersion 3 9 4 Nothing)
       (debInfo . sourceFormat) .= Native3
       (debInfo . missingDependencies) %= Set.insert (BinPkgName "libghc-clckwrks-theme-clckwrks-doc")
       (debInfo . revision) .= Just ""
       doWebsite (BinPkgName "clckwrks-dot-com-production") (theSite (BinPkgName "clckwrks-dot-com-production"))
       doBackups (BinPkgName "clckwrks-dot-com-backups") "clckwrks-dot-com-backups"
       liftCabal fixRules
       liftCabal tight
       (debInfo . compat) .= Just 7

serverNames = List.map BinPkgName ["clckwrks-dot-com-production" {- , "clckwrks-dot-com-staging", "clckwrks-dot-com-development" -}]

-- Insert a line just above the debhelper.mk include
fixRules = rulesSettings %= (++ ["DEB_SETUP_GHC_CONFIGURE_ARGS = -fbackups"])

tight = mapM_ (tightDependencyFixup
                         -- For each pair (A, B) make sure that this package requires the
                         -- same exact version of package B as the version of A currently
                         -- installed during the build.
                         [(BinPkgName "libghc-clckwrks-theme-clckwrks-dev", BinPkgName "haskell-clckwrks-theme-clckwrks-utils"),
                          (BinPkgName "libghc-clckwrks-plugin-media-dev", BinPkgName "haskell-clckwrks-plugin-media-utils"),
                          -- (BinPkgName "libghc-clckwrks-plugin-page-dev", BinPkgName "haskell-clckwrks-plugin-page-utils"),
                          (BinPkgName "libghc-clckwrks-dev", BinPkgName "haskell-clckwrks-utils")]) serverNames

theSite :: BinPkgName -> Site
theSite deb =
    Site { domain = hostname'
         , serverAdmin = "logic@seereason.com"
         , server = theServer deb }

theServer :: BinPkgName -> Server
theServer deb =
          Server { hostname =
                       case deb of
                         BinPkgName "clckwrks-dot-com-production" -> hostname'
                         _ -> hostname'
                 , port = portNum deb
                 , headerMessage = "Generated by clckwrks-dot-com/Setup.hs"
                 , retry = "60"
                 , serverFlags =
                     [ "--http-port", show (portNum deb)
                     , "--hide-port"
                     , "--hostname", hostname'
                     , "--top", databaseDirectory deb
                     , "--enable-analytics"
                     , "--jquery-path", "/usr/share/javascript/jquery/"
                     , "--jqueryui-path", "/usr/share/javascript/jquery-ui/"
                     , "--jstree-path", "/usr/share/clckwrks/jstree"
                     , "--json2-path", "/usr/share/clckwrks/json2"
                     ]
                 , installFile =
                     InstallFile { execName   = "clckwrks-dot-com-server"
                                 , destName   = ppShow deb
                                 , sourceDir  = Nothing
                                 , destDir    = Nothing }
                 }

hostname' = "clckwrks.com"

portNum :: BinPkgName -> Int
portNum (BinPkgName deb) =
          case deb of
            "clckwrks-dot-com-production"  -> 9029
            "clckwrks-dot-com-staging"     -> 9038
            "clckwrks-dot-com-development" -> 9039
            _ -> error $ "Unexpected package name: " ++ deb
