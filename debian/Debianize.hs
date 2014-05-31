{-# LANGUAGE OverloadedStrings #-}
import Data.List as List (concat, map)
import Data.Set (singleton)
import Data.Text as T (lines, pack, Text, unlines)
import Debian.Changes (ChangeLog)
import Debian.Debianize (evalDebT, newAtoms, debianization, writeDebianization, compat, control, DebT, doBackups, doWebsite, execMap, inputChangeLog, installTo, missingDependencies, revision, rulesFragments, rulesHead, seereasonDefaultAtoms, sourceFormat, tightDependencyFixup, homepage, standardsVersion, (+++=), (~=), (+=), (%=), InstallFile(InstallFile, destDir, destName, execName, sourceDir), Server(..), Site(..), Top(Top))
import Debian.Debianize.Goodies (makeRulesHead)
import Debian.Policy (databaseDirectory, SourceFormat(Native3), StandardsVersion(StandardsVersion))
import Debian.Pretty (Pretty(pretty))
import Debian.Relation (BinPkgName(BinPkgName), Relation(Rel))

top :: Top
top = Top "."

main :: IO ()
main = newAtoms >>= evalDebT (debianization top seereasonDefaultAtoms customize >> writeDebianization top)

customize :: DebT IO ()
customize =
    do inputChangeLog top
       execMap +++= ("hsx2hs", [[Rel (BinPkgName "hsx2hs") Nothing Nothing]])
       homepage ~= Just "http://www.clckwrks.com/"
       rulesFragments += pack (Prelude.unlines ["build/clckwrks-dot-com-production::", "\techo CLCKWRKS=`ghc-pkg field clckwrks version | sed 's/version: //'` > debian/default"])
       installTo +++= (BinPkgName "clckwrks-dot-com-production", singleton ("debian/default", "/etc/default/clckwrks-dot-com-production"))
       standardsVersion ~= Just (StandardsVersion 3 9 4 Nothing)
       sourceFormat ~= Just Native3
       missingDependencies += (BinPkgName "libghc-clckwrks-theme-clckwrks-doc")
       revision ~= Just ""
       doWebsite (BinPkgName "clckwrks-dot-com-production") (theSite (BinPkgName "clckwrks-dot-com-production"))
       doBackups (BinPkgName "clckwrks-dot-com-backups") "clckwrks-dot-com-backups"
       fixRules
       tight
       compat ~= Just 7

serverNames = List.map BinPkgName ["clckwrks-dot-com-production" {- , "clckwrks-dot-com-staging", "clckwrks-dot-com-development" -}]

-- Insert a line just above the debhelper.mk include
fixRules =
    do hd <- makeRulesHead
       rulesHead ~= Just (f hd)
    where
      f t = T.unlines $ List.concat $
            List.map (\ line -> if line == "include /usr/share/cdbs/1/rules/debhelper.mk"
                                then ["DEB_SETUP_GHC_CONFIGURE_ARGS = -fbackups", "", line] :: [T.Text]
                                else [line] :: [T.Text]) (T.lines t)

tight = mapM_ (tightDependencyFixup
                         -- For each pair (A, B) make sure that this package requires the
                         -- same exact version of package B as the version of A currently
                         -- installed during the build.
                         [(BinPkgName "libghc-clckwrks-theme-clckwrks-dev", BinPkgName "haskell-clckwrks-theme-clckwrks-utils"),
                          (BinPkgName "libghc-clckwrks-plugin-media-dev", BinPkgName "haskell-clckwrks-plugin-media-utils"),
                          (BinPkgName "libghc-clckwrks-plugin-bugs-dev", BinPkgName "haskell-clckwrks-plugin-bugs-utils"),
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
                                 , destName   = show (pretty deb)
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
