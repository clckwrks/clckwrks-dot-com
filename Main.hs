{-# LANGUAGE FlexibleContexts, OverloadedStrings, PackageImports, RankNTypes #-}
module Main where

import Clckwrks
import Clckwrks.GetOpts          (parseArgs, clckwrksOpts)
import Clckwrks.Server           (simpleClckwrks)
import Clckwrks.Plugin           (clckPlugin)
import Clckwrks.Bugs.Plugin      (bugsPlugin)
import Clckwrks.Media.Plugin     (mediaPlugin)
import Control.Applicative       ((<$>))
import Data.Text                 (Text)
import "clckwrks-theme-clckwrks" Theme (theme)
import Web.Plugins.Core          (initPlugin, setTheme)
import System.Environment        (getArgs)

------------------------------------------------------------------------------
-- ClckwrksConfig
------------------------------------------------------------------------------

clckwrksConfig :: ClckwrksConfig
clckwrksConfig = ClckwrksConfig
    { clckHostname        = "localhost"
    , clckPort            = 8000
    , clckHidePort        = False
    , clckJQueryPath      = ""
    , clckJQueryUIPath    = ""
    , clckJSTreePath      = ""
    , clckJSON2Path       = ""
    , clckTopDir          = Nothing
    , clckEnableAnalytics = False
    , clckInitHook        = initHook
    }

------------------------------------------------------------------------------
-- main
------------------------------------------------------------------------------

main :: IO ()
main =
    do args <- getArgs
       f    <- parseArgs (clckwrksOpts clckwrksConfig) args
       simpleClckwrks  (f clckwrksConfig)

initHook :: Text
         -> ClckState
         -> ClckwrksConfig
         -> IO (ClckState, ClckwrksConfig)
initHook baseURI clckState cc =
    do let p = plugins clckState
       initPlugin p baseURI clckPlugin
       initPlugin p baseURI bugsPlugin
       initPlugin p baseURI mediaPlugin
       setTheme p (Just theme)
       return (clckState, cc)
