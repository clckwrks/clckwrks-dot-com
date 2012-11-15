{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Main where

import Clckwrks.URL
import Clckwrks.Admin.Template
import Clckwrks.Monad
import Clckwrks.Server
import Clckwrks.Plugin
import Control.Applicative ((<$>))
import Control.Monad.Trans
import Theme
import qualified Data.Map as Map
import qualified Data.Text as Text
import Happstack.Server
import Web.Plugin.Core

clckwrksConfig :: ClckwrksConfig ClckURL
clckwrksConfig = ClckwrksConfig
    { clckHostname        = "localhost"
    , clckPort            = 8000
    , clckURL             = id
    , clckJQueryPath      = ""
    , clckJQueryUIPath    = ""
    , clckJSTreePath      = ""
    , clckJSON2Path       = ""
    , clckThemeDir        = "../clckwrks-theme-clckwrks"
    , clckPluginDir       = Map.empty
    , clckStaticDir       = "../clckwrks/static"
    , clckTopDir          = Nothing
    , clckEnableAnalytics = False
    , clckInitHook        = initHook
    }

main :: IO ()
main = simpleClckwrks clckwrksConfig

initHook :: ClckState
         -> ClckwrksConfig ClckURL
         -> IO (ClckState, ClckwrksConfig ClckURL)
initHook clckState cc =
    do let p = plugins clckState
       initPlugin p "" clckPlugin
       setTheme p (Just theme)
       return (clckState, cc)
