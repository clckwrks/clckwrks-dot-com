{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Main where

import Clckwrks.URL
import Clckwrks.Admin.Template
import Clckwrks.Monad
import Clckwrks.Server
import Clckwrks.Plugin
import Clckwrks.Bugs.Plugin
import Control.Applicative ((<$>))
import Control.Monad.Trans
import Theme
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Happstack.Server
import Web.Plugins.Core (initPlugin, setTheme)

clckwrksConfig :: ClckwrksConfig
clckwrksConfig = ClckwrksConfig
    { clckHostname        = "localhost"
    , clckPort            = 8000
    , clckHidePort        = False
    , clckJQueryPath      = ""
    , clckJQueryUIPath    = ""
    , clckJSTreePath      = ""
    , clckJSON2Path       = ""
    , clckStaticDir       = "../clckwrks/static"
    , clckTopDir          = Nothing
    , clckEnableAnalytics = False
    , clckInitHook        = initHook ""
    }

main :: IO ()
main = simpleClckwrks clckwrksConfig

initHook :: Text
         -> ClckState
         -> ClckwrksConfig
         -> IO (ClckState, ClckwrksConfig)
initHook baseURI clckState cc =
    do let p = plugins clckState
       initPlugin p baseURI clckPlugin
       initPlugin p baseURI bugsPlugin
       setTheme p (Just theme)
       return (clckState, cc)
