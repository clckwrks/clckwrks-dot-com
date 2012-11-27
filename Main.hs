{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes #-}
module Main where

import Clckwrks.URL
import Clckwrks.Admin.Template
import Clckwrks.GetOpts
import Clckwrks.Monad
import Clckwrks.Server
import Clckwrks.Plugin
import Clckwrks.Bugs.Plugin
import Clckwrks.Media.Plugin
import Control.Applicative ((<$>))
import Control.Monad.Trans
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Happstack.Server
import Theme
import Web.Plugins.Core (initPlugin, setTheme)
import System.Environment (getArgs)


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
