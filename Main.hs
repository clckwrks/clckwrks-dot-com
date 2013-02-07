{-# LANGUAGE FlexibleContexts, OverloadedStrings, PackageImports, RankNTypes #-}
module Main where

import Clckwrks
import Clckwrks.GetOpts          (parseArgs, clckwrksOpts)
import Clckwrks.Server           (simpleClckwrks)
import Clckwrks.Plugin           (clckPlugin)
import Clckwrks.Bugs.Plugin      (bugsPlugin)
import Clckwrks.Media.Plugin     (mediaPlugin)
import Clckwrks.Page.Plugin      (pagePlugin)
import Clckwrks.Page.URL         (PageURL(..))
import Control.Applicative       ((<$>))
import Data.Text                 (Text)
import "clckwrks-theme-clckwrks" Theme (theme)
import Web.Plugins.Core          (Plugin(..), addHandler, getPluginRouteFn, initPlugin, setTheme)
import System.Environment        (getArgs)

------------------------------------------------------------------------------
-- ClckwrksConfig
------------------------------------------------------------------------------

clckwrksConfig :: ClckwrksConfig
clckwrksConfig = ClckwrksConfig
    { clckHostname        = "localhost"
    , clckHidePort        = False
    , clckPort            = 8000
    , clckTLS             = Nothing      -- disable TLS by default
    , clckJQueryPath      = "../jquery"  -- directory containing 'jquery.js'
    , clckJQueryUIPath    = ""           -- directory containing 'jquery.js'
    , clckJSTreePath      = "../jstree"  -- directory containing 'jquery.jstree.js'
    , clckJSON2Path       = "../json2"   -- directory containing 'json2.js'
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
       simpleClckwrks =<< f clckwrksConfig

initHook :: Text
         -> ClckState
         -> ClckwrksConfig
         -> IO (ClckState, ClckwrksConfig)
initHook baseURI clckState cc =
    do let p = plugins clckState
       addHandler p "blog" blogHandler
       initPlugin p "" clckPlugin
       initPlugin p "" pagePlugin
       initPlugin p "" bugsPlugin
       initPlugin p "" mediaPlugin
       setTheme p (Just theme)
       return (clckState, cc)

blogHandler :: ClckPlugins -> [Text] -> ClckT ClckURL (ServerPartT IO) Response
blogHandler plugins paths =
    do (Just showPageURL) <- getPluginRouteFn plugins (pluginName pagePlugin)
       case paths of
         [] ->
            do let blogURL = showPageURL Blog []
               seeOther blogURL (toResponse ())
         ["atom.xml"] ->
             do let atomURL = showPageURL AtomFeed []
                seeOther atomURL (toResponse ())
         _ -> notFound (toResponse ())
