{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes #-}
module Main where

import Clckwrks.URL
import Clckwrks.Admin.Template
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
import Paths_clckwrks as Clckwrks (getDataFileName)
import System.Console.GetOpt -- (Permute, OptDescr, getOpt)
import System.Environment
import System.Exit
import Theme
import Web.Plugins.Core (initPlugin, setTheme)

------------------------------------------------------------------------------
-- Command line options
------------------------------------------------------------------------------

-- | command-line Flags
data Flag
    = ModifyConfig (ClckwrksConfig -> ClckwrksConfig)
    | Help
    | Version

-- | Flag selectors
isHelp, isVersion :: Flag -> Bool
isHelp    flag = case flag of Help    -> True; _ -> False
isVersion flag = case flag of Version -> True; _ -> False

-- | Command line options.
clckwrksOpts :: ClckwrksConfig -> [OptDescr Flag]
clckwrksOpts def =
    [ Option [] ["help"]          (NoArg Help)                    "Display this help message"
    , Option [] ["http-port"]     (ReqArg setPort "port")         ("Port to bind http server, default: " ++ show (clckPort def))
    , Option [] ["hide-port"]     (NoArg setHidePort)             "Do not show the port number in the URL"
    , Option [] ["hostname"]      (ReqArg setHostname "hostname") ("Server hostename, default: " ++ show (clckHostname def))
    , Option [] ["jquery-path"]   (ReqArg setJQueryPath   "path") ("path to jquery directory, default: " ++ show (clckJQueryPath def))
    , Option [] ["jqueryui-path"] (ReqArg setJQueryUIPath "path") ("path to jqueryui directory, default: " ++ show (clckJQueryUIPath def))
    , Option [] ["jstree-path"]   (ReqArg setJSTreePath   "path") ("path to jstree directory, default: " ++ show (clckJSTreePath def))
    , Option [] ["json2-path"]    (ReqArg setJSON2Path    "path") ("path to json2 directory, default: " ++ show (clckJSON2Path def))
    , Option [] ["top"]           (ReqArg setTopDir       "path") ("path to directory that holds the state directory, uploads, etc")
    , Option [] ["static"]        (ReqArg noop "ignored")         "unused"
    , Option [] ["logs"]          (ReqArg noop "ignored")         "unimplemented"
    , Option [] ["log-mode"]      (ReqArg noop "ignored")         "unimplemented"
    , Option [] ["enable-analytics"] (NoArg setAnalytics)         "enable google analytics tracking"
    ]
    where
      noop            _   = ModifyConfig $ id
      setPort         str = ModifyConfig $ \c -> c { clckPort         = read str }
      setHostname     str = ModifyConfig $ \c -> c { clckHostname     = str      }
      setHidePort         = ModifyConfig $ \c -> c { clckHidePort     = True     }
      setJQueryPath   str = ModifyConfig $ \c -> c { clckJQueryPath   = str      }
      setJQueryUIPath str = ModifyConfig $ \c -> c { clckJQueryUIPath = str      }
      setJSTreePath   str = ModifyConfig $ \c -> c { clckJSTreePath   = str      }
      setJSON2Path    str = ModifyConfig $ \c -> c { clckJSON2Path    = str      }
      setTopDir       str = ModifyConfig $ \c -> c { clckTopDir       = Just str }
      setAnalytics        = ModifyConfig $ \c -> c { clckEnableAnalytics = True  }

-- | Parse the command line arguments into a list of flags. Exits with usage
-- message, in case of failure.
parseArgs :: [OptDescr Flag] -> [String] -> IO (ClckwrksConfig -> ClckwrksConfig)
parseArgs opts args =
    do staticDir <- getDataFileName "static"
       case getOpt Permute opts args of
         (flags,_,[]) ->
             if any isHelp flags
             then do putStr (helpMessage opts)
                     exitSuccess
             else do let f config =
                             let c   = foldr (.) (\c -> c { clckStaticDir = staticDir }) [f | (ModifyConfig f) <- flags ] config
                                 url = "http://" ++ (clckHostname c) ++ if ((clckPort c /= 80) && (clckHidePort c == False)) then (':' : show (clckPort c)) else ""
                             in c { clckInitHook = initHook (Text.pack url) }
                     return f
         (_,_,errs)   ->
             do putStr ("Failure while parsing command line:\n"++unlines errs)
                putStr (helpMessage opts)
                exitFailure

-- | A simple usage message listing all flags possible.
helpMessage :: [OptDescr Flag] -> String
helpMessage opts =
    usageInfo header opts
    where
      header = "Usage: clckwrks [OPTION...]"


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
    , clckStaticDir       = "../clckwrks/static"
    , clckTopDir          = Nothing
    , clckEnableAnalytics = False
    , clckInitHook        = initHook ""
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
