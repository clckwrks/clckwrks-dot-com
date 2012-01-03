{-# LANGUAGE CPP, RankNTypes, RecordWildCards #-}
module Main where

import Control.Monad.State (evalStateT, get, modify)
import Clckwrks
import Clckwrks.Server
import Clckwrks.Media
import Clckwrks.Media.PreProcess (mediaCmd)
import qualified Data.ByteString.Char8 as C
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Monoid (mappend)
import Data.Text  (Text)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text as Text
import URL
import Web.Routes.Happstack

#ifdef PLUGINS
import Control.Monad.State (get)
import System.Plugins.Auto (PluginHandle, PluginConf(..), defaultPluginConf, initPlugins, withMonadIOFile)
#else
import PageMapper
#endif

clckwrksConfig :: ClckwrksConfig SiteURL
clckwrksConfig = ClckwrksConfig
      { clckHostname     = "localhost"
      , clckPort         = 8000
      , clckURL          = C
      , clckJQueryPath   = "/usr/share/javascript/jquery/"
      , clckJQueryUIPath = "/usr/share/javascript/jquery-ui/"
      , clckJSTreePath   = "../jstree/"
      , clckJSON2Path    = "../json2/"
      , clckThemeDir     = "../clckwrks-theme-basic/"
      , clckStaticDir    = "../static"
#ifdef PLUGINS
      , clckPageHandler  = undefined
#else
      , clckPageHandler  = staticPageHandler
#endif
      }

data SitePlus url a = SitePlus 
    { siteSite    :: Site url a
    , siteDomain  :: Text
    , sitePort    :: Int
    , siteAppRoot :: Text
    , sitePrefix  :: Text
    , siteShowURL :: url -> [(Text, Maybe Text)] -> Text
    , siteParsePathInfo :: C.ByteString -> Either String url
    }

instance Functor (SitePlus url) where
  fmap f sitePlus = sitePlus { siteSite = fmap f (siteSite sitePlus) }

mkSitePlus :: Text
           -> Int
           -> Text
           -> Site url a
           -> SitePlus url a
mkSitePlus domain port approot site =
    SitePlus { siteSite          = site 
             , siteDomain        = domain
             , sitePort          = port
             , siteAppRoot       = approot
             , sitePrefix        = prefix
             , siteShowURL       = showFn
             , siteParsePathInfo = parsePathSegments site . decodePathInfo
             }
    where
      showFn url qs =
        let (pieces, qs') = formatPathSegments site url
        in approot `mappend` (encodePathInfo pieces (qs ++ qs'))
      prefix = Text.concat $ [ Text.pack "http://" 
                             , domain
                             ] ++
                             (if port == 80
                                then []
                                else [Text.pack ":", Text.pack $ show port]
                             ) ++
                             [ approot ]

runSitePlus_ :: (Happstack m) => SitePlus url (m a) -> m (Either String a)
runSitePlus_ sitePlus =
    dirs (Text.unpack (siteAppRoot sitePlus)) $
         do rq <- askRq
            let pathInfo = C.pack $ intercalate "/" (map escapeSlash (rqPaths rq))
                r        = runSite (sitePrefix sitePlus) (siteSite sitePlus) pathInfo
            case r of
              (Left parseError) -> return (Left parseError)
              (Right sp)   -> Right <$> (localRq (const $ rq { rqPaths = [] }) sp)
        where
          escapeSlash :: String -> String
          escapeSlash [] = []
          escapeSlash ('/':cs) = "%2F" ++ escapeSlash cs
          escapeSlash (c:cs)   = c : escapeSlash cs

runSitePlus :: (Happstack m) => SitePlus url (m a) -> m a
runSitePlus sitePlus =
    do r <- runSitePlus_ sitePlus
       case r of
         (Left _)  -> mzero
         (Right a) -> return a

{-
clckwrks' :: ToMessage a =>
             Text.Text
          -> ClckwrksConfig url
          -> (ClckState -> MediaConfig -> RouteT SiteURL (ServerPartT IO) a)
          -> IO ()
clckwrks' approot cc handler =
  withClckwrks cc $ \clckState ->
   withMediaConfig Nothing "_uploads" clckState $ \clckState' mediaConfig ->
       let s = site (clckPageHandler cc) clckState' mediaConfig
       in unRouteT (r clckState mediaConfig) (showUrlFn s)
    where
      showUrlFn s url qs =
        let (pieces, qs') = formatPathSegments s url
        in approot `mappend` encodePathInfo pieces (qs ++ qs')
      r :: ClckState -> MediaConfig -> RouteT SiteURL IO ()
      r clckState mediaConfig = 
          do rf <- askRouteFn
             let clckState' = clckState { preProcessorCmds = Map.insert (Text.pack "media") (mediaCmd (\u p -> rf (M u) p)) (preProcessorCmds clckState) }
             clckwrks_ cc (handler clckState' mediaConfig)
-}
{-
clckwrksServer :: (ToMessage a) => Conf -> ClckT SiteURL (ServerPartT IO) a -> IO ()
clckwrksServer =
    runClckT
-}
mkSite3 :: ClckwrksConfig url -> ClckState -> MediaConfig -> Site SiteURL (ServerPartT IO Response)
mkSite3 cc clckState mediaConfig = setDefault (C $ ViewPage $ PageId 1) $ mkSitePI route'
    where
      route' :: (SiteURL -> [(Text.Text, Maybe Text.Text)] -> Text.Text) -> SiteURL -> (ServerPartT IO Response)
      route' f u =
          evalStateT (unRouteT (unClckT (route2 cc mediaConfig u)) f) clckState
{-
          routeSite ph media u
-}

mkSite4 :: ClckwrksConfig url -> ClckState -> MediaConfig -> Site SiteURL (ClckT SiteURL IO Response)
mkSite4 cc clckState mediaConfig = setDefault (C $ ViewPage $ PageId 1) $ mkSitePI route'
    where
      route' :: (SiteURL -> [(Text.Text, Maybe Text.Text)] -> Text.Text) -> SiteURL -> (ClckT SiteURL IO Response)
      route' f u = undefined
--           evalStateT (unRouteT (unClckT (route2 cc mediaConfig u)) f) clckState


clckwrksServer' :: (ToMessage a) => Conf -> ClckT SiteURL (ServerPartT IO) a -> ClckT SiteURL IO ()
clckwrksServer' conf handler = 
    do cs <- get
       routeFn <- askRouteFn
       liftIO $ simpleHTTP conf (evalStateT (unRouteT (unClckT handler) routeFn) cs)

route2 :: ClckwrksConfig url -> MediaConfig -> SiteURL -> ClckT SiteURL (ServerPartT IO) Response
route2 cc mediaConfig url =
    do decodeBody (defaultBodyPolicy "/tmp/" (10 * 10^6)  (1 * 10^6)  (1 * 10^6))
       msum $  [ jsHandlers cc
               , dir "favicon.ico" $ notFound (toResponse ())
               , dir "static"      $ serveDirectory DisableBrowsing [] (clckStaticDir cc)
               , routeSite (clckPageHandler cc) mediaConfig url
--               , implSite (Text.pack $ "http://" ++ clckHostname cc ++ ":" ++ show (clckPort cc)) (Text.pack "") (mkSite2 (clckPageHandler cc) mediaConfig) 
               -- this is no good because we need the path higher up
               ]

runTheSite :: Site SiteURL (ClckT SiteURL (ServerPartT IO) Response) -> IO ()
runTheSite site = undefined

{-
addMediaPlugin :: Site SiteURL (ClckT SiteURL (ServerPartT IO) Response) -> ClckT SiteURL (ServerPartT IO) ()
addMediaPlugin site u = 
    do rf <- askRouteFn
       modify $ \clckState -> clckState { preProcessorCmds = Map.insert (Text.pack "media") (mediaCmd (\u p -> rf (M u) p)) (preProcessorCmds clckState) }
       liftIO $ simpleHTTP nullConf $ 
       implSite
-}
{-
clckwrks cc =
  withClckwrks cc $ \clckState ->
   withMediaConfig Nothing "_uploads" $ \mediaConfig ->
    do let conf = nullConf { port = clckPort cc }
           site = mkSite2 cc mediaConfig
       runTheSite site
-}
{-
    in simpleHTTP conf $ implSite (Text.pack $ "http://" ++ clckHostname cc ++ ":" ++ show (clckPort cc)) (Text.pack "") (mkSite3 cc clckState' mediaConfig)
-}
{-
    in implSite (Text.pack $ "http://" ++ clckHostname cc ++ ":" ++ show (clckPort cc)) (Text.pack "") (mkSite3 cc conf clckState' mediaConfig)
-}
route' :: Text.Text -> ClckwrksConfig url -> ClckState -> MediaConfig -> RouteT SiteURL (ServerPartT IO) Response
route' approot cc clckState mediaConfig =
    (lift $ do decodeBody (defaultBodyPolicy "/tmp/" (10 * 10^6)  (1 * 10^6)  (1 * 10^6))
               msum $  [ jsHandlers cc
                       , dir "favicon.ico" $ notFound (toResponse ())
                      , dir "static"      $ serveDirectory DisableBrowsing [] (clckStaticDir cc)
--            , implSite (Text.pack $ "http://" ++ clckHostname cc ++ ":" ++ show (clckPort cc)) (Text.pack "") (clckSite (clckPageHandler cc) clckState)
--            , implSite (Text.pack $ "http://" ++ clckHostname cc ++ ":" ++ show (clckPort cc)) (Text.pack "") (site (clckPageHandler cc) clckState media)
                      ])
     `mplus` r
    where
      r :: RouteT SiteURL (ServerPartT IO) Response
      r = dirs (Text.unpack approot) $
            do rq <- askRq
               let pathInfo = intercalate "/" (map escapeSlash (rqPaths rq))
                   s = mkSite (clckPageHandler cc) clckState mediaConfig
               case parsePathSegments s $ decodePathInfo (C.pack pathInfo) of
                 (Left parseError) -> notFound $ toResponse parseError
                 (Right url) ->
                     mapRouteT (\m -> evalStateT m clckState) $ unClckT $ routeSite (clckPageHandler cc) mediaConfig url
      escapeSlash :: String -> String
      escapeSlash [] = []
      escapeSlash ('/':cs) = "%2F" ++ escapeSlash cs
      escapeSlash (c:cs)   = c : escapeSlash cs
{-
runClckwrks :: Conf -> Text.Text -> ClckT SiteURL IO () -> ClckState -> Site SiteURL (ClckT SiteURL (ServerPartT IO) Response) -> IO ()
runClckwrks conf approot init clckState site  =
    do -- runClckT showFn clckState init
       simpleHTTP conf $ runClckT showFn clckState (handleSite site)
    where
    showFn url qs =
        let (pieces, qs') = formatPathSegments site url
        in approot `mappend` (encodePathInfo pieces (qs ++ qs'))

  -}  
    
clckwrks :: ClckwrksConfig SiteURL -> IO ()
clckwrks cc =
    withClckwrks cc $ \clckState ->
        withMediaConfig Nothing "_uploads" $ \mediaConf ->
            let -- site     = mkSite (clckPageHandler cc) clckState mediaConf
                site     = mkSite2 cc mediaConf
                sitePlus = mkSitePlus (Text.pack "localhost") (clckPort cc) Text.empty site
                mediaCmd' :: forall url m. (Monad m) => (Text -> ClckT url m Builder)
                mediaCmd' = mediaCmd (\u p -> (siteShowURL sitePlus) (M u) p)
                clckState' = clckState { preProcessorCmds = Map.insert (Text.pack "media") mediaCmd' (preProcessorCmds clckState) }
                sitePlus'  = fmap (runClckT (siteShowURL sitePlus) clckState') sitePlus
            in simpleHTTP (nullConf { port = clckPort cc }) (route cc sitePlus')

route :: Happstack m => ClckwrksConfig url -> SitePlus url (m Response) -> m Response
route cc sitePlus =
    do decodeBody (defaultBodyPolicy "/tmp/" (10 * 10^6)  (1 * 10^6)  (1 * 10^6))
       msum $ 
            [ jsHandlers cc
            , dir "favicon.ico" $ notFound (toResponse ())
            , dir "static"      $ serveDirectory DisableBrowsing [] (clckStaticDir cc)
            , runSitePlus sitePlus
--            , implSite (Text.pack $ "http://" ++ clckHostname cc ++ ":" ++ show (clckPort cc)) (Text.pack "") (clckSite (clckPageHandler cc) clckState)
--            , implSite (Text.pack $ "http://" ++ clckHostname cc ++ ":" ++ show (clckPort cc)) (Text.pack "") site
            ]

{-

we can't register the pp callbacks instead the nestURL because then
the only callbacks will only be available when that route is active.

it is 'tricky' to register the callbacks outside, because the
callbacks might require information that is only available 'inside'
the monad. But, of course, that is silly now that we think about
it. Because that monad is only available when processing the route. But when doing the pp, that route may not be the one we are doing.

So, the reason it is hard to get the monad into the callback is because we shouldn't. The pp has to assume that the route being processed is not one of those.

Well, that is not actually a problem. The monad is really an environment in which a computation can run. And we can create that environment multiple ways.

The issue with the MediaT monad is that it includes the MediaURL. And so to work with that, we need to specify how to turn a MediaURL into a SiteURL. 

That is something we normally do in routeSite via 'nestURL M'. But that means we have to repeat ourselves.

we could have a function like withMediaT to contruct a temporary MediaT monad to be used when registering the callback. Though there is a danger there, because some of the information use to register the callback might become stale.

In theory, we would like to do some stuff in the ClckT monad before start listening to incoming requests. However, to run the ClckT monad we need to provide the show function. Normally that is done transparently via implSite / site / etc.

Though it seems the information we need comes from Site not implSite.
-}
routeSite :: Clck ClckURL Response -> MediaConfig -> SiteURL -> Clck SiteURL Response
routeSite pageHandler media url =
    do 
       case url of
        (C clckURL)  -> nestURL C $ routeClck pageHandler clckURL
        (M mediaURL) -> nestURL M $ runMediaT media $ routeMedia mediaURL
      
mkSite :: Clck ClckURL Response -> ClckState -> MediaConfig -> Site SiteURL (ServerPart Response)
mkSite ph clckState media = setDefault (C $ ViewPage $ PageId 1) $ mkSitePI route'
    where
      route' f u =
          evalStateT (unRouteT (unClckT $ routeSite ph media u) f) clckState

mkSite2 :: ClckwrksConfig u -> MediaConfig -> Site SiteURL (ClckT SiteURL (ServerPartT IO) Response)
mkSite2 cc media = setDefault (C $ ViewPage $ PageId 1) $ mkSitePI route'
    where
      route' :: (SiteURL -> [(Text.Text, Maybe Text.Text)] -> Text.Text) -> SiteURL -> ClckT SiteURL (ServerPartT IO) Response
      route' f u =
          route2 cc media u


#ifdef PLUGINS
main :: IO ()
main = 
  do ph <- initPlugins 
     putStrLn "Dynamic Server Started."
     clckwrks (clckwrksConfig { clckPageHandler = dynamicPageHandler ph })

dynamicPageHandler :: PluginHandle -> Clck ClckURL Response
dynamicPageHandler ph =
  do fp <- themePath <$> get
     withMonadIOFile "PageMapper.hs" "pageMapper" ph (\pc -> pc { pcGHCArgs = [ "-i" ++ fp]  }) notLoaded page
  where
    page :: [String] -> XMLGenT (Clck url) XML -> Clck url Response
    page _errs (XMLGenT part) = toResponse <$> part
    notLoaded errs =
      internalServerError $ toResponse $ unlines errs
#else
main :: IO ()
main = 
  do putStrLn "Static Server Started."
     clckwrks clckwrksConfig

staticPageHandler :: Clck ClckURL Response
staticPageHandler = toResponse <$> unXMLGenT pageMapper
#endif
