module Main where

import Network.URI (URIAuth(..))
import System.Archive.Site (BackupTarget(..), backup)

main = backup (BackupTarget
               { app = "clckwrks-dot-com-production"
               , auth = URIAuth { uriUserInfo = "upload@"
                                , uriRegName = "clckwrks.com"
                                , uriPort = ""}
               , keep = 50
               , localTop = "/srv/backups"
               , nice = 10, delay = 0
               , bwLimit = Just 20
               , remoteTop = "/srv" })
