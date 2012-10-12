module Main where

import System.Archive.Site (BackupTarget(..), backup)

main = backup (BackupSite {app = "clckwrks-dot-com-production", user = "upload", host = "seereason.com", keep = 50})
