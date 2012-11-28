module Main where

import System.Archive.Site (BackupTarget(..), backup)

main = backup (BackupTarget {app = "clckwrks-dot-com-production", user = "upload", host = "seereason.com", keep = 50})
