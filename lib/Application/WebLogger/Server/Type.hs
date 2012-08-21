{-# LANGUAGE OverloadedStrings #-}

module Application.WebLogger.Server.Type where

import Control.Applicative
import Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Yesod.Dispatch
import Text.Blaze
import Application.WebLogger.Type
-- import Debug.Trace 
import Data.Acid


data WebLoggerServer = WebLoggerServer {
  server_acid :: AcidState WebLoggerRepo
}
