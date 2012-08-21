{-# LANGUAGE OverloadedStrings #-}

module Main where

import Application.WebLogger.Server.Type
import Application.WebLogger.Server.Yesod ()
import Yesod
import qualified Data.Map as M
import Data.Acid 

main :: IO ()
main = do 
  putStrLn "weblogger-server"
  acid <- openLocalState M.empty 
  warpDebug 7800 (WebLoggerServer acid)