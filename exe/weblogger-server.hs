{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Sequence 
import Data.Acid 
import Yesod
--
import Application.WebLogger.Server.Type
import Application.WebLogger.Server.Yesod ()

main :: IO ()
main = do 
  putStrLn "weblogger-server"
  acid <- openLocalState empty 
  warpDebug 7800 (WebLoggerServer acid)