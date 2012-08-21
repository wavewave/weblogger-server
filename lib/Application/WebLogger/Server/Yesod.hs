{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, 
             MultiParamTypeClasses, TypeFamilies, FlexibleContexts,  
             FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Application.WebLogger.Server.Yesod where 

import           Control.Applicative
import           Data.Acid
import           Data.Aeson as A
import           Data.Attoparsec as P
import qualified Data.ByteString as S
import           Data.Conduit 
import qualified Data.Conduit.List as CL
import           Data.Foldable
import           Network.Wai
import           Yesod hiding (update)
-- 
import           Application.WebLogger.Type
import           Application.WebLogger.Server.Type
-- 
import           Prelude hiding (concat,concatMap)


-- |
mkYesod "WebLoggerServer" [parseRoutes|
/ HomeR GET
/list ListR GET
/upload UploadR POST
|]


-- |
instance Yesod WebLoggerServer where
  maximumContentLength _ _ = 100000000


-- |
getHomeR :: Handler RepHtml 
getHomeR = do 
  liftIO $ putStrLn "getHomeR called"
  defaultLayout [whamlet|
!!!
<html>
  <head> 
    <title> weblogger 
  <body> 
    <h1> Weblogger
|]


-- |
defhlet :: GWidget s m ()
defhlet = [whamlet| <h1> HTML output not supported |]


-- | 
showstr :: String -> GWidget s m () 
showstr str  = [whamlet| 
<h1> output 
<p> #{str}
|]


-- | 
formatLog :: WebLoggerRepo -> GWidget s m () 
formatLog xs = [whamlet| 
<h1> output
<table> 
  <tr> 
    <td> log
  $forall x <- xs 
    <tr> 
      <td> #{weblog_content x}
|]

--  concatMap (\x -> weblog_content x ++ "\n\n") (toList xs)


-- |
getListR :: Handler RepHtmlJson
getListR = do 
  -- setHeader "Access-Control-Allow-Origin" "*" 
  -- setHeader "Access-Control-Allow-Headers" "X-Requested-With, Content-Type"
  liftIO $ putStrLn "getListR called" 
  acid <- server_acid <$> getYesod
  r <- liftIO $ query acid QueryAllLog
  -- liftIO $ putStrLn $ show r 
  defaultLayoutJson (formatLog r) (A.toJSON (Just r))


-- | 
postUploadR :: Handler RepHtmlJson
postUploadR = do 
  liftIO $ putStrLn "postQueueR called" 
  acid <- server_acid <$> getYesod
  wr <- reqWaiRequest <$> getRequest
  bs' <- liftIO $ runResourceT (requestBody wr $$ CL.consume)
  let bs = S.concat bs' 
  let parsed = parse json bs 
  case parsed of 
    Done _ parsedjson -> do 
      case (A.fromJSON parsedjson :: A.Result WebLoggerInfo) of 
        Success minfo -> do 
          r <- liftIO $ update acid (AddLog minfo)
          liftIO $ print (Just r)
          liftIO $ print (A.toJSON (Just r))
          defaultLayoutJson defhlet (A.toJSON (Just r))
        Error err -> do 
          liftIO $ putStrLn err 
          defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe WebLoggerInfo))
    Fail _ ctxts err -> do 
      liftIO $ putStrLn (concat ctxts++err)
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe WebLoggerInfo))
    Partial _ -> do 
      liftIO $ putStrLn "partial" 
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe WebLoggerInfo))


