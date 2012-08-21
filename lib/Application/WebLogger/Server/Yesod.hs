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
import           Data.UUID
import           Network.Wai
import           Yesod hiding (update)
-- 
import           Application.WebLogger.Type
import           Application.WebLogger.Server.Type



mkYesod "WebLoggerServer" [parseRoutes|
/ HomeR GET
/listyesodcrud  ListWebLoggerR GET
/uploadyesodcrud  UploadWebLoggerR POST
/yesodcrud/#UUID WebLoggerR 
|]

instance Yesod WebLoggerServer where
  -- approot = ""
  maximumContentLength _ _ = 100000000

{-instance RenderMessage WebLoggerServer FormMessage where
  renderMessage _ _ = defaultFormMessage -}


getHomeR :: Handler RepHtml 
getHomeR = do 
  liftIO $ putStrLn "getHomeR called"
  defaultLayout [whamlet|
!!!
<html>
  <head> 
    <title> test 
  <body> 
    <h1> hello world 
|]


defhlet :: GWidget s m ()
defhlet = [whamlet| <h1> HTML output not supported |]


getListWebLoggerR :: Handler RepHtmlJson
getListWebLoggerR = do 
  liftIO $ putStrLn "getQueueListR called" 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid QueryAll
  liftIO $ putStrLn $ show r 
  defaultLayoutJson defhlet (A.toJSON (Just r))


postUploadWebLoggerR :: Handler RepHtmlJson
postUploadWebLoggerR = do 
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
          r <- liftIO $ update acid (AddWebLogger minfo)
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



handleWebLoggerR :: UUID -> Handler RepHtmlJson
handleWebLoggerR name = do
  wr <- return.reqWaiRequest =<< getRequest
  case requestMethod wr of 
    "GET" -> getWebLoggerR name
    "PUT" -> putWebLoggerR name
    "DELETE" -> deleteWebLoggerR name
    x -> error ("No such action " ++ show x ++ " in handlerWebLoggerR")

getWebLoggerR :: UUID -> Handler RepHtmlJson
getWebLoggerR idee = do 
  liftIO $ putStrLn "getWebLoggerR called"
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid (QueryWebLogger idee)
  liftIO $ putStrLn $ show r 
  let hlet = [whamlet| <h1> File #{idee}|]
  defaultLayoutJson hlet (A.toJSON (Just r))


putWebLoggerR :: UUID -> Handler RepHtmlJson
putWebLoggerR idee = do 
  liftIO $ putStrLn "putWebLoggerR called"
  acid <- server_acid <$> getYesod
  wr <- reqWaiRequest <$> getRequest
  bs' <- liftIO $ runResourceT (requestBody wr $$ CL.consume)
  let bs = S.concat bs'
  let parsed = parse json bs 
  liftIO $ print parsed 
  case parsed of 
    Done _ parsedjson -> do 
      case (A.fromJSON parsedjson :: A.Result WebLoggerInfo) of 
        Success minfo -> do 
          if idee == yesodcrud_uuid minfo
            then do r <- liftIO $ update acid (UpdateWebLogger minfo)
                    defaultLayoutJson defhlet (A.toJSON (Just r))
            else do liftIO $ putStrLn "yesodcrudname mismatched"
                    defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe WebLoggerInfo))
        Error err -> do 
          liftIO $ putStrLn err 
          defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe WebLoggerInfo))
    Fail _ ctxts err -> do 
      liftIO $ putStrLn (concat ctxts++err)
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe WebLoggerInfo))
         
    Partial _ -> do 
      liftIO $ putStrLn "partial" 
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe WebLoggerInfo))

deleteWebLoggerR :: UUID -> Handler RepHtmlJson
deleteWebLoggerR idee = do 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ update acid (DeleteWebLogger idee)
  liftIO $ putStrLn $ show r 
  defaultLayoutJson defhlet (A.toJSON (Just r))
