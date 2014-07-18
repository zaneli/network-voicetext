{-# LANGUAGE FlexibleContexts #-}

module Network.VoiceText (
  basicAuth,
  ttsParams,
  addEmotion,
  addEmotionLevel,
  addPitch,
  addSpeed,
  addVolume,
  tts,
  ttsToFile,
  BasicAuth(..),
  TtsParams(..),
  Speaker(..),
  Emotion(..),
  Error(..)) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.ByteString.Lazy (hPut)
import Data.Maybe (maybeToList)
import GHC.Exception (throw)
import Network.HTTP.Conduit (
  applyBasicAuth, checkStatus, HttpException(StatusCodeException), httpLbs, parseUrl, responseBody, responseStatus, urlEncodedBody, withManager)
import Network.HTTP.Types (statusCode, statusMessage)
import Network.VoiceText.Types
import System.IO (openFile, IOMode(WriteMode))
import qualified Data.ByteString.Lazy.Internal as LI
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.UTF8 as U8

apiURLBase :: String
apiURLBase = "https://api.voicetext.jp/"

ttsToFile :: FilePath -> BasicAuth -> TtsParams -> IO (Either Error ())
ttsToFile filePath basicAuth ttsParam = do
  res <- tts basicAuth ttsParam
  case res of
    Left err -> return $ Left err
    Right bytes -> do
      fileH <- openFile filePath WriteMode
      hPut fileH bytes
      return $ Right ()

tts :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) => BasicAuth -> TtsParams -> m (Either Error LI.ByteString)
tts basicAuth ttsParam = doRequest basicAuth "v1/tts" $ [textParam, speakerParam] ++ optionParams
  where
    textParam = ("text", text ttsParam)
    speakerParam = ("speaker", speakerName $ speaker ttsParam)
    optionParams = concat $ fmap maybeToList $ [
      fmap (\e -> ("emotion", emotionName e)) $ emotion ttsParam,
      fmap (\el -> ("emotion_level", show el)) $ emotionLevel ttsParam,
      fmap (\p -> ("pitch", show p)) $ pitch ttsParam,
      fmap (\s -> ("speed", show s)) $ speed ttsParam,
      fmap (\v -> ("volume", show v)) $ volume ttsParam]

doRequest :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) => BasicAuth -> String -> [(String, String)] -> m (Either Error LI.ByteString)
doRequest basicAuth path params = do
  req <- (parseUrl $ apiURLBase ++ path)
      >>= return . applyBasicAuth user pass
      >>= return . urlEncodedBody packedParams
      >>= \r -> return (r { checkStatus = ckSt })
  res <- withManager (\manager -> httpLbs req manager)
  return $ returnValue res
  where
    user = C8.pack $ username basicAuth
    pass = C8.pack $ password basicAuth
    packedParams = map (\(k, v) -> (C8.pack k, U8.fromString v)) params
    returnValue res
      | statusCode status == 200  = Right $ responseBody res
      | otherwise = Left $ Error { code = statusCode status, message = C8.unpack $ statusMessage status, body = LI.unpackChars $ responseBody res}
      where
        status = responseStatus res
    ckSt status headers cookieJar
      | statusCode status < 500 = Nothing
      | otherwise               = throw $ StatusCodeException status headers cookieJar
