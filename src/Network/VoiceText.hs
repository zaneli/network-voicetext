{-# LANGUAGE FlexibleContexts #-}

module Network.VoiceText (
  basicAuth,
  ttsParams,
  addFormat,
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
  Format(..),
  Emotion(..),
  Error(..)) where

import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString (hPut)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (catMaybes)
import GHC.Exception (throw)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Conduit (
  applyBasicAuth, checkStatus, HttpException(StatusCodeException), httpLbs, newManager, parseUrl, responseBody, responseStatus, urlEncodedBody)
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
      result <- hPut fileH $ toStrict bytes
      return $ Right result

tts :: BasicAuth -> TtsParams -> IO (Either Error LI.ByteString)
tts basicAuth ttsParam = doRequest basicAuth "v1/tts" $ [textParam, speakerParam] ++ optionParams
  where
    textParam = ("text", text ttsParam)
    speakerParam = ("speaker", speakerName $ speaker ttsParam)
    optionParams = catMaybes [
      fmap (\f -> ("format", formatName f)) $ format ttsParam,
      fmap (\e -> ("emotion", emotionName e)) $ emotion ttsParam,
      fmap (\el -> ("emotion_level", show el)) $ emotionLevel ttsParam,
      fmap (\p -> ("pitch", show p)) $ pitch ttsParam,
      fmap (\s -> ("speed", show s)) $ speed ttsParam,
      fmap (\v -> ("volume", show v)) $ volume ttsParam]

doRequest :: BasicAuth -> String -> [(String, String)] -> IO (Either Error LI.ByteString)
doRequest basicAuth path params = do
  req <- (parseUrl $ apiURLBase ++ path)
      >>= return . applyBasicAuth user pass
      >>= return . urlEncodedBody packedParams
      >>= \r -> return (r { checkStatus = ckSt })
  manager <- newManager tlsManagerSettings
  runResourceT $ do
    res <- httpLbs req manager
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
