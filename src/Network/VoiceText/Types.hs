module Network.VoiceText.Types (
  basicAuth,
  ttsParams,
  speakerName,
  emotionName,
  addEmotion,
  addEmotionLevel,
  addPitch,
  addSpeed,
  addVolume,
  BasicAuth(..),
  TtsParams(..),
  Speaker(..),
  Emotion(..),
  Error(..)) where

data BasicAuth = BasicAuth { username::String, password::String } deriving (Show, Eq)
basicAuth :: String -> String -> BasicAuth
basicAuth username password = BasicAuth { username=username, password=password }

data TtsParams = TtsParams {
  text::String
  , speaker::Speaker
  , emotion::Maybe Emotion
  , emotionLevel::Maybe Int
  , pitch::Maybe Int
  , speed::Maybe Int
  , volume::Maybe Int } deriving (Show, Eq)

data Speaker = Show | Haruka | Hikari | Takeru deriving (Show, Eq)
speakerName :: Speaker -> String
speakerName Show   = "show"
speakerName Haruka = "haruka"
speakerName Hikari = "hikari"
speakerName Takeru = "takeru"

data Emotion = Happiness | Anger | Sadness deriving (Show, Eq)
emotionName :: Emotion -> String
emotionName Happiness = "happiness"
emotionName Anger     = "anger"
emotionName Sadness   = "sadness"

ttsParams :: String -> Speaker -> TtsParams
ttsParams text speaker = TtsParams {
  text=text, speaker=speaker, emotion=Nothing, emotionLevel=Nothing, pitch=Nothing, speed=Nothing, volume=Nothing }

addEmotion :: Emotion -> TtsParams -> TtsParams
addEmotion emotion ttsParams = ttsParams { emotion=Just emotion }

addEmotionLevel :: Int -> TtsParams -> TtsParams
addEmotionLevel emotionLevel ttsParams = ttsParams { emotionLevel=Just emotionLevel }

addPitch :: Int -> TtsParams -> TtsParams
addPitch pitch ttsParams = ttsParams { pitch=Just pitch }

addSpeed :: Int -> TtsParams -> TtsParams
addSpeed speed ttsParams = ttsParams { speed=Just speed }

addVolume :: Int -> TtsParams -> TtsParams
addVolume volume ttsParams = ttsParams { volume=Just volume }

data Error = Error { code::Int, message::String, body::String } deriving (Show, Eq)
