module Network.VoiceText.Types (
  basicAuth,
  ttsParams,
  speakerName,
  formatName,
  emotionName,
  addFormat,
  addEmotion,
  addEmotionLevel,
  addPitch,
  addSpeed,
  addVolume,
  BasicAuth(..),
  TtsParams(..),
  Speaker(..),
  Format(..),
  Emotion(..),
  Error(..)) where

data BasicAuth = BasicAuth { username::String, password::String } deriving (Eq, Show)
basicAuth :: String -> String -> BasicAuth
basicAuth username password = BasicAuth { username=username, password=password }

data TtsParams = TtsParams {
  text::String
  , speaker::Speaker
  , format::Maybe Format
  , emotion::Maybe Emotion
  , emotionLevel::Maybe Int
  , pitch::Maybe Int
  , speed::Maybe Int
  , volume::Maybe Int } deriving (Eq, Show)

data Speaker = Show | Haruka | Hikari | Takeru | Santa | Bear deriving (Eq, Read, Show)
speakerName :: Speaker -> String
speakerName Show   = "show"
speakerName Haruka = "haruka"
speakerName Hikari = "hikari"
speakerName Takeru = "takeru"
speakerName Santa = "santa"
speakerName Bear = "bear"

data Format = Wav | Ogg | Aac deriving (Eq, Read, Show)
formatName :: Format -> String
formatName Wav = "wav"
formatName Ogg = "ogg"
formatName Aac = "aac"

data Emotion = Happiness | Anger | Sadness deriving (Eq, Read, Show)
emotionName :: Emotion -> String
emotionName Happiness = "happiness"
emotionName Anger     = "anger"
emotionName Sadness   = "sadness"

data Error = Error { code::Int, message::String, body::String } deriving (Eq, Show)

ttsParams :: String -> Speaker -> TtsParams
ttsParams text speaker = TtsParams {
  text=text,
  speaker=speaker,
  format=Nothing,
  emotion=Nothing,
  emotionLevel=Nothing,
  pitch=Nothing,
  speed=Nothing,
  volume=Nothing }

addFormat :: Format -> TtsParams -> TtsParams
addFormat format ttsParams = ttsParams { format=Just format }

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
