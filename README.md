# Network.VoiceText

[VoiceText Web API](https://cloud.voicetext.jp/webapi) Haskell wrapper library

[![Build Status](https://api.travis-ci.org/zaneli/network-voicetext.png?branch=master)](https://travis-ci.org/zaneli/network-voicetext)

## Usage

### Create Basic Auth Information
```
basicAuth "basic_auth_username" ""
```

### Create TTS Parameter

#### Set only the required items.

```hs
ttsParams "Hello, world." Show
```

#### Set the all items.

```hs
addVolume 120 $
  addSpeed 150 $
  addPitch 50 $
  addEmotionLevel 2 $
  addEmotion Happiness $
  addFormat Ogg $
  ttsParams "こんにちは" Takeru
```

### Send the request

#### Create sound file.

call `ttsToFile`.

```hs
import Network.VoiceText

main = do
  let b = basicAuth "basic_auth_username" ""
  let p = ttsParams "Hello, world." Show
  ttsToFile "./test.wav" b p
```

#### Use response data to another way.

call `tts`, and use response `Data.ByteString.Lazy.Internal.ByteString` data.

```hs
import Network.VoiceText

main = do
  let b = basicAuth "basic_auth_username" ""
  let p = ttsParams "Hello, world." Show
  bytes <- tts b p
  print bytes
```

##### For example, play the voice data using [Sound.ALUT](https://hackage.haskell.org/package/ALUT).

* stack.yaml

```yml
resolver: lts-2.22

packages:
- location: .
- location:
    git: https://github.com/zaneli/network-voicetext.git
    commit: 54ce62af6c4decb023a36a39c081d0411c8cc11d
  extra-dep: true

extra-deps:
- ALUT-2.4.0.2
- OpenAL-1.7.0.4
```

* example.cabal

```
library
  build-depends: base >=4.6 && <4.8
               , network-voicetext >=0.0
               , bytestring >=0.10
               , word8 >=0.1
               , OpenAL >=1.6
               , ALUT >=2.3
```

```hs
import Network.VoiceText
import Sound.ALUT
import Data.Word8

import Data.ByteString.Internal (toForeignPtr)
import Data.ByteString.Lazy (toStrict)
import Foreign.ForeignPtr (withForeignPtr)
import System.Environment (getArgs)

import qualified Foreign.C.Types as CT
import qualified Sound.OpenAL.AL.Buffer as AL

main = do
  mr <- createVoice
  playVoice mr

createVoice :: IO (AL.MemoryRegion Word8)
createVoice = do
  [message] <- getArgs
  (Right bytes) <- tts (basicAuth "basic_auth_username" "") (ttsParams message Show)
  let (fptrw, _, ptrLength) = toForeignPtr $ toStrict bytes
  withForeignPtr fptrw $ \ptr -> do
    let size = CT.CInt $ fromIntegral ptrLength
    return $ AL.MemoryRegion ptr size

playVoice :: AL.MemoryRegion a -> IO ()
playVoice mr = withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ -> do
  (Just device) <- openDevice Nothing
  context <- createContext device []
  currentContext $= context
  buffer <- createBuffer $ FileImage mr
  [source] <- genObjectNames 1
  queueBuffers source [buffer]
  play [source]
  sleep 1
  closeDevice device
  return ()
```
