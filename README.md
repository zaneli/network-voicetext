# Network.VoiceText

[VoiceText Web API](https://cloud.voicetext.jp/webapi) Haskell wrapper library

[![Build Status](https://api.travis-ci.org/zaneli/network-voicetext.png?branch=master)](https://travis-ci.org/zaneli/network-voicetext)

## Installation

```
> curl -O http://www.zaneli.com/repositories/cabalpkg/network-voicetext-0.0.0.1.tar.gz
> cabal install network-voicetext-0.0.0.1.tar.gz
```

## Usage

### Create Basic Auth Information
```
basicAuth "basic_auth_username" ""
```

### Create TTS Parameter

#### Set only the required items.

```
ttsParams "Hello, world." Show
```

#### Set the all items.
```
addVolume 120 $
  addSpeed 150 $
  addPitch 50 $
  addEmotionLevel 2 $
  addEmotion Happiness $
  ttsParams "こんにちは" Takeru
```

### Send the request

#### Create sound file.

call `ttsToFile`.

```
import Network.VoiceText

main = do
  let b = basicAuth "basic_auth_username" ""
  let p = ttsParams "Hello, world." Show
  ttsToFile "./test.wav" b p
```

#### Use response data to another way.

call `tts`, and use response `Data.ByteString.Lazy.Internal.ByteString` data.

```
import Network.VoiceText

main = do
  let b = basicAuth "basic_auth_username" ""
  let p = ttsParams "Hello, world." Show
  bytes <- tts b p
  print bytes
```

##### For example, play the voice data using [Sound.ALUT](https://hackage.haskell.org/package/ALUT).

```
import Network.VoiceText
import Sound.ALUT

import Data.ByteString.Internal (toForeignPtr)
import Data.ByteString.Lazy (toStrict)
import Foreign.ForeignPtr (castForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import System.Environment (getArgs)

import qualified Foreign.C.Types as CT
import qualified Sound.OpenAL.AL.Buffer as AL

main = do
  mr <- createVoice
  playVoice mr

createVoice :: IO (AL.MemoryRegion a)
createVoice = do
  [message] <- getArgs
  (Right bytes) <- tts (basicAuth "basic_auth_username" "") (ttsParams message Show)
  let (fptrw, _, ptrLength) = toForeignPtr $ toStrict bytes
  let size = CT.CInt $ fromIntegral ptrLength
  let ptr = unsafeForeignPtrToPtr $ castForeignPtr fptrw
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
