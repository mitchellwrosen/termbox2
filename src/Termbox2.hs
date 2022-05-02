module Termbox2
  ( init,
  )
where

import Control.Exception (Exception, throwIO)
import qualified Data.ByteString.Unsafe as ByteString
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Foreign.C.Types (CInt)
import qualified Termbox2.Internal as C
import Prelude hiding (init)

data TermboxException = TermboxException
  { function :: Text,
    message :: Text
  }
  deriving stock (Show)
  deriving anyclass (Exception)

init :: IO ()
init = do
  result <- C.init
  if
      | result == C._OK -> pure ()
      | result == C._ERR_INIT_ALREADY -> pure ()
      | otherwise -> exception "init" result

exception :: Text -> CInt -> IO a
exception function code = do
  message <- strerror code
  throwIO TermboxException{function, message}

strerror :: CInt -> IO Text
strerror n = do
  cstring <- C.strerror n
  bytes <- ByteString.unsafePackCString cstring
  pure (Text.decodeUtf8 bytes)
