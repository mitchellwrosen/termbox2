module Termbox2
  ( InputMode (..),
    clear,
    getInputMode,
    hideCursor,
    height,
    init,
    present,
    setInputMode,
    shutdown,
    width,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import qualified Data.ByteString.Unsafe as ByteString
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Foreign.C.Types (CInt)
import qualified Termbox2.Internal as C
import Prelude hiding (init)

clear :: IO ()
clear = do
  result <- C.clear
  when (result /= C._OK) (exception "tb_clear" result)

getInputMode :: IO InputMode
getInputMode = do
  result <- C.set_input_mode C._INPUT_CURRENT
  if
      | result == C._INPUT_ESC -> pure InputModeEsc
      | result == C._INPUT_ALT -> pure InputModeAlt
      | otherwise -> exception "tb_set_input_mode" result

height :: IO ()
height = do
  result <- C.height
  when (result == C._ERR_NOT_INIT) (exception "tb_height" result)

hideCursor :: IO ()
hideCursor = do
  result <- C.hide_cursor
  when (result /= C._OK) (exception "tb_hide_cursor" result)

init :: IO ()
init = do
  result <- C.init
  when (result /= C._OK && result /= C._ERR_INIT_ALREADY) (exception "tb_init" result)

-- peek_event

-- poll_event

present :: IO ()
present = do
  result <- C.present
  when (result /= C._OK) (exception "tb_present" result)

-- print

-- set_cell

-- set_clear_attrs

-- set_cursor

-- TODO mouse mode
data InputMode
  = InputModeEsc
  | InputModeAlt

setInputMode :: InputMode -> IO ()
setInputMode mode = do
  result <- C.set_input_mode cmode
  when (result /= C._OK) (exception "tb_set_input_mode" result)
  where
    cmode :: CInt
    cmode =
      case mode of
        InputModeEsc -> C._INPUT_ESC
        InputModeAlt -> C._INPUT_ALT

-- set_output_mode

shutdown :: IO ()
shutdown = do
  result <- C.shutdown
  when (result /= C._OK) (exception "tb_shutdown" result)

strerror :: CInt -> IO Text
strerror n = do
  cstring <- C.strerror n
  bytes <- ByteString.unsafePackCString cstring
  pure (Text.decodeUtf8 bytes)

width :: IO ()
width = do
  result <- C.width
  when (result == C._ERR_NOT_INIT) (exception "tb_width" result)

--

data TermboxException = TermboxException
  { function :: Text,
    message :: Text
  }
  deriving stock (Show)
  deriving anyclass (Exception)

exception :: Text -> CInt -> IO a
exception function code = do
  message <- strerror code
  throwIO TermboxException {function, message}
