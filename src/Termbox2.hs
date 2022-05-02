module Termbox2
  ( InputMode (..),
    MouseMode (..),
    OutputMode (..),
    clear,
    getInputMode,
    getOutputMode,
    hideCursor,
    height,
    init,
    present,
    setInputMode,
    setOutputMode,
    shutdown,
    width,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.Bits ((.|.))
import qualified Data.ByteString.Unsafe as ByteString
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Foreign.C.Types (CInt)
import qualified Termbox2.Internal as C
import Prelude hiding (init)

-- TODO mouse mode
data InputMode
  = InputModeEsc MouseMode
  | InputModeAlt MouseMode

data MouseMode
  = MouseModeNo
  | MouseModeYes

data OutputMode
  = OutputModeNormal
  | OutputMode256
  | OutputMode216
  | OutputModeGrayscale
  | OutputModeTrueColor

clear :: IO ()
clear = do
  result <- C.clear
  when (result /= C._OK) (exception "tb_clear" result)

getInputMode :: IO InputMode
getInputMode = do
  result <- C.set_input_mode C._INPUT_CURRENT
  if
      | result == C._INPUT_ESC -> pure (InputModeEsc MouseModeNo)
      | result == C._INPUT_ESC .|. C._INPUT_MOUSE -> pure (InputModeEsc MouseModeYes)
      | result == C._INPUT_ALT -> pure (InputModeAlt MouseModeNo)
      | result == C._INPUT_ALT .|. C._INPUT_MOUSE -> pure (InputModeAlt MouseModeYes)
      | otherwise -> exception "tb_set_input_mode" result

getOutputMode :: IO OutputMode
getOutputMode = do
  result <- C.set_output_mode C._OUTPUT_CURRENT
  if
      | result == C._OUTPUT_NORMAL -> pure OutputModeNormal
      | result == C._OUTPUT_256 -> pure OutputMode256
      | result == C._OUTPUT_216 -> pure OutputMode216
      | result == C._OUTPUT_GRAYSCALE -> pure OutputModeGrayscale
      | result == C._OUTPUT_TRUECOLOR -> pure OutputModeTrueColor
      | otherwise -> exception "tb_set_output_mode" result

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

setInputMode :: InputMode -> IO ()
setInputMode mode = do
  result <- C.set_input_mode cmode
  when (result /= C._OK) (exception "tb_set_input_mode" result)
  where
    cmode :: CInt
    cmode =
      case mode of
        InputModeEsc MouseModeNo -> C._INPUT_ESC
        InputModeEsc MouseModeYes -> C._INPUT_ESC .|. C._INPUT_MOUSE
        InputModeAlt MouseModeNo -> C._INPUT_ALT
        InputModeAlt MouseModeYes -> C._INPUT_ALT .|. C._INPUT_MOUSE

setOutputMode :: OutputMode -> IO ()
setOutputMode mode = do
  result <- C.set_output_mode cmode
  when (result /= C._OK) (exception "tb_set_output_mode" result)
  where
    cmode :: CInt
    cmode =
      case mode of
        OutputModeNormal -> C._OUTPUT_NORMAL
        OutputMode256 -> C._OUTPUT_256
        OutputMode216 -> C._OUTPUT_216
        OutputModeGrayscale -> C._OUTPUT_GRAYSCALE
        OutputModeTrueColor -> C._OUTPUT_TRUECOLOR

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
