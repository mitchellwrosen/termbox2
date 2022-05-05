module Termbox2
  ( Event (..),
    InputMode (..),
    Mouse (..),
    MouseMode (..),
    OutputMode (..),
    clear,
    getInputMode,
    getOutputMode,
    hideCursor,
    height,
    init,
    pollEvent,
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
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Word (Word16, Word8)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import qualified Foreign.Storable as Storable
import qualified Termbox2.Bindings
import Prelude hiding (init, mod)

data Event
  = EventKey Word8 Termbox2.Bindings.Key Word16 -- FIXME better types
  | EventResize Width Height
  | EventMouse Mouse Column Row

data InputMode
  = InputModeEsc MouseMode
  | InputModeAlt MouseMode

data Mouse
  = MouseLeft
  | MouseRight
  | MouseMiddle
  | MouseRelease
  | MouseWheelUp
  | MouseWheelDown

data MouseMode
  = MouseModeNo
  | MouseModeYes

data OutputMode
  = OutputModeNormal
  | OutputMode256
  | OutputMode216
  | OutputModeGrayscale
  | OutputModeTrueColor

type Column = Int32

type Height = Int32

type Row = Int32

type Width = Int32

clear :: IO ()
clear = do
  result <- Termbox2.Bindings.clear
  when (result /= Termbox2.Bindings._OK) (exception "tb_clear" result)

getInputMode :: IO InputMode
getInputMode = do
  result <- Termbox2.Bindings.set_input_mode Termbox2.Bindings._INPUT_CURRENT
  if
      | result == Termbox2.Bindings._INPUT_ESC -> pure (InputModeEsc MouseModeNo)
      | result == Termbox2.Bindings._INPUT_ESC .|. Termbox2.Bindings._INPUT_MOUSE -> pure (InputModeEsc MouseModeYes)
      | result == Termbox2.Bindings._INPUT_ALT -> pure (InputModeAlt MouseModeNo)
      | result == Termbox2.Bindings._INPUT_ALT .|. Termbox2.Bindings._INPUT_MOUSE -> pure (InputModeAlt MouseModeYes)
      | otherwise -> exception "tb_set_input_mode" result

getOutputMode :: IO OutputMode
getOutputMode = do
  result <- Termbox2.Bindings.set_output_mode Termbox2.Bindings._OUTPUT_CURRENT
  if
      | result == Termbox2.Bindings._OUTPUT_NORMAL -> pure OutputModeNormal
      | result == Termbox2.Bindings._OUTPUT_256 -> pure OutputMode256
      | result == Termbox2.Bindings._OUTPUT_216 -> pure OutputMode216
      | result == Termbox2.Bindings._OUTPUT_GRAYSCALE -> pure OutputModeGrayscale
      | result == Termbox2.Bindings._OUTPUT_TRUECOLOR -> pure OutputModeTrueColor
      | otherwise -> exception "tb_set_output_mode" result

height :: IO ()
height = do
  result <- Termbox2.Bindings.height
  when (result == Termbox2.Bindings._ERR_NOT_INIT) (exception "tb_height" result)

hideCursor :: IO ()
hideCursor = do
  result <- Termbox2.Bindings.hide_cursor
  when (result /= Termbox2.Bindings._OK) (exception "tb_hide_cursor" result)

init :: IO ()
init = do
  result <- Termbox2.Bindings.init
  when (result /= Termbox2.Bindings._OK && result /= Termbox2.Bindings._ERR_INIT_ALREADY) (exception "tb_init" result)

-- peek_event

pollEvent :: IO Event
pollEvent =
  fmap parseEvent do
    alloca \eventPointer -> do
      result <- Termbox2.Bindings.poll_event eventPointer
      when (result /= Termbox2.Bindings._OK) (exception "tb_poll_event" result)
      Storable.peek eventPointer
  where
    parseEvent :: Termbox2.Bindings.Event -> Event
    parseEvent = \case
      Termbox2.Bindings.EventKey mod key ch -> EventKey mod key ch
      Termbox2.Bindings.EventResize w h -> EventResize w h
      Termbox2.Bindings.EventMouse key x y -> EventMouse (parseMouse key) x y

    parseMouse :: Word16 -> Mouse
    parseMouse key
      | key == Termbox2.Bindings._KEY_MOUSE_LEFT = MouseLeft
      | key == Termbox2.Bindings._KEY_MOUSE_RIGHT = MouseRight
      | key == Termbox2.Bindings._KEY_MOUSE_MIDDLE = MouseMiddle
      | key == Termbox2.Bindings._KEY_MOUSE_RELEASE = MouseRelease
      | key == Termbox2.Bindings._KEY_MOUSE_WHEEL_UP = MouseWheelUp
      | key == Termbox2.Bindings._KEY_MOUSE_WHEEL_DOWN = MouseWheelDown
      | otherwise = error ("unknown mouse: " ++ show key)

present :: IO ()
present = do
  result <- Termbox2.Bindings.present
  when (result /= Termbox2.Bindings._OK) (exception "tb_present" result)

-- print

-- set_cell

-- set_clear_attrs

-- set_cursor

setInputMode :: InputMode -> IO ()
setInputMode mode = do
  result <- Termbox2.Bindings.set_input_mode cmode
  when (result /= Termbox2.Bindings._OK) (exception "tb_set_input_mode" result)
  where
    cmode :: CInt
    cmode =
      case mode of
        InputModeEsc MouseModeNo -> Termbox2.Bindings._INPUT_ESC
        InputModeEsc MouseModeYes -> Termbox2.Bindings._INPUT_ESC .|. Termbox2.Bindings._INPUT_MOUSE
        InputModeAlt MouseModeNo -> Termbox2.Bindings._INPUT_ALT
        InputModeAlt MouseModeYes -> Termbox2.Bindings._INPUT_ALT .|. Termbox2.Bindings._INPUT_MOUSE

setOutputMode :: OutputMode -> IO ()
setOutputMode mode = do
  result <- Termbox2.Bindings.set_output_mode cmode
  when (result /= Termbox2.Bindings._OK) (exception "tb_set_output_mode" result)
  where
    cmode :: CInt
    cmode =
      case mode of
        OutputModeNormal -> Termbox2.Bindings._OUTPUT_NORMAL
        OutputMode256 -> Termbox2.Bindings._OUTPUT_256
        OutputMode216 -> Termbox2.Bindings._OUTPUT_216
        OutputModeGrayscale -> Termbox2.Bindings._OUTPUT_GRAYSCALE
        OutputModeTrueColor -> Termbox2.Bindings._OUTPUT_TRUECOLOR

shutdown :: IO ()
shutdown = do
  result <- Termbox2.Bindings.shutdown
  when (result /= Termbox2.Bindings._OK) (exception "tb_shutdown" result)

strerror :: CInt -> IO Text
strerror n = do
  cstring <- Termbox2.Bindings.strerror n
  bytes <- ByteString.unsafePackCString cstring
  pure (Text.decodeUtf8 bytes)

width :: IO ()
width = do
  result <- Termbox2.Bindings.width
  when (result == Termbox2.Bindings._ERR_NOT_INIT) (exception "tb_width" result)

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
