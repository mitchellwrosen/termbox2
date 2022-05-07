module Termbox2
  ( debug,
    Event (..),
    InputMode (..),
    Termbox2.Bindings.Mouse (..),
    Mod,
    alt,
    ctrl,
    shift,
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
    print,
    setInputMode,
    setOutputMode,
    shutdown,
    width,
  )
where

import Control.Exception (Exception, bracket_, throwIO)
import Control.Monad (guard, when)
import Data.Bits ((.&.), (.|.))
import qualified Data.ByteString as ByteString (useAsCString)
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.Char as Char
import Data.Coerce (coerce)
import Data.Functor (($>))
import Data.Int (Int32)
import qualified Data.List as List
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word32)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import qualified Foreign.Storable as Storable
import qualified Termbox2.Bindings
import Prelude hiding (init, mod, print)

debug :: IO ()
debug =
  bracket_ init shutdown do
    setInputMode (InputModeEsc MouseModeYes)
    setOutputMode OutputMode256
    let loop = do
          event <- pollEvent
          if event == EventKey mempty (Termbox2.Bindings.Key 27)
            then pure ()
            else do
              clear
              print 5 5 0 0 (Text.pack (show event))
              present
              loop
    loop

data Event
  = EventChar Mod Char
  | EventKey Mod Termbox2.Bindings.Key -- FIXME better types
  | EventResize Width Height
  | EventMouse Termbox2.Bindings.Mouse Column Row
  deriving stock (Eq, Show)

data InputMode
  = InputModeEsc MouseMode
  | InputModeAlt MouseMode

newtype Mod
  = Mod Termbox2.Bindings.Mod
  deriving stock (Eq)

instance Monoid Mod where
  mempty = Mod (Termbox2.Bindings.Mod 0)
  mappend = (<>)

instance Semigroup Mod where
  Mod x <> Mod y =
    Mod (x .|. y)

instance Show Mod where
  show mod
    | mod == mempty = "mempty"
    | mod == alt = "alt"
    | mod == ctrl = "ctrl"
    | mod == shift = "shift"
    | otherwise =
        "("
          ++ List.intercalate
            " <> "
            ( catMaybes
                [ guard (altd mod) $> "alt",
                  guard (ctrld mod) $> "ctrl",
                  guard (shiftd mod) $> "shift"
                ]
            )
          ++ ")"

alt :: Mod
alt =
  Mod Termbox2.Bindings.ModAlt

ctrl :: Mod
ctrl =
  Mod Termbox2.Bindings.ModCtrl

shift :: Mod
shift =
  Mod Termbox2.Bindings.ModShift

altd :: Mod -> Bool
altd (Mod x) =
  x .&. Termbox2.Bindings.ModAlt /= Termbox2.Bindings.Mod 0

ctrld :: Mod -> Bool
ctrld (Mod x) =
  x .&. Termbox2.Bindings.ModCtrl /= Termbox2.Bindings.Mod 0

shiftd :: Mod -> Bool
shiftd (Mod x) =
  x .&. Termbox2.Bindings.ModShift /= Termbox2.Bindings.Mod 0

data MouseMode
  = MouseModeNo
  | MouseModeYes

data OutputMode
  = OutputModeNormal
  | OutputMode256
  | OutputMode216
  | OutputModeGrayscale
  | OutputModeTruecolor

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
  result <- Termbox2.Bindings.set_input_mode Termbox2.Bindings.InputCurrent
  if
      | coerce result == Termbox2.Bindings.InputEsc -> pure (InputModeEsc MouseModeNo)
      | coerce result == Termbox2.Bindings.InputEsc .|. Termbox2.Bindings.InputMouse -> pure (InputModeEsc MouseModeYes)
      | coerce result == Termbox2.Bindings.InputAlt -> pure (InputModeAlt MouseModeNo)
      | coerce result == Termbox2.Bindings.InputAlt .|. Termbox2.Bindings.InputMouse -> pure (InputModeAlt MouseModeYes)
      | otherwise -> exception "tb_set_input_mode" result

getOutputMode :: IO OutputMode
getOutputMode = do
  result <- Termbox2.Bindings.set_output_mode Termbox2.Bindings.OutputCurrent
  if
      | coerce result == Termbox2.Bindings.OutputNormal -> pure OutputModeNormal
      | coerce result == Termbox2.Bindings.Output256 -> pure OutputMode256
      | coerce result == Termbox2.Bindings.Output216 -> pure OutputMode216
      | coerce result == Termbox2.Bindings.OutputGrayscale -> pure OutputModeGrayscale
      | coerce result == Termbox2.Bindings.OutputTruecolor -> pure OutputModeTruecolor
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
      Termbox2.Bindings.EventChar mod ch -> EventChar (Mod mod) (Char.chr (word32_to_int ch))
      Termbox2.Bindings.EventKey mod key -> EventKey (Mod mod) key
      Termbox2.Bindings.EventResize w h -> EventResize w h
      Termbox2.Bindings.EventMouse key x y -> EventMouse key x y

present :: IO ()
present = do
  result <- Termbox2.Bindings.present
  when (result /= Termbox2.Bindings._OK) (exception "tb_present" result)

print :: Column -> Row -> Word32 -> Word32 -> Text -> IO ()
print x y fg bg str = do
  result <- ByteString.useAsCString (Text.encodeUtf8 str) (Termbox2.Bindings.print (int32_to_cint x) (int32_to_cint y) fg bg)
  when (result /= Termbox2.Bindings._OK) (exception "tb_print" result)

-- set_cell

-- set_clear_attrs

-- set_cursor

setInputMode :: InputMode -> IO ()
setInputMode mode = do
  result <- Termbox2.Bindings.set_input_mode cmode
  when (result /= Termbox2.Bindings._OK) (exception "tb_set_input_mode" result)
  where
    cmode :: Termbox2.Bindings.InputMode
    cmode =
      case mode of
        InputModeEsc MouseModeNo -> Termbox2.Bindings.InputEsc
        InputModeEsc MouseModeYes -> Termbox2.Bindings.InputEsc .|. Termbox2.Bindings.InputMouse
        InputModeAlt MouseModeNo -> Termbox2.Bindings.InputAlt
        InputModeAlt MouseModeYes -> Termbox2.Bindings.InputAlt .|. Termbox2.Bindings.InputMouse

setOutputMode :: OutputMode -> IO ()
setOutputMode mode = do
  result <- Termbox2.Bindings.set_output_mode cmode
  when (result /= Termbox2.Bindings._OK) (exception "tb_set_output_mode" result)
  where
    cmode :: Termbox2.Bindings.OutputMode
    cmode =
      case mode of
        OutputModeNormal -> Termbox2.Bindings.OutputNormal
        OutputMode256 -> Termbox2.Bindings.Output256
        OutputMode216 -> Termbox2.Bindings.Output216
        OutputModeGrayscale -> Termbox2.Bindings.OutputGrayscale
        OutputModeTruecolor -> Termbox2.Bindings.OutputTruecolor

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

int32_to_cint :: Int32 -> CInt
int32_to_cint =
  fromIntegral

word32_to_int :: Word32 -> Int
word32_to_int =
  fromIntegral
