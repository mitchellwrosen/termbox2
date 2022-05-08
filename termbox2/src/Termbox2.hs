module Termbox2
  ( debug,
    run,
    Event (..),
    InputMode (..),
    Key
      ( Termbox2.ArrowDown,
        Termbox2.ArrowLeft,
        Termbox2.ArrowRight,
        Termbox2.ArrowUp,
        Termbox2.BackTab,
        Termbox2.Backspace,
        Termbox2.Backspace2,
        Termbox2.Ctrl2,
        Termbox2.Ctrl3,
        Termbox2.Ctrl4,
        Termbox2.Ctrl5,
        Termbox2.Ctrl6,
        Termbox2.Ctrl7,
        Termbox2.Ctrl8,
        Termbox2.CtrlA,
        Termbox2.CtrlB,
        Termbox2.CtrlBackslash,
        Termbox2.CtrlC,
        Termbox2.CtrlD,
        Termbox2.CtrlE,
        Termbox2.CtrlF,
        Termbox2.CtrlG,
        Termbox2.CtrlH,
        Termbox2.CtrlI,
        Termbox2.CtrlJ,
        Termbox2.CtrlK,
        Termbox2.CtrlL,
        Termbox2.CtrlLsqBracket,
        Termbox2.CtrlM,
        Termbox2.CtrlN,
        Termbox2.CtrlO,
        Termbox2.CtrlP,
        Termbox2.CtrlQ,
        Termbox2.CtrlR,
        Termbox2.CtrlRsqBracket,
        Termbox2.CtrlS,
        Termbox2.CtrlSlash,
        Termbox2.CtrlT,
        Termbox2.CtrlTilde,
        Termbox2.CtrlU,
        Termbox2.CtrlUnderscore,
        Termbox2.CtrlV,
        Termbox2.CtrlW,
        Termbox2.CtrlX,
        Termbox2.CtrlY,
        Termbox2.CtrlZ,
        Termbox2.Delete,
        Termbox2.End,
        Termbox2.Enter,
        Termbox2.Esc,
        Termbox2.F1,
        Termbox2.F10,
        Termbox2.F11,
        Termbox2.F12,
        Termbox2.F2,
        Termbox2.F3,
        Termbox2.F4,
        Termbox2.F5,
        Termbox2.F6,
        Termbox2.F7,
        Termbox2.F8,
        Termbox2.F9,
        Termbox2.Home,
        Termbox2.Insert,
        Termbox2.Pgdn,
        Termbox2.Pgup,
        Termbox2.Space,
        Termbox2.Tab
      ),
    Mouse
      ( Termbox2.MouseLeft,
        Termbox2.MouseRight,
        Termbox2.MouseMiddle,
        Termbox2.MouseRelease,
        Termbox2.MouseWheelUp,
        Termbox2.MouseWheelDown
      ),
    Mod,
    alt,
    ctrl,
    shift,
    altd,
    ctrld,
    shiftd,
    MouseMode (..),
    OutputMode (..),
    Scene,
    Size (..),
    --
    Column,
    Row,
    --
    getInputMode,
    getOutputMode,
    hideCursor,
    pollEvent,
    print,
    print2,
    setInputMode,
    setOutputMode,
  )
where

import Control.Exception (Exception, bracket_, throwIO)
import Control.Monad (guard, when)
import Data.Bits ((.&.), (.|.))
import qualified Data.ByteString as ByteString (useAsCString)
import qualified Data.ByteString.Unsafe as ByteString
import Data.Functor (($>))
import Data.Int (Int32)
import qualified Data.List as List
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word32)
import Foreign.C.Types (CInt, CSize)
import Foreign.Marshal.Alloc (alloca)
import qualified Foreign.Storable as Storable
import qualified Termbox2.Bindings as Bindings
import Prelude hiding (init, mod, print)

debug :: IO ()
debug =
  run \_size draw -> do
    setInputMode (InputModeEsc MouseModeYes)
    setOutputMode OutputMode256
    let loop = do
          event <- pollEvent
          if event == EventKey mempty Esc
            then pure ()
            else do
              draw do
                c <- print2 10 10 0 0 "ン"
                print 10 11 0 0 ("^ width = " <> Text.pack (show c))
                print 5 5 0 0 (Text.pack (show event))
              loop
    loop

run :: (Size -> (Scene -> IO ()) -> IO a) -> IO a
run action =
  bracket_ init shutdown do
    size <- getSize
    action size \scene -> do
      clear
      scene
      present
  where
    init :: IO ()
    init = do
      result <- Bindings.init
      when (result /= Bindings.Ok && result /= Bindings.ErrInitAlready) (exception "tb_init" result)

    shutdown :: IO ()
    shutdown =
      binding_ "tb_shutdown" Bindings.shutdown

    getSize :: IO Size
    getSize = do
      height <- binding "tb_height" Bindings.height
      width <- binding "tb_width" Bindings.width
      pure
        Size
          { height = cint_to_int32 height,
            width = cint_to_int32 width
          }

    clear :: IO ()
    clear =
      binding_ "tb_clear" Bindings.clear

    present :: IO ()
    present =
      binding_ "tb_present" Bindings.present

data Event
  = EventChar Mod Char
  | EventKey Mod Key
  | EventResize Size
  | EventMouse Mouse Column Row
  deriving stock (Eq, Show)

data InputMode
  = InputModeEsc MouseMode
  | InputModeAlt MouseMode

newtype Key
  = Key Bindings.Key
  deriving stock (Eq)

pattern ArrowDown :: Key
pattern ArrowDown = Key Bindings.ArrowDown

pattern ArrowLeft :: Key
pattern ArrowLeft = Key Bindings.ArrowLeft

pattern ArrowRight :: Key
pattern ArrowRight = Key Bindings.ArrowRight

pattern ArrowUp :: Key
pattern ArrowUp = Key Bindings.ArrowUp

pattern BackTab :: Key
pattern BackTab = Key Bindings.BackTab

pattern Backspace :: Key
pattern Backspace = Key Bindings.Backspace

pattern Backspace2 :: Key
pattern Backspace2 = Key Bindings.Backspace2

pattern Ctrl2 :: Key
pattern Ctrl2 = Key Bindings.Ctrl2

pattern Ctrl3 :: Key
pattern Ctrl3 = Key Bindings.Ctrl3

pattern Ctrl4 :: Key
pattern Ctrl4 = Key Bindings.Ctrl4

pattern Ctrl5 :: Key
pattern Ctrl5 = Key Bindings.Ctrl5

pattern Ctrl6 :: Key
pattern Ctrl6 = Key Bindings.Ctrl6

pattern Ctrl7 :: Key
pattern Ctrl7 = Key Bindings.Ctrl7

pattern Ctrl8 :: Key
pattern Ctrl8 = Key Bindings.Ctrl8

pattern CtrlA :: Key
pattern CtrlA = Key Bindings.CtrlA

pattern CtrlB :: Key
pattern CtrlB = Key Bindings.CtrlB

pattern CtrlBackslash :: Key
pattern CtrlBackslash = Key Bindings.CtrlBackslash

pattern CtrlC :: Key
pattern CtrlC = Key Bindings.CtrlC

pattern CtrlD :: Key
pattern CtrlD = Key Bindings.CtrlD

pattern CtrlE :: Key
pattern CtrlE = Key Bindings.CtrlE

pattern CtrlF :: Key
pattern CtrlF = Key Bindings.CtrlF

pattern CtrlG :: Key
pattern CtrlG = Key Bindings.CtrlG

pattern CtrlH :: Key
pattern CtrlH = Key Bindings.CtrlH

pattern CtrlI :: Key
pattern CtrlI = Key Bindings.CtrlI

pattern CtrlJ :: Key
pattern CtrlJ = Key Bindings.CtrlJ

pattern CtrlK :: Key
pattern CtrlK = Key Bindings.CtrlK

pattern CtrlL :: Key
pattern CtrlL = Key Bindings.CtrlL

pattern CtrlLsqBracket :: Key
pattern CtrlLsqBracket = Key Bindings.CtrlLsqBracket

pattern CtrlM :: Key
pattern CtrlM = Key Bindings.CtrlM

pattern CtrlN :: Key
pattern CtrlN = Key Bindings.CtrlN

pattern CtrlO :: Key
pattern CtrlO = Key Bindings.CtrlO

pattern CtrlP :: Key
pattern CtrlP = Key Bindings.CtrlP

pattern CtrlQ :: Key
pattern CtrlQ = Key Bindings.CtrlQ

pattern CtrlR :: Key
pattern CtrlR = Key Bindings.CtrlR

pattern CtrlRsqBracket :: Key
pattern CtrlRsqBracket = Key Bindings.CtrlRsqBracket

pattern CtrlS :: Key
pattern CtrlS = Key Bindings.CtrlS

pattern CtrlSlash :: Key
pattern CtrlSlash = Key Bindings.CtrlSlash

pattern CtrlT :: Key
pattern CtrlT = Key Bindings.CtrlT

pattern CtrlTilde :: Key
pattern CtrlTilde = Key Bindings.CtrlTilde

pattern CtrlU :: Key
pattern CtrlU = Key Bindings.CtrlU

pattern CtrlUnderscore :: Key
pattern CtrlUnderscore = Key Bindings.CtrlUnderscore

pattern CtrlV :: Key
pattern CtrlV = Key Bindings.CtrlV

pattern CtrlW :: Key
pattern CtrlW = Key Bindings.CtrlW

pattern CtrlX :: Key
pattern CtrlX = Key Bindings.CtrlX

pattern CtrlY :: Key
pattern CtrlY = Key Bindings.CtrlY

pattern CtrlZ :: Key
pattern CtrlZ = Key Bindings.CtrlZ

pattern Delete :: Key
pattern Delete = Key Bindings.Delete

pattern End :: Key
pattern End = Key Bindings.End

pattern Enter :: Key
pattern Enter = Key Bindings.Enter

pattern Esc :: Key
pattern Esc = Key Bindings.Esc

pattern F1 :: Key
pattern F1 = Key Bindings.F1

pattern F10 :: Key
pattern F10 = Key Bindings.F10

pattern F11 :: Key
pattern F11 = Key Bindings.F11

pattern F12 :: Key
pattern F12 = Key Bindings.F12

pattern F2 :: Key
pattern F2 = Key Bindings.F2

pattern F3 :: Key
pattern F3 = Key Bindings.F3

pattern F4 :: Key
pattern F4 = Key Bindings.F4

pattern F5 :: Key
pattern F5 = Key Bindings.F5

pattern F6 :: Key
pattern F6 = Key Bindings.F6

pattern F7 :: Key
pattern F7 = Key Bindings.F7

pattern F8 :: Key
pattern F8 = Key Bindings.F8

pattern F9 :: Key
pattern F9 = Key Bindings.F9

pattern Home :: Key
pattern Home = Key Bindings.Home

pattern Insert :: Key
pattern Insert = Key Bindings.Insert

pattern Pgdn :: Key
pattern Pgdn = Key Bindings.Pgdn

pattern Pgup :: Key
pattern Pgup = Key Bindings.Pgup

pattern Space :: Key
pattern Space = Key Bindings.Space

pattern Tab :: Key
pattern Tab = Key Bindings.Tab

instance Show Key where
  show = \case
    ArrowDown -> "ArrowDown"
    ArrowLeft -> "ArrowLeft"
    ArrowRight -> "ArrowRight"
    ArrowUp -> "ArrowUp"
    BackTab -> "BackTab"
    Backspace -> "Backspace"
    Backspace2 -> "Backspace2"
    Ctrl4 -> "Ctrl4"
    Ctrl5 -> "Ctrl5"
    Ctrl6 -> "Ctrl6"
    Ctrl7 -> "Ctrl7"
    CtrlA -> "CtrlA"
    CtrlB -> "CtrlB"
    CtrlC -> "CtrlC"
    CtrlD -> "CtrlD"
    CtrlE -> "CtrlE"
    CtrlF -> "CtrlF"
    CtrlG -> "CtrlG"
    CtrlJ -> "CtrlJ"
    CtrlK -> "CtrlK"
    CtrlL -> "CtrlL"
    CtrlN -> "CtrlN"
    CtrlO -> "CtrlO"
    CtrlP -> "CtrlP"
    CtrlQ -> "CtrlQ"
    CtrlR -> "CtrlR"
    CtrlS -> "CtrlS"
    CtrlT -> "CtrlT"
    CtrlTilde -> "CtrlTilde"
    CtrlU -> "CtrlU"
    CtrlV -> "CtrlV"
    CtrlW -> "CtrlW"
    CtrlX -> "CtrlX"
    CtrlY -> "CtrlY"
    CtrlZ -> "CtrlZ"
    Delete -> "Delete"
    End -> "End"
    Enter -> "Enter"
    Esc -> "Esc"
    F1 -> "F1"
    F10 -> "F10"
    F11 -> "F11"
    F12 -> "F12"
    F2 -> "F2"
    F3 -> "F3"
    F4 -> "F4"
    F5 -> "F5"
    F6 -> "F6"
    F7 -> "F7"
    F8 -> "F8"
    F9 -> "F9"
    Home -> "Home"
    Insert -> "Insert"
    Pgdn -> "Pgdn"
    Pgup -> "Pgup"
    Space -> "Space"
    Tab -> "Tab"
    key -> error ("unknown key: " ++ show key)

newtype Mouse
  = Mouse Bindings.Mouse
  deriving stock (Eq)

pattern MouseLeft :: Mouse
pattern MouseLeft = Mouse Bindings.MouseLeft

pattern MouseRight :: Mouse
pattern MouseRight = Mouse Bindings.MouseRight

pattern MouseMiddle :: Mouse
pattern MouseMiddle = Mouse Bindings.MouseMiddle

pattern MouseRelease :: Mouse
pattern MouseRelease = Mouse Bindings.MouseRelease

pattern MouseWheelUp :: Mouse
pattern MouseWheelUp = Mouse Bindings.MouseWheelUp

pattern MouseWheelDown :: Mouse
pattern MouseWheelDown = Mouse Bindings.MouseWheelDown

{-# COMPLETE MouseLeft, MouseRight, MouseMiddle, MouseRelease, MouseWheelUp, MouseWheelDown #-}

instance Show Mouse where
  show = \case
    MouseLeft -> "MouseLeft"
    MouseRight -> "MouseRight"
    MouseMiddle -> "MouseMiddle"
    MouseRelease -> "MouseRelease"
    MouseWheelUp -> "MouseWheelUp"
    MouseWheelDown -> "MouseWheelDown"

-- note: mod Alt is not possible in Esc input mode. hmm...
-- open question: is key Esc possible in Alt input mode?
newtype Mod
  = Mod Bindings.Mod
  deriving stock (Eq)

instance Monoid Mod where
  mempty = Mod 0
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
  Mod Bindings.Alt

ctrl :: Mod
ctrl =
  Mod Bindings.Ctrl

shift :: Mod
shift =
  Mod Bindings.Shift

altd :: Mod -> Bool
altd (Mod x) =
  x .&. Bindings.Alt /= 0

ctrld :: Mod -> Bool
ctrld (Mod x) =
  x .&. Bindings.Ctrl /= 0

shiftd :: Mod -> Bool
shiftd (Mod x) =
  x .&. Bindings.Shift /= 0

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

type Row = Int32

type Scene = IO ()

data Size = Size
  { height :: Int32,
    width :: Int32
  }
  deriving stock (Eq, Show)

getInputMode :: IO InputMode
getInputMode = do
  result <- Bindings.set_input_mode Bindings.InputCurrent
  if
      | result == Bindings.InputEsc -> pure (InputModeEsc MouseModeNo)
      | result == Bindings.InputEsc .|. Bindings.InputMouse -> pure (InputModeEsc MouseModeYes)
      | result == Bindings.InputAlt -> pure (InputModeAlt MouseModeNo)
      | result == Bindings.InputAlt .|. Bindings.InputMouse -> pure (InputModeAlt MouseModeYes)
      | otherwise -> exception "tb_set_input_mode" result

getOutputMode :: IO OutputMode
getOutputMode = do
  result <- Bindings.set_output_mode Bindings.OutputCurrent
  if
      | result == Bindings.OutputNormal -> pure OutputModeNormal
      | result == Bindings.Output256 -> pure OutputMode256
      | result == Bindings.Output216 -> pure OutputMode216
      | result == Bindings.OutputGrayscale -> pure OutputModeGrayscale
      | result == Bindings.OutputTruecolor -> pure OutputModeTruecolor
      | otherwise -> exception "tb_set_output_mode" result

hideCursor :: IO ()
hideCursor =
  binding_ "tb_hide_cursor" Bindings.hide_cursor

-- peek_event

pollEvent :: IO Event
pollEvent =
  fmap parseEvent do
    alloca \eventPointer -> do
      binding_ "tb_poll_event" (Bindings.poll_event eventPointer)
      Storable.peek eventPointer
  where
    parseEvent :: Bindings.Event -> Event
    parseEvent = \case
      Bindings.EventChar mod ch -> EventChar (Mod mod) ch
      Bindings.EventKey mod key -> EventKey (Mod mod) (Key key)
      Bindings.EventResize width height -> EventResize Size {height, width}
      Bindings.EventMouse mouse x y -> EventMouse (Mouse mouse) x y

print :: Column -> Row -> Word32 -> Word32 -> Text -> IO ()
print x y fg bg str =
  binding_ "tb_print" do
    ByteString.useAsCString
      (Text.encodeUtf8 str)
      (Bindings.print (int32_to_cint x) (int32_to_cint y) fg bg)

print2 :: Column -> Row -> Word32 -> Word32 -> Text -> IO Int
print2 x y fg bg str = do
  size <-
    ByteString.useAsCString (Text.encodeUtf8 str) \cstr -> do
      alloca \widthPointer -> do
        binding_ "tb_print_ex" (Bindings.print_ex (int32_to_cint x) (int32_to_cint y) fg bg widthPointer cstr)
        Storable.peek widthPointer
  pure (csize_to_int size)

-- set_cell

-- set_clear_attrs

-- set_cursor

setInputMode :: InputMode -> IO ()
setInputMode mode =
  binding_ "tb_set_input_mode" (Bindings.set_input_mode cmode)
  where
    cmode :: Bindings.InputMode
    cmode =
      case mode of
        InputModeEsc MouseModeNo -> Bindings.InputEsc
        InputModeEsc MouseModeYes -> Bindings.InputEsc .|. Bindings.InputMouse
        InputModeAlt MouseModeNo -> Bindings.InputAlt
        InputModeAlt MouseModeYes -> Bindings.InputAlt .|. Bindings.InputMouse

setOutputMode :: OutputMode -> IO ()
setOutputMode mode =
  binding_ "tb_set_output_mode" (Bindings.set_output_mode cmode)
  where
    cmode :: Bindings.OutputMode
    cmode =
      case mode of
        OutputModeNormal -> Bindings.OutputNormal
        OutputMode256 -> Bindings.Output256
        OutputMode216 -> Bindings.Output216
        OutputModeGrayscale -> Bindings.OutputGrayscale
        OutputModeTruecolor -> Bindings.OutputTruecolor

strerror :: CInt -> IO Text
strerror n = do
  cstring <- Bindings.strerror n
  bytes <- ByteString.unsafePackCString cstring
  pure (Text.decodeUtf8 bytes)

--

data TermboxException = TermboxException
  { function :: Text,
    message :: Text
  }
  deriving stock (Show)
  deriving anyclass (Exception)

binding :: Text -> IO Bindings.Result -> IO CInt
binding name func = do
  result <- func
  when (result < 0) (exception name result)
  pure result

binding_ :: Text -> IO Bindings.Result -> IO ()
binding_ name func = do
  result <- func
  when (result < 0) (exception name result)

exception :: Text -> Bindings.Result -> IO a
exception function code = do
  message <- strerror code
  throwIO TermboxException {function, message}

cint_to_int32 :: CInt -> Int32
cint_to_int32 =
  fromIntegral

csize_to_int :: CSize -> Int
csize_to_int =
  fromIntegral

int32_to_cint :: Int32 -> CInt
int32_to_cint =
  fromIntegral
