module Termbox2.Bindings
  ( Event (..),
    Key
      ( CtrlTilde,
        Ctrl2,
        CtrlA,
        CtrlB,
        CtrlC,
        CtrlD,
        CtrlE,
        CtrlF,
        CtrlG,
        Backspace,
        CtrlH,
        Tab,
        CtrlI,
        CtrlJ,
        CtrlK,
        CtrlL,
        Enter,
        CtrlM,
        CtrlN,
        CtrlO,
        CtrlP,
        CtrlQ,
        CtrlR,
        CtrlS,
        CtrlT,
        CtrlU,
        CtrlV,
        CtrlW,
        CtrlX,
        CtrlY,
        CtrlZ,
        Esc,
        CtrlLsqBracket,
        Ctrl3,
        Ctrl4,
        CtrlBackslash,
        Ctrl5,
        CtrlRsqBracket,
        Ctrl6,
        Ctrl7,
        CtrlSlash,
        CtrlUnderscore,
        Space,
        Backspace2,
        Ctrl8,
        F1,
        F2,
        F3,
        F4,
        F5,
        F6,
        F7,
        F8,
        F9,
        F10,
        F11,
        F12,
        Insert,
        Delete,
        Home,
        End,
        Pgup,
        Pgdn,
        ArrowUp,
        ArrowDown,
        ArrowLeft,
        ArrowRight,
        BackTab
      ),
    Mouse (MouseLeft, MouseRight, MouseMiddle, MouseRelease, MouseWheelUp, MouseWheelDown),
    _DEFAULT,
    _BLACK,
    _RED,
    _GREEN,
    _YELLOW,
    _BLUE,
    _MAGENTA,
    _CYAN,
    _WHITE,
    _BOLD,
    _UNDERLINE,
    _REVERSE,
    _ITALIC,
    _EVENT_KEY,
    _EVENT_RESIZE,
    _EVENT_MOUSE,
    _MOD_ALT,
    _MOD_CTRL,
    _MOD_SHIFT,
    _MOD_MOTION,
    _INPUT_CURRENT,
    _INPUT_ESC,
    _INPUT_ALT,
    _INPUT_MOUSE,
    _OUTPUT_CURRENT,
    _OUTPUT_NORMAL,
    _OUTPUT_256,
    _OUTPUT_216,
    _OUTPUT_GRAYSCALE,
    _OUTPUT_TRUECOLOR,
    _OK,
    _ERR,
    _ERR_NEED_MORE,
    _ERR_INIT_ALREADY,
    _ERR_INIT_OPEN,
    _ERR_MEM,
    _ERR_NO_EVENT,
    _ERR_NO_TERM,
    _ERR_NOT_INIT,
    _ERR_OUT_OF_BOUNDS,
    _ERR_READ,
    _ERR_RESIZE_IOCTL,
    _ERR_RESIZE_PIPE,
    _ERR_RESIZE_SIGACTION,
    _ERR_POLL,
    _ERR_TCGETATTR,
    _ERR_TCSETATTR,
    _ERR_UNSUPPORTED_TERM,
    _ERR_RESIZE_WRITE,
    _ERR_RESIZE_POLL,
    _ERR_RESIZE_READ,
    _ERR_RESIZE_SSCANF,
    _ERR_CAP_COLLISION,
    _ERR_SELECT,
    _ERR_RESIZE_SELECT,
    _FUNC_EXTRACT_PRE,
    _FUNC_EXTRACT_POST,
    clear,
    height,
    hide_cursor,
    init,
    peek_event,
    poll_event,
    present,
    print,
    set_cell,
    set_clear_attrs,
    set_cursor,
    set_input_mode,
    set_output_mode,
    shutdown,
    strerror,
    width,
  )
where

import Data.Int (Int32)
import Data.Word (Word16, Word32, Word8)
import Foreign.C.Types
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))
import Prelude hiding (init, mod, print)

------------------------------------------------------------------------------------------------------------------------
-- Event

data Event
  = -- | mod, key, ch
    EventKey Word8 Key Word16
  | -- | w, h
    EventResize Int32 Int32
  | -- | key, x, y
    EventMouse Mouse Int32 Int32

instance Storable Event where
  alignment :: Event -> Int
  alignment _ =
    4

  peek :: Ptr Event -> IO Event
  peek eventPointer = do
    type_ <- peekByteOff eventPointer 0
    if
        | type_ == _EVENT_KEY -> do
            mod <- peekByteOff eventPointer 1
            key <- peekByteOff eventPointer 2
            ch <- peekByteOff eventPointer 4
            pure (EventKey mod (Key key) ch)
        | type_ == _EVENT_RESIZE -> do
            w <- peekByteOff eventPointer 8
            h <- peekByteOff eventPointer 12
            pure (EventResize w h)
        | type_ == _EVENT_MOUSE -> do
            key <- peekByteOff eventPointer 2
            x <- peekByteOff eventPointer 16
            y <- peekByteOff eventPointer 20
            pure (EventMouse (Mouse key) x y)
        | otherwise -> error ("unknown event type: " ++ show type_)

  poke :: Ptr Event -> Event -> IO ()
  poke eventPointer = \case
    EventKey mod (Key key) ch -> do
      pokeByteOff eventPointer 0 _EVENT_KEY
      pokeByteOff eventPointer 1 mod
      pokeByteOff eventPointer 2 key
      pokeByteOff eventPointer 4 ch
    EventResize w h -> do
      pokeByteOff eventPointer 0 _EVENT_RESIZE
      pokeByteOff eventPointer 8 w
      pokeByteOff eventPointer 12 h
    EventMouse (Mouse key) x y -> do
      pokeByteOff eventPointer 0 _EVENT_MOUSE
      pokeByteOff eventPointer 2 key
      pokeByteOff eventPointer 16 x
      pokeByteOff eventPointer 20 y

  sizeOf :: Event -> Int
  sizeOf _ =
    24

------------------------------------------------------------------------------------------------------------------------
-- Key

newtype Key = Key Word16
  deriving newtype (Eq, Show)

pattern CtrlTilde :: Key
pattern CtrlTilde <- ((== _KEY_CTRL_TILDE) -> True) where CtrlTilde = _KEY_CTRL_TILDE

pattern Ctrl2 :: Key
pattern Ctrl2 <- ((== _KEY_CTRL_2) -> True) where Ctrl2 = _KEY_CTRL_2

pattern CtrlA :: Key
pattern CtrlA <- ((== _KEY_CTRL_A) -> True) where CtrlA = _KEY_CTRL_A

pattern CtrlB :: Key
pattern CtrlB <- ((== _KEY_CTRL_B) -> True) where CtrlB = _KEY_CTRL_B

pattern CtrlC :: Key
pattern CtrlC <- ((== _KEY_CTRL_C) -> True) where CtrlC = _KEY_CTRL_C

pattern CtrlD :: Key
pattern CtrlD <- ((== _KEY_CTRL_D) -> True) where CtrlD = _KEY_CTRL_D

pattern CtrlE :: Key
pattern CtrlE <- ((== _KEY_CTRL_E) -> True) where CtrlE = _KEY_CTRL_E

pattern CtrlF :: Key
pattern CtrlF <- ((== _KEY_CTRL_F) -> True) where CtrlF = _KEY_CTRL_F

pattern CtrlG :: Key
pattern CtrlG <- ((== _KEY_CTRL_G) -> True) where CtrlG = _KEY_CTRL_G

pattern Backspace :: Key
pattern Backspace <- ((== _KEY_BACKSPACE) -> True) where Backspace = _KEY_BACKSPACE

pattern CtrlH :: Key
pattern CtrlH <- ((== _KEY_CTRL_H) -> True) where CtrlH = _KEY_CTRL_H

pattern Tab :: Key
pattern Tab <- ((== _KEY_TAB) -> True) where Tab = _KEY_TAB

pattern CtrlI :: Key
pattern CtrlI <- ((== _KEY_CTRL_I) -> True) where CtrlI = _KEY_CTRL_I

pattern CtrlJ :: Key
pattern CtrlJ <- ((== _KEY_CTRL_J) -> True) where CtrlJ = _KEY_CTRL_J

pattern CtrlK :: Key
pattern CtrlK <- ((== _KEY_CTRL_K) -> True) where CtrlK = _KEY_CTRL_K

pattern CtrlL :: Key
pattern CtrlL <- ((== _KEY_CTRL_L) -> True) where CtrlL = _KEY_CTRL_L

pattern Enter :: Key
pattern Enter <- ((== _KEY_ENTER) -> True) where Enter = _KEY_ENTER

pattern CtrlM :: Key
pattern CtrlM <- ((== _KEY_CTRL_M) -> True) where CtrlM = _KEY_CTRL_M

pattern CtrlN :: Key
pattern CtrlN <- ((== _KEY_CTRL_N) -> True) where CtrlN = _KEY_CTRL_N

pattern CtrlO :: Key
pattern CtrlO <- ((== _KEY_CTRL_O) -> True) where CtrlO = _KEY_CTRL_O

pattern CtrlP :: Key
pattern CtrlP <- ((== _KEY_CTRL_P) -> True) where CtrlP = _KEY_CTRL_P

pattern CtrlQ :: Key
pattern CtrlQ <- ((== _KEY_CTRL_Q) -> True) where CtrlQ = _KEY_CTRL_Q

pattern CtrlR :: Key
pattern CtrlR <- ((== _KEY_CTRL_R) -> True) where CtrlR = _KEY_CTRL_R

pattern CtrlS :: Key
pattern CtrlS <- ((== _KEY_CTRL_S) -> True) where CtrlS = _KEY_CTRL_S

pattern CtrlT :: Key
pattern CtrlT <- ((== _KEY_CTRL_T) -> True) where CtrlT = _KEY_CTRL_T

pattern CtrlU :: Key
pattern CtrlU <- ((== _KEY_CTRL_U) -> True) where CtrlU = _KEY_CTRL_U

pattern CtrlV :: Key
pattern CtrlV <- ((== _KEY_CTRL_V) -> True) where CtrlV = _KEY_CTRL_V

pattern CtrlW :: Key
pattern CtrlW <- ((== _KEY_CTRL_W) -> True) where CtrlW = _KEY_CTRL_W

pattern CtrlX :: Key
pattern CtrlX <- ((== _KEY_CTRL_X) -> True) where CtrlX = _KEY_CTRL_X

pattern CtrlY :: Key
pattern CtrlY <- ((== _KEY_CTRL_Y) -> True) where CtrlY = _KEY_CTRL_Y

pattern CtrlZ :: Key
pattern CtrlZ <- ((== _KEY_CTRL_Z) -> True) where CtrlZ = _KEY_CTRL_Z

pattern Esc :: Key
pattern Esc <- ((== _KEY_ESC) -> True) where Esc = _KEY_ESC

pattern CtrlLsqBracket :: Key
pattern CtrlLsqBracket <- ((== _KEY_CTRL_LSQ_BRACKET) -> True) where CtrlLsqBracket = _KEY_CTRL_LSQ_BRACKET

pattern Ctrl3 :: Key
pattern Ctrl3 <- ((== _KEY_CTRL_3) -> True) where Ctrl3 = _KEY_CTRL_3

pattern Ctrl4 :: Key
pattern Ctrl4 <- ((== _KEY_CTRL_4) -> True) where Ctrl4 = _KEY_CTRL_4

pattern CtrlBackslash :: Key
pattern CtrlBackslash <- ((== _KEY_CTRL_BACKSLASH) -> True) where CtrlBackslash = _KEY_CTRL_BACKSLASH

pattern Ctrl5 :: Key
pattern Ctrl5 <- ((== _KEY_CTRL_5) -> True) where Ctrl5 = _KEY_CTRL_5

pattern CtrlRsqBracket :: Key
pattern CtrlRsqBracket <- ((== _KEY_CTRL_RSQ_BRACKET) -> True) where CtrlRsqBracket = _KEY_CTRL_RSQ_BRACKET

pattern Ctrl6 :: Key
pattern Ctrl6 <- ((== _KEY_CTRL_6) -> True) where Ctrl6 = _KEY_CTRL_6

pattern Ctrl7 :: Key
pattern Ctrl7 <- ((== _KEY_CTRL_7) -> True) where Ctrl7 = _KEY_CTRL_7

pattern CtrlSlash :: Key
pattern CtrlSlash <- ((== _KEY_CTRL_SLASH) -> True) where CtrlSlash = _KEY_CTRL_SLASH

pattern CtrlUnderscore :: Key
pattern CtrlUnderscore <- ((== _KEY_CTRL_UNDERSCORE) -> True) where CtrlUnderscore = _KEY_CTRL_UNDERSCORE

pattern Space :: Key
pattern Space <- ((== _KEY_SPACE) -> True) where Space = _KEY_SPACE

pattern Backspace2 :: Key
pattern Backspace2 <- ((== _KEY_BACKSPACE2) -> True) where Backspace2 = _KEY_BACKSPACE2

pattern Ctrl8 :: Key
pattern Ctrl8 <- ((== _KEY_CTRL_8) -> True) where Ctrl8 = _KEY_CTRL_8

pattern F1 :: Key
pattern F1 <- ((== _KEY_F1) -> True) where F1 = _KEY_F1

pattern F2 :: Key
pattern F2 <- ((== _KEY_F2) -> True) where F2 = _KEY_F2

pattern F3 :: Key
pattern F3 <- ((== _KEY_F3) -> True) where F3 = _KEY_F3

pattern F4 :: Key
pattern F4 <- ((== _KEY_F4) -> True) where F4 = _KEY_F4

pattern F5 :: Key
pattern F5 <- ((== _KEY_F5) -> True) where F5 = _KEY_F5

pattern F6 :: Key
pattern F6 <- ((== _KEY_F6) -> True) where F6 = _KEY_F6

pattern F7 :: Key
pattern F7 <- ((== _KEY_F7) -> True) where F7 = _KEY_F7

pattern F8 :: Key
pattern F8 <- ((== _KEY_F8) -> True) where F8 = _KEY_F8

pattern F9 :: Key
pattern F9 <- ((== _KEY_F9) -> True) where F9 = _KEY_F9

pattern F10 :: Key
pattern F10 <- ((== _KEY_F10) -> True) where F10 = _KEY_F10

pattern F11 :: Key
pattern F11 <- ((== _KEY_F11) -> True) where F11 = _KEY_F11

pattern F12 :: Key
pattern F12 <- ((== _KEY_F12) -> True) where F12 = _KEY_F12

pattern Insert :: Key
pattern Insert <- ((== _KEY_INSERT) -> True) where Insert = _KEY_INSERT

pattern Delete :: Key
pattern Delete <- ((== _KEY_DELETE) -> True) where Delete = _KEY_DELETE

pattern Home :: Key
pattern Home <- ((== _KEY_HOME) -> True) where Home = _KEY_HOME

pattern End :: Key
pattern End <- ((== _KEY_END) -> True) where End = _KEY_END

pattern Pgup :: Key
pattern Pgup <- ((== _KEY_PGUP) -> True) where Pgup = _KEY_PGUP

pattern Pgdn :: Key
pattern Pgdn <- ((== _KEY_PGDN) -> True) where Pgdn = _KEY_PGDN

pattern ArrowUp :: Key
pattern ArrowUp <- ((== _KEY_ARROW_UP) -> True) where ArrowUp = _KEY_ARROW_UP

pattern ArrowDown :: Key
pattern ArrowDown <- ((== _KEY_ARROW_DOWN) -> True) where ArrowDown = _KEY_ARROW_DOWN

pattern ArrowLeft :: Key
pattern ArrowLeft <- ((== _KEY_ARROW_LEFT) -> True) where ArrowLeft = _KEY_ARROW_LEFT

pattern ArrowRight :: Key
pattern ArrowRight <- ((== _KEY_ARROW_RIGHT) -> True) where ArrowRight = _KEY_ARROW_RIGHT

pattern BackTab :: Key
pattern BackTab <- ((== _KEY_BACK_TAB) -> True) where BackTab = _KEY_BACK_TAB

_KEY_CTRL_TILDE,
  _KEY_CTRL_2,
  _KEY_CTRL_A,
  _KEY_CTRL_B,
  _KEY_CTRL_C,
  _KEY_CTRL_D,
  _KEY_CTRL_E,
  _KEY_CTRL_F,
  _KEY_CTRL_G,
  _KEY_BACKSPACE,
  _KEY_CTRL_H,
  _KEY_TAB,
  _KEY_CTRL_I,
  _KEY_CTRL_J,
  _KEY_CTRL_K,
  _KEY_CTRL_L,
  _KEY_ENTER,
  _KEY_CTRL_M,
  _KEY_CTRL_N,
  _KEY_CTRL_O,
  _KEY_CTRL_P,
  _KEY_CTRL_Q,
  _KEY_CTRL_R,
  _KEY_CTRL_S,
  _KEY_CTRL_T,
  _KEY_CTRL_U,
  _KEY_CTRL_V,
  _KEY_CTRL_W,
  _KEY_CTRL_X,
  _KEY_CTRL_Y,
  _KEY_CTRL_Z,
  _KEY_ESC,
  _KEY_CTRL_LSQ_BRACKET,
  _KEY_CTRL_3,
  _KEY_CTRL_4,
  _KEY_CTRL_BACKSLASH,
  _KEY_CTRL_5,
  _KEY_CTRL_RSQ_BRACKET,
  _KEY_CTRL_6,
  _KEY_CTRL_7,
  _KEY_CTRL_SLASH,
  _KEY_CTRL_UNDERSCORE,
  _KEY_SPACE,
  _KEY_BACKSPACE2,
  _KEY_CTRL_8,
  _KEY_F1,
  _KEY_F2,
  _KEY_F3,
  _KEY_F4,
  _KEY_F5,
  _KEY_F6,
  _KEY_F7,
  _KEY_F8,
  _KEY_F9,
  _KEY_F10,
  _KEY_F11,
  _KEY_F12,
  _KEY_INSERT,
  _KEY_DELETE,
  _KEY_HOME,
  _KEY_END,
  _KEY_PGUP,
  _KEY_PGDN,
  _KEY_ARROW_UP,
  _KEY_ARROW_DOWN,
  _KEY_ARROW_LEFT,
  _KEY_ARROW_RIGHT,
  _KEY_BACK_TAB ::
    Key
_KEY_CTRL_TILDE = Key 0x00
_KEY_CTRL_2 = Key 0x00
_KEY_CTRL_A = Key 0x01
_KEY_CTRL_B = Key 0x02
_KEY_CTRL_C = Key 0x03
_KEY_CTRL_D = Key 0x04
_KEY_CTRL_E = Key 0x05
_KEY_CTRL_F = Key 0x06
_KEY_CTRL_G = Key 0x07
_KEY_BACKSPACE = Key 0x08
_KEY_CTRL_H = Key 0x08
_KEY_TAB = Key 0x09
_KEY_CTRL_I = Key 0x09
_KEY_CTRL_J = Key 0x0a
_KEY_CTRL_K = Key 0x0b
_KEY_CTRL_L = Key 0x0c
_KEY_ENTER = Key 0x0d
_KEY_CTRL_M = Key 0x0d
_KEY_CTRL_N = Key 0x0e
_KEY_CTRL_O = Key 0x0f
_KEY_CTRL_P = Key 0x10
_KEY_CTRL_Q = Key 0x11
_KEY_CTRL_R = Key 0x12
_KEY_CTRL_S = Key 0x13
_KEY_CTRL_T = Key 0x14
_KEY_CTRL_U = Key 0x15
_KEY_CTRL_V = Key 0x16
_KEY_CTRL_W = Key 0x17
_KEY_CTRL_X = Key 0x18
_KEY_CTRL_Y = Key 0x19
_KEY_CTRL_Z = Key 0x1a
_KEY_ESC = Key 0x1b
_KEY_CTRL_LSQ_BRACKET = Key 0x1b
_KEY_CTRL_3 = Key 0x1b
_KEY_CTRL_4 = Key 0x1c
_KEY_CTRL_BACKSLASH = Key 0x1c
_KEY_CTRL_5 = Key 0x1d
_KEY_CTRL_RSQ_BRACKET = Key 0x1d
_KEY_CTRL_6 = Key 0x1e
_KEY_CTRL_7 = Key 0x1f
_KEY_CTRL_SLASH = Key 0x1f
_KEY_CTRL_UNDERSCORE = Key 0x1f
_KEY_SPACE = Key 0x20
_KEY_BACKSPACE2 = Key 0x7f
_KEY_CTRL_8 = Key 0x7f
_KEY_F1 = Key (0xffff - 0)
_KEY_F2 = Key (0xffff - 1)
_KEY_F3 = Key (0xffff - 2)
_KEY_F4 = Key (0xffff - 3)
_KEY_F5 = Key (0xffff - 4)
_KEY_F6 = Key (0xffff - 5)
_KEY_F7 = Key (0xffff - 6)
_KEY_F8 = Key (0xffff - 7)
_KEY_F9 = Key (0xffff - 8)
_KEY_F10 = Key (0xffff - 9)
_KEY_F11 = Key (0xffff - 10)
_KEY_F12 = Key (0xffff - 11)
_KEY_INSERT = Key (0xffff - 12)
_KEY_DELETE = Key (0xffff - 13)
_KEY_HOME = Key (0xffff - 14)
_KEY_END = Key (0xffff - 15)
_KEY_PGUP = Key (0xffff - 16)
_KEY_PGDN = Key (0xffff - 17)
_KEY_ARROW_UP = Key (0xffff - 18)
_KEY_ARROW_DOWN = Key (0xffff - 19)
_KEY_ARROW_LEFT = Key (0xffff - 20)
_KEY_ARROW_RIGHT = Key (0xffff - 21)
_KEY_BACK_TAB = Key (0xffff - 22)

------------------------------------------------------------------------------------------------------------------------
-- Mouse

newtype Mouse = Mouse Word16
  deriving newtype (Eq)

pattern MouseLeft :: Mouse
pattern MouseLeft <- ((== _KEY_MOUSE_LEFT) -> True) where MouseLeft = _KEY_MOUSE_LEFT

pattern MouseRight :: Mouse
pattern MouseRight <- ((== _KEY_MOUSE_RIGHT) -> True) where MouseRight = _KEY_MOUSE_RIGHT

pattern MouseMiddle :: Mouse
pattern MouseMiddle <- ((== _KEY_MOUSE_MIDDLE) -> True) where MouseMiddle = _KEY_MOUSE_MIDDLE

pattern MouseRelease :: Mouse
pattern MouseRelease <- ((== _KEY_MOUSE_RELEASE) -> True) where MouseRelease = _KEY_MOUSE_RELEASE

pattern MouseWheelUp :: Mouse
pattern MouseWheelUp <- ((== _KEY_MOUSE_WHEEL_UP) -> True) where MouseWheelUp = _KEY_MOUSE_WHEEL_UP

pattern MouseWheelDown :: Mouse
pattern MouseWheelDown <- ((== _KEY_MOUSE_WHEEL_DOWN) -> True) where MouseWheelDown = _KEY_MOUSE_WHEEL_DOWN

{-# COMPLETE MouseLeft, MouseRight, MouseMiddle, MouseRelease, MouseWheelUp, MouseWheelDown #-}

_KEY_MOUSE_LEFT,
  _KEY_MOUSE_RIGHT,
  _KEY_MOUSE_MIDDLE,
  _KEY_MOUSE_RELEASE,
  _KEY_MOUSE_WHEEL_UP,
  _KEY_MOUSE_WHEEL_DOWN ::
    Mouse
_KEY_MOUSE_LEFT = Mouse (0xffff - 23)
_KEY_MOUSE_RIGHT = Mouse (0xffff - 24)
_KEY_MOUSE_MIDDLE = Mouse (0xffff - 25)
_KEY_MOUSE_RELEASE = Mouse (0xffff - 26)
_KEY_MOUSE_WHEEL_UP = Mouse (0xffff - 27)
_KEY_MOUSE_WHEEL_DOWN = Mouse (0xffff - 28)

_DEFAULT,
  _BLACK,
  _RED,
  _GREEN,
  _YELLOW,
  _BLUE,
  _MAGENTA,
  _CYAN,
  _WHITE,
  _BOLD,
  _UNDERLINE,
  _REVERSE,
  _ITALIC ::
    Word32
_DEFAULT = 0x0000
_BLACK = 0x0001
_RED = 0x0002
_GREEN = 0x0003
_YELLOW = 0x0004
_BLUE = 0x0005
_MAGENTA = 0x0006
_CYAN = 0x0007
_WHITE = 0x0008
_BOLD = 0x0100
_UNDERLINE = 0x0200
_REVERSE = 0x0400
_ITALIC = 0x0800

_EVENT_KEY, _EVENT_RESIZE, _EVENT_MOUSE :: Word8
_EVENT_KEY = 1
_EVENT_RESIZE = 2
_EVENT_MOUSE = 3

_MOD_ALT, _MOD_CTRL, _MOD_SHIFT, _MOD_MOTION :: Word8
_MOD_ALT = 1
_MOD_CTRL = 2
_MOD_SHIFT = 4
_MOD_MOTION = 8

_INPUT_CURRENT, _INPUT_ESC, _INPUT_ALT, _INPUT_MOUSE :: CInt
_INPUT_CURRENT = 0
_INPUT_ESC = 1
_INPUT_ALT = 2
_INPUT_MOUSE = 4

_OUTPUT_CURRENT, _OUTPUT_NORMAL, _OUTPUT_256, _OUTPUT_216, _OUTPUT_GRAYSCALE, _OUTPUT_TRUECOLOR :: CInt
_OUTPUT_CURRENT = 0
_OUTPUT_NORMAL = 1
_OUTPUT_256 = 2
_OUTPUT_216 = 3
_OUTPUT_GRAYSCALE = 4
_OUTPUT_TRUECOLOR = 5

_OK,
  _ERR,
  _ERR_NEED_MORE,
  _ERR_INIT_ALREADY,
  _ERR_INIT_OPEN,
  _ERR_MEM,
  _ERR_NO_EVENT,
  _ERR_NO_TERM,
  _ERR_NOT_INIT,
  _ERR_OUT_OF_BOUNDS,
  _ERR_READ,
  _ERR_RESIZE_IOCTL,
  _ERR_RESIZE_PIPE,
  _ERR_RESIZE_SIGACTION,
  _ERR_POLL,
  _ERR_TCGETATTR,
  _ERR_TCSETATTR,
  _ERR_UNSUPPORTED_TERM,
  _ERR_RESIZE_WRITE,
  _ERR_RESIZE_POLL,
  _ERR_RESIZE_READ,
  _ERR_RESIZE_SSCANF,
  _ERR_CAP_COLLISION ::
    CInt
_OK = 0
_ERR = -1
_ERR_NEED_MORE = -2
_ERR_INIT_ALREADY = -3
_ERR_INIT_OPEN = -4
_ERR_MEM = -5
_ERR_NO_EVENT = -6
_ERR_NO_TERM = -7
_ERR_NOT_INIT = -8
_ERR_OUT_OF_BOUNDS = -9
_ERR_READ = -10
_ERR_RESIZE_IOCTL = -11
_ERR_RESIZE_PIPE = -12
_ERR_RESIZE_SIGACTION = -13
_ERR_POLL = -14
_ERR_TCGETATTR = -15
_ERR_TCSETATTR = -16
_ERR_UNSUPPORTED_TERM = -17
_ERR_RESIZE_WRITE = -18
_ERR_RESIZE_POLL = -19
_ERR_RESIZE_READ = -20
_ERR_RESIZE_SSCANF = -21
_ERR_CAP_COLLISION = -22

_ERR_SELECT, _ERR_RESIZE_SELECT :: CInt
_ERR_SELECT = _ERR_POLL
_ERR_RESIZE_SELECT = _ERR_RESIZE_POLL

_FUNC_EXTRACT_PRE, _FUNC_EXTRACT_POST :: CInt
_FUNC_EXTRACT_PRE = 0
_FUNC_EXTRACT_POST = 1

------------------------------------------------------------------------------------------------------------------------
-- Bindings

foreign import ccall unsafe "tb_clear"
  clear :: IO CInt

foreign import ccall unsafe "tb_height"
  height :: IO CInt

foreign import ccall unsafe "tb_hide_cursor"
  hide_cursor :: IO CInt

foreign import ccall unsafe "tb_init"
  init :: IO CInt

-- foreign import ccall unsafe "tb_last_errno"
--   last_errno :: IO CInt

foreign import ccall safe "tb_peek_event"
  peek_event :: Ptr Event -> CInt -> IO CInt

foreign import ccall safe "tb_poll_event"
  poll_event :: Ptr Event -> IO CInt

foreign import ccall unsafe "tb_present"
  present :: IO CInt

foreign import ccall unsafe "tb_print"
  print :: CInt -> CInt -> Word32 -> Word32 -> Ptr CChar -> IO CInt

foreign import ccall unsafe "tb_set_cell"
  set_cell :: CInt -> CInt -> Word32 -> Word32 -> Word32 -> IO CInt

foreign import ccall unsafe "tb_set_clear_attrs"
  set_clear_attrs :: Word32 -> Word32 -> IO CInt

foreign import ccall unsafe "tb_set_cursor"
  set_cursor :: CInt -> CInt -> IO CInt

foreign import ccall unsafe "tb_set_input_mode"
  set_input_mode :: CInt -> IO CInt

foreign import ccall unsafe "tb_set_output_mode"
  set_output_mode :: CInt -> IO CInt

foreign import ccall unsafe "tb_shutdown"
  shutdown :: IO CInt

foreign import ccall unsafe "tb_strerror"
  strerror :: CInt -> IO (Ptr CChar)

foreign import ccall unsafe "tb_width"
  width :: IO CInt
