module Termbox2.Bindings
  ( Event (..),
    Key
      ( KeyCtrlTilde,
        KeyCtrl2,
        KeyCtrlA,
        KeyCtrlB,
        KeyCtrlC,
        KeyCtrlD,
        KeyCtrlE,
        KeyCtrlF,
        KeyCtrlG,
        KeyBackspace,
        KeyCtrlH,
        KeyTab,
        KeyCtrlI,
        KeyCtrlJ,
        KeyCtrlK,
        KeyCtrlL,
        KeyEnter,
        KeyCtrlM,
        KeyCtrlN,
        KeyCtrlO,
        KeyCtrlP,
        KeyCtrlQ,
        KeyCtrlR,
        KeyCtrlS,
        KeyCtrlT,
        KeyCtrlU,
        KeyCtrlV,
        KeyCtrlW,
        KeyCtrlX,
        KeyCtrlY,
        KeyCtrlZ,
        KeyEsc,
        KeyCtrlLsqBracket,
        KeyCtrl3,
        KeyCtrl4,
        KeyCtrlBackslash,
        KeyCtrl5,
        KeyCtrlRsqBracket,
        KeyCtrl6,
        KeyCtrl7,
        KeyCtrlSlash,
        KeyCtrlUnderscore,
        KeySpace,
        KeyBackspace2,
        KeyCtrl8,
        KeyF1,
        KeyF2,
        KeyF3,
        KeyF4,
        KeyF5,
        KeyF6,
        KeyF7,
        KeyF8,
        KeyF9,
        KeyF10,
        KeyF11,
        KeyF12,
        KeyInsert,
        KeyDelete,
        KeyHome,
        KeyEnd,
        KeyPgup,
        KeyPgdn,
        KeyArrowUp,
        KeyArrowDown,
        KeyArrowLeft,
        KeyArrowRight,
        KeyBackTab
      ),
    _KEY_MOUSE_LEFT,
    _KEY_MOUSE_RIGHT,
    _KEY_MOUSE_MIDDLE,
    _KEY_MOUSE_RELEASE,
    _KEY_MOUSE_WHEEL_UP,
    _KEY_MOUSE_WHEEL_DOWN,
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

newtype Key = Key Word16
  deriving newtype (Eq, Show)

pattern KeyCtrlTilde :: Key
pattern KeyCtrlTilde <- ((== _KEY_CTRL_TILDE) -> True) where KeyCtrlTilde = _KEY_CTRL_TILDE

pattern KeyCtrl2 :: Key
pattern KeyCtrl2 <- ((== _KEY_CTRL_2) -> True) where KeyCtrl2 = _KEY_CTRL_2

pattern KeyCtrlA :: Key
pattern KeyCtrlA <- ((== _KEY_CTRL_A) -> True) where KeyCtrlA = _KEY_CTRL_A

pattern KeyCtrlB :: Key
pattern KeyCtrlB <- ((== _KEY_CTRL_B) -> True) where KeyCtrlB = _KEY_CTRL_B

pattern KeyCtrlC :: Key
pattern KeyCtrlC <- ((== _KEY_CTRL_C) -> True) where KeyCtrlC = _KEY_CTRL_C

pattern KeyCtrlD :: Key
pattern KeyCtrlD <- ((== _KEY_CTRL_D) -> True) where KeyCtrlD = _KEY_CTRL_D

pattern KeyCtrlE :: Key
pattern KeyCtrlE <- ((== _KEY_CTRL_E) -> True) where KeyCtrlE = _KEY_CTRL_E

pattern KeyCtrlF :: Key
pattern KeyCtrlF <- ((== _KEY_CTRL_F) -> True) where KeyCtrlF = _KEY_CTRL_F

pattern KeyCtrlG :: Key
pattern KeyCtrlG <- ((== _KEY_CTRL_G) -> True) where KeyCtrlG = _KEY_CTRL_G

pattern KeyBackspace :: Key
pattern KeyBackspace <- ((== _KEY_BACKSPACE) -> True) where KeyBackspace = _KEY_BACKSPACE

pattern KeyCtrlH :: Key
pattern KeyCtrlH <- ((== _KEY_CTRL_H) -> True) where KeyCtrlH = _KEY_CTRL_H

pattern KeyTab :: Key
pattern KeyTab <- ((== _KEY_TAB) -> True) where KeyTab = _KEY_TAB

pattern KeyCtrlI :: Key
pattern KeyCtrlI <- ((== _KEY_CTRL_I) -> True) where KeyCtrlI = _KEY_CTRL_I

pattern KeyCtrlJ :: Key
pattern KeyCtrlJ <- ((== _KEY_CTRL_J) -> True) where KeyCtrlJ = _KEY_CTRL_J

pattern KeyCtrlK :: Key
pattern KeyCtrlK <- ((== _KEY_CTRL_K) -> True) where KeyCtrlK = _KEY_CTRL_K

pattern KeyCtrlL :: Key
pattern KeyCtrlL <- ((== _KEY_CTRL_L) -> True) where KeyCtrlL = _KEY_CTRL_L

pattern KeyEnter :: Key
pattern KeyEnter <- ((== _KEY_ENTER) -> True) where KeyEnter = _KEY_ENTER

pattern KeyCtrlM :: Key
pattern KeyCtrlM <- ((== _KEY_CTRL_M) -> True) where KeyCtrlM = _KEY_CTRL_M

pattern KeyCtrlN :: Key
pattern KeyCtrlN <- ((== _KEY_CTRL_N) -> True) where KeyCtrlN = _KEY_CTRL_N

pattern KeyCtrlO :: Key
pattern KeyCtrlO <- ((== _KEY_CTRL_O) -> True) where KeyCtrlO = _KEY_CTRL_O

pattern KeyCtrlP :: Key
pattern KeyCtrlP <- ((== _KEY_CTRL_P) -> True) where KeyCtrlP = _KEY_CTRL_P

pattern KeyCtrlQ :: Key
pattern KeyCtrlQ <- ((== _KEY_CTRL_Q) -> True) where KeyCtrlQ = _KEY_CTRL_Q

pattern KeyCtrlR :: Key
pattern KeyCtrlR <- ((== _KEY_CTRL_R) -> True) where KeyCtrlR = _KEY_CTRL_R

pattern KeyCtrlS :: Key
pattern KeyCtrlS <- ((== _KEY_CTRL_S) -> True) where KeyCtrlS = _KEY_CTRL_S

pattern KeyCtrlT :: Key
pattern KeyCtrlT <- ((== _KEY_CTRL_T) -> True) where KeyCtrlT = _KEY_CTRL_T

pattern KeyCtrlU :: Key
pattern KeyCtrlU <- ((== _KEY_CTRL_U) -> True) where KeyCtrlU = _KEY_CTRL_U

pattern KeyCtrlV :: Key
pattern KeyCtrlV <- ((== _KEY_CTRL_V) -> True) where KeyCtrlV = _KEY_CTRL_V

pattern KeyCtrlW :: Key
pattern KeyCtrlW <- ((== _KEY_CTRL_W) -> True) where KeyCtrlW = _KEY_CTRL_W

pattern KeyCtrlX :: Key
pattern KeyCtrlX <- ((== _KEY_CTRL_X) -> True) where KeyCtrlX = _KEY_CTRL_X

pattern KeyCtrlY :: Key
pattern KeyCtrlY <- ((== _KEY_CTRL_Y) -> True) where KeyCtrlY = _KEY_CTRL_Y

pattern KeyCtrlZ :: Key
pattern KeyCtrlZ <- ((== _KEY_CTRL_Z) -> True) where KeyCtrlZ = _KEY_CTRL_Z

pattern KeyEsc :: Key
pattern KeyEsc <- ((== _KEY_ESC) -> True) where KeyEsc = _KEY_ESC

pattern KeyCtrlLsqBracket :: Key
pattern KeyCtrlLsqBracket <- ((== _KEY_CTRL_LSQ_BRACKET) -> True) where KeyCtrlLsqBracket = _KEY_CTRL_LSQ_BRACKET

pattern KeyCtrl3 :: Key
pattern KeyCtrl3 <- ((== _KEY_CTRL_3) -> True) where KeyCtrl3 = _KEY_CTRL_3

pattern KeyCtrl4 :: Key
pattern KeyCtrl4 <- ((== _KEY_CTRL_4) -> True) where KeyCtrl4 = _KEY_CTRL_4

pattern KeyCtrlBackslash :: Key
pattern KeyCtrlBackslash <- ((== _KEY_CTRL_BACKSLASH) -> True) where KeyCtrlBackslash = _KEY_CTRL_BACKSLASH

pattern KeyCtrl5 :: Key
pattern KeyCtrl5 <- ((== _KEY_CTRL_5) -> True) where KeyCtrl5 = _KEY_CTRL_5

pattern KeyCtrlRsqBracket :: Key
pattern KeyCtrlRsqBracket <- ((== _KEY_CTRL_RSQ_BRACKET) -> True) where KeyCtrlRsqBracket = _KEY_CTRL_RSQ_BRACKET

pattern KeyCtrl6 :: Key
pattern KeyCtrl6 <- ((== _KEY_CTRL_6) -> True) where KeyCtrl6 = _KEY_CTRL_6

pattern KeyCtrl7 :: Key
pattern KeyCtrl7 <- ((== _KEY_CTRL_7) -> True) where KeyCtrl7 = _KEY_CTRL_7

pattern KeyCtrlSlash :: Key
pattern KeyCtrlSlash <- ((== _KEY_CTRL_SLASH) -> True) where KeyCtrlSlash = _KEY_CTRL_SLASH

pattern KeyCtrlUnderscore :: Key
pattern KeyCtrlUnderscore <- ((== _KEY_CTRL_UNDERSCORE) -> True) where KeyCtrlUnderscore = _KEY_CTRL_UNDERSCORE

pattern KeySpace :: Key
pattern KeySpace <- ((== _KEY_SPACE) -> True) where KeySpace = _KEY_SPACE

pattern KeyBackspace2 :: Key
pattern KeyBackspace2 <- ((== _KEY_BACKSPACE2) -> True) where KeyBackspace2 = _KEY_BACKSPACE2

pattern KeyCtrl8 :: Key
pattern KeyCtrl8 <- ((== _KEY_CTRL_8) -> True) where KeyCtrl8 = _KEY_CTRL_8

pattern KeyF1 :: Key
pattern KeyF1 <- ((== _KEY_F1) -> True) where KeyF1 = _KEY_F1

pattern KeyF2 :: Key
pattern KeyF2 <- ((== _KEY_F2) -> True) where KeyF2 = _KEY_F2

pattern KeyF3 :: Key
pattern KeyF3 <- ((== _KEY_F3) -> True) where KeyF3 = _KEY_F3

pattern KeyF4 :: Key
pattern KeyF4 <- ((== _KEY_F4) -> True) where KeyF4 = _KEY_F4

pattern KeyF5 :: Key
pattern KeyF5 <- ((== _KEY_F5) -> True) where KeyF5 = _KEY_F5

pattern KeyF6 :: Key
pattern KeyF6 <- ((== _KEY_F6) -> True) where KeyF6 = _KEY_F6

pattern KeyF7 :: Key
pattern KeyF7 <- ((== _KEY_F7) -> True) where KeyF7 = _KEY_F7

pattern KeyF8 :: Key
pattern KeyF8 <- ((== _KEY_F8) -> True) where KeyF8 = _KEY_F8

pattern KeyF9 :: Key
pattern KeyF9 <- ((== _KEY_F9) -> True) where KeyF9 = _KEY_F9

pattern KeyF10 :: Key
pattern KeyF10 <- ((== _KEY_F10) -> True) where KeyF10 = _KEY_F10

pattern KeyF11 :: Key
pattern KeyF11 <- ((== _KEY_F11) -> True) where KeyF11 = _KEY_F11

pattern KeyF12 :: Key
pattern KeyF12 <- ((== _KEY_F12) -> True) where KeyF12 = _KEY_F12

pattern KeyInsert :: Key
pattern KeyInsert <- ((== _KEY_INSERT) -> True) where KeyInsert = _KEY_INSERT

pattern KeyDelete :: Key
pattern KeyDelete <- ((== _KEY_DELETE) -> True) where KeyDelete = _KEY_DELETE

pattern KeyHome :: Key
pattern KeyHome <- ((== _KEY_HOME) -> True) where KeyHome = _KEY_HOME

pattern KeyEnd :: Key
pattern KeyEnd <- ((== _KEY_END) -> True) where KeyEnd = _KEY_END

pattern KeyPgup :: Key
pattern KeyPgup <- ((== _KEY_PGUP) -> True) where KeyPgup = _KEY_PGUP

pattern KeyPgdn :: Key
pattern KeyPgdn <- ((== _KEY_PGDN) -> True) where KeyPgdn = _KEY_PGDN

pattern KeyArrowUp :: Key
pattern KeyArrowUp <- ((== _KEY_ARROW_UP) -> True) where KeyArrowUp = _KEY_ARROW_UP

pattern KeyArrowDown :: Key
pattern KeyArrowDown <- ((== _KEY_ARROW_DOWN) -> True) where KeyArrowDown = _KEY_ARROW_DOWN

pattern KeyArrowLeft :: Key
pattern KeyArrowLeft <- ((== _KEY_ARROW_LEFT) -> True) where KeyArrowLeft = _KEY_ARROW_LEFT

pattern KeyArrowRight :: Key
pattern KeyArrowRight <- ((== _KEY_ARROW_RIGHT) -> True) where KeyArrowRight = _KEY_ARROW_RIGHT

pattern KeyBackTab :: Key
pattern KeyBackTab <- ((== _KEY_BACK_TAB) -> True) where KeyBackTab = _KEY_BACK_TAB

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

_KEY_MOUSE_LEFT,
  _KEY_MOUSE_RIGHT,
  _KEY_MOUSE_MIDDLE,
  _KEY_MOUSE_RELEASE,
  _KEY_MOUSE_WHEEL_UP,
  _KEY_MOUSE_WHEEL_DOWN ::
    Word16
_KEY_MOUSE_LEFT = 0xffff - 23
_KEY_MOUSE_RIGHT = 0xffff - 24
_KEY_MOUSE_MIDDLE = 0xffff - 25
_KEY_MOUSE_RELEASE = 0xffff - 26
_KEY_MOUSE_WHEEL_UP = 0xffff - 27
_KEY_MOUSE_WHEEL_DOWN = 0xffff - 28

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

data Event
  = -- | mod, key, ch
    EventKey Word8 Word16 Word16
  | -- | w, h
    EventResize Int32 Int32
  | -- | key, x, y
    EventMouse Word16 Int32 Int32

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
            pure (EventKey mod key ch)
        | type_ == _EVENT_RESIZE -> do
            w <- peekByteOff eventPointer 8
            h <- peekByteOff eventPointer 12
            pure (EventResize w h)
        | type_ == _EVENT_MOUSE -> do
            key <- peekByteOff eventPointer 2
            x <- peekByteOff eventPointer 16
            y <- peekByteOff eventPointer 20
            pure (EventMouse key x y)
        | otherwise -> error ("unknown event type: " ++ show type_)

  poke :: Ptr Event -> Event -> IO ()
  poke eventPointer = \case
    -- Event {type_, mod, key, ch, w, h, x, y} = do
    EventKey mod key ch -> do
      pokeByteOff eventPointer 0 _EVENT_KEY
      pokeByteOff eventPointer 1 mod
      pokeByteOff eventPointer 2 key
      pokeByteOff eventPointer 4 ch
    EventResize w h -> do
      pokeByteOff eventPointer 0 _EVENT_RESIZE
      pokeByteOff eventPointer 8 w
      pokeByteOff eventPointer 12 h
    EventMouse key x y -> do
      pokeByteOff eventPointer 0 _EVENT_MOUSE
      pokeByteOff eventPointer 2 key
      pokeByteOff eventPointer 16 x
      pokeByteOff eventPointer 20 y

  sizeOf :: Event -> Int
  sizeOf _ =
    24

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
