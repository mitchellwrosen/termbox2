{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Termbox2 where

import Data.Int (Int32)
import Data.Word (Word16, Word32, Word8)
import Foreign.C.Types
import Foreign.Ptr (Ptr)

--

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
  _KEY_CTRL_8 ::
    Word16
_KEY_CTRL_TILDE = 0x00
_KEY_CTRL_2 = 0x00
_KEY_CTRL_A = 0x01
_KEY_CTRL_B = 0x02
_KEY_CTRL_C = 0x03
_KEY_CTRL_D = 0x04
_KEY_CTRL_E = 0x05
_KEY_CTRL_F = 0x06
_KEY_CTRL_G = 0x07
_KEY_BACKSPACE = 0x08
_KEY_CTRL_H = 0x08
_KEY_TAB = 0x09
_KEY_CTRL_I = 0x09
_KEY_CTRL_J = 0x0a
_KEY_CTRL_K = 0x0b
_KEY_CTRL_L = 0x0c
_KEY_ENTER = 0x0d
_KEY_CTRL_M = 0x0d
_KEY_CTRL_N = 0x0e
_KEY_CTRL_O = 0x0f
_KEY_CTRL_P = 0x10
_KEY_CTRL_Q = 0x11
_KEY_CTRL_R = 0x12
_KEY_CTRL_S = 0x13
_KEY_CTRL_T = 0x14
_KEY_CTRL_U = 0x15
_KEY_CTRL_V = 0x16
_KEY_CTRL_W = 0x17
_KEY_CTRL_X = 0x18
_KEY_CTRL_Y = 0x19
_KEY_CTRL_Z = 0x1a
_KEY_ESC = 0x1b
_KEY_CTRL_LSQ_BRACKET = 0x1b
_KEY_CTRL_3 = 0x1b
_KEY_CTRL_4 = 0x1c
_KEY_CTRL_BACKSLASH = 0x1c
_KEY_CTRL_5 = 0x1d
_KEY_CTRL_RSQ_BRACKET = 0x1d
_KEY_CTRL_6 = 0x1e
_KEY_CTRL_7 = 0x1f
_KEY_CTRL_SLASH = 0x1f
_KEY_CTRL_UNDERSCORE = 0x1f
_KEY_SPACE = 0x20
_KEY_BACKSPACE2 = 0x7f
_KEY_CTRL_8 = 0x7f

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
  _KEY_BACK_TAB,
  _KEY_MOUSE_LEFT,
  _KEY_MOUSE_RIGHT,
  _KEY_MOUSE_MIDDLE,
  _KEY_MOUSE_RELEASE,
  _KEY_MOUSE_WHEEL_UP,
  _KEY_MOUSE_WHEEL_DOWN ::
    Word16
_KEY_F1 = 0xffff - 0
_KEY_F2 = 0xffff - 1
_KEY_F3 = 0xffff - 2
_KEY_F4 = 0xffff - 3
_KEY_F5 = 0xffff - 4
_KEY_F6 = 0xffff - 5
_KEY_F7 = 0xffff - 6
_KEY_F8 = 0xffff - 7
_KEY_F9 = 0xffff - 8
_KEY_F10 = 0xffff - 9
_KEY_F11 = 0xffff - 10
_KEY_F12 = 0xffff - 11
_KEY_INSERT = 0xffff - 12
_KEY_DELETE = 0xffff - 13
_KEY_HOME = 0xffff - 14
_KEY_END = 0xffff - 15
_KEY_PGUP = 0xffff - 16
_KEY_PGDN = 0xffff - 17
_KEY_ARROW_UP = 0xffff - 18
_KEY_ARROW_DOWN = 0xffff - 19
_KEY_ARROW_LEFT = 0xffff - 20
_KEY_ARROW_RIGHT = 0xffff - 21
_KEY_BACK_TAB = 0xffff - 22
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

data Event = Event
  { type_ :: Word8,
    mod :: Word8,
    key :: Word16,
    ch :: Word32,
    w :: Int32,
    h :: Int32,
    x :: Int32,
    y :: Int32
  }

foreign import ccall unsafe "tb_clear"
  clear :: IO CInt

foreign import ccall unsafe "tb_height"
  height :: IO CInt

foreign import ccall unsafe "tb_hide_cursor"
  hide_cursor :: IO CInt

foreign import ccall unsafe "tb_init"
  init :: IO CInt

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

foreign import ccall unsafe "tb_width"
  width :: IO CInt
