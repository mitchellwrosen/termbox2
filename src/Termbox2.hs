{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Termbox2 where

import Data.Word (Word16, Word32, Word8)

--

_TB_KEY_CTRL_TILDE,
  _TB_KEY_CTRL_2,
  _TB_KEY_CTRL_A,
  _TB_KEY_CTRL_B,
  _TB_KEY_CTRL_C,
  _TB_KEY_CTRL_D,
  _TB_KEY_CTRL_E,
  _TB_KEY_CTRL_F,
  _TB_KEY_CTRL_G,
  _TB_KEY_BACKSPACE,
  _TB_KEY_CTRL_H,
  _TB_KEY_TAB,
  _TB_KEY_CTRL_I,
  _TB_KEY_CTRL_J,
  _TB_KEY_CTRL_K,
  _TB_KEY_CTRL_L,
  _TB_KEY_ENTER,
  _TB_KEY_CTRL_M,
  _TB_KEY_CTRL_N,
  _TB_KEY_CTRL_O,
  _TB_KEY_CTRL_P,
  _TB_KEY_CTRL_Q,
  _TB_KEY_CTRL_R,
  _TB_KEY_CTRL_S,
  _TB_KEY_CTRL_T,
  _TB_KEY_CTRL_U,
  _TB_KEY_CTRL_V,
  _TB_KEY_CTRL_W,
  _TB_KEY_CTRL_X,
  _TB_KEY_CTRL_Y,
  _TB_KEY_CTRL_Z,
  _TB_KEY_ESC,
  _TB_KEY_CTRL_LSQ_BRACKET,
  _TB_KEY_CTRL_3,
  _TB_KEY_CTRL_4,
  _TB_KEY_CTRL_BACKSLASH,
  _TB_KEY_CTRL_5,
  _TB_KEY_CTRL_RSQ_BRACKET,
  _TB_KEY_CTRL_6,
  _TB_KEY_CTRL_7,
  _TB_KEY_CTRL_SLASH,
  _TB_KEY_CTRL_UNDERSCORE,
  _TB_KEY_SPACE,
  _TB_KEY_BACKSPACE2,
  _TB_KEY_CTRL_8 ::
    Word16
_TB_KEY_CTRL_TILDE = 0x00
_TB_KEY_CTRL_2 = 0x00
_TB_KEY_CTRL_A = 0x01
_TB_KEY_CTRL_B = 0x02
_TB_KEY_CTRL_C = 0x03
_TB_KEY_CTRL_D = 0x04
_TB_KEY_CTRL_E = 0x05
_TB_KEY_CTRL_F = 0x06
_TB_KEY_CTRL_G = 0x07
_TB_KEY_BACKSPACE = 0x08
_TB_KEY_CTRL_H = 0x08
_TB_KEY_TAB = 0x09
_TB_KEY_CTRL_I = 0x09
_TB_KEY_CTRL_J = 0x0a
_TB_KEY_CTRL_K = 0x0b
_TB_KEY_CTRL_L = 0x0c
_TB_KEY_ENTER = 0x0d
_TB_KEY_CTRL_M = 0x0d
_TB_KEY_CTRL_N = 0x0e
_TB_KEY_CTRL_O = 0x0f
_TB_KEY_CTRL_P = 0x10
_TB_KEY_CTRL_Q = 0x11
_TB_KEY_CTRL_R = 0x12
_TB_KEY_CTRL_S = 0x13
_TB_KEY_CTRL_T = 0x14
_TB_KEY_CTRL_U = 0x15
_TB_KEY_CTRL_V = 0x16
_TB_KEY_CTRL_W = 0x17
_TB_KEY_CTRL_X = 0x18
_TB_KEY_CTRL_Y = 0x19
_TB_KEY_CTRL_Z = 0x1a
_TB_KEY_ESC = 0x1b
_TB_KEY_CTRL_LSQ_BRACKET = 0x1b
_TB_KEY_CTRL_3 = 0x1b
_TB_KEY_CTRL_4 = 0x1c
_TB_KEY_CTRL_BACKSLASH = 0x1c
_TB_KEY_CTRL_5 = 0x1d
_TB_KEY_CTRL_RSQ_BRACKET = 0x1d
_TB_KEY_CTRL_6 = 0x1e
_TB_KEY_CTRL_7 = 0x1f
_TB_KEY_CTRL_SLASH = 0x1f
_TB_KEY_CTRL_UNDERSCORE = 0x1f
_TB_KEY_SPACE = 0x20
_TB_KEY_BACKSPACE2 = 0x7f
_TB_KEY_CTRL_8 = 0x7f

_TB_KEY_F1,
  _TB_KEY_F2,
  _TB_KEY_F3,
  _TB_KEY_F4,
  _TB_KEY_F5,
  _TB_KEY_F6,
  _TB_KEY_F7,
  _TB_KEY_F8,
  _TB_KEY_F9,
  _TB_KEY_F10,
  _TB_KEY_F11,
  _TB_KEY_F12,
  _TB_KEY_INSERT,
  _TB_KEY_DELETE,
  _TB_KEY_HOME,
  _TB_KEY_END,
  _TB_KEY_PGUP,
  _TB_KEY_PGDN,
  _TB_KEY_ARROW_UP,
  _TB_KEY_ARROW_DOWN,
  _TB_KEY_ARROW_LEFT,
  _TB_KEY_ARROW_RIGHT,
  _TB_KEY_BACK_TAB,
  _TB_KEY_MOUSE_LEFT,
  _TB_KEY_MOUSE_RIGHT,
  _TB_KEY_MOUSE_MIDDLE,
  _TB_KEY_MOUSE_RELEASE,
  _TB_KEY_MOUSE_WHEEL_UP,
  _TB_KEY_MOUSE_WHEEL_DOWN ::
    Word16
_TB_KEY_F1 = 0xffff - 0
_TB_KEY_F2 = 0xffff - 1
_TB_KEY_F3 = 0xffff - 2
_TB_KEY_F4 = 0xffff - 3
_TB_KEY_F5 = 0xffff - 4
_TB_KEY_F6 = 0xffff - 5
_TB_KEY_F7 = 0xffff - 6
_TB_KEY_F8 = 0xffff - 7
_TB_KEY_F9 = 0xffff - 8
_TB_KEY_F10 = 0xffff - 9
_TB_KEY_F11 = 0xffff - 10
_TB_KEY_F12 = 0xffff - 11
_TB_KEY_INSERT = 0xffff - 12
_TB_KEY_DELETE = 0xffff - 13
_TB_KEY_HOME = 0xffff - 14
_TB_KEY_END = 0xffff - 15
_TB_KEY_PGUP = 0xffff - 16
_TB_KEY_PGDN = 0xffff - 17
_TB_KEY_ARROW_UP = 0xffff - 18
_TB_KEY_ARROW_DOWN = 0xffff - 19
_TB_KEY_ARROW_LEFT = 0xffff - 20
_TB_KEY_ARROW_RIGHT = 0xffff - 21
_TB_KEY_BACK_TAB = 0xffff - 22
_TB_KEY_MOUSE_LEFT = 0xffff - 23
_TB_KEY_MOUSE_RIGHT = 0xffff - 24
_TB_KEY_MOUSE_MIDDLE = 0xffff - 25
_TB_KEY_MOUSE_RELEASE = 0xffff - 26
_TB_KEY_MOUSE_WHEEL_UP = 0xffff - 27
_TB_KEY_MOUSE_WHEEL_DOWN = 0xffff - 28

_TB_DEFAULT,
  _TB_BLACK,
  _TB_RED,
  _TB_GREEN,
  _TB_YELLOW,
  _TB_BLUE,
  _TB_MAGENTA,
  _TB_CYAN,
  _TB_WHITE,
  _TB_BOLD,
  _TB_UNDERLINE,
  _TB_REVERSE,
  _TB_ITALIC ::
    Word32
_TB_DEFAULT = 0x0000
_TB_BLACK = 0x0001
_TB_RED = 0x0002
_TB_GREEN = 0x0003
_TB_YELLOW = 0x0004
_TB_BLUE = 0x0005
_TB_MAGENTA = 0x0006
_TB_CYAN = 0x0007
_TB_WHITE = 0x0008
_TB_BOLD = 0x0100
_TB_UNDERLINE = 0x0200
_TB_REVERSE = 0x0400
_TB_ITALIC = 0x0800

_TB_EVENT_KEY, _TB_EVENT_RESIZE, _TB_EVENT_MOUSE :: Word8
_TB_EVENT_KEY = 1
_TB_EVENT_RESIZE = 2
_TB_EVENT_MOUSE = 3

_TB_MOD_ALT, _TB_MOD_CTRL, _TB_MOD_SHIFT, _TB_MOD_MOTION :: Word8
_TB_MOD_ALT = 1
_TB_MOD_CTRL = 2
_TB_MOD_SHIFT = 4
_TB_MOD_MOTION = 8

_TB_INPUT_CURRENT, _TB_INPUT_ESC, _TB_INPUT_ALT, _TB_INPUT_MOUSE :: Int
_TB_INPUT_CURRENT = 0
_TB_INPUT_ESC = 1
_TB_INPUT_ALT = 2
_TB_INPUT_MOUSE = 4

_TB_OUTPUT_CURRENT, _TB_OUTPUT_NORMAL, _TB_OUTPUT_256, _TB_OUTPUT_216, _TB_OUTPUT_GRAYSCALE, _TB_OUTPUT_TRUECOLOR :: Int
_TB_OUTPUT_CURRENT = 0
_TB_OUTPUT_NORMAL = 1
_TB_OUTPUT_256 = 2
_TB_OUTPUT_216 = 3
_TB_OUTPUT_GRAYSCALE = 4
_TB_OUTPUT_TRUECOLOR = 5

_TB_OK,
  _TB_ERR,
  _TB_ERR_NEED_MORE,
  _TB_ERR_INIT_ALREADY,
  _TB_ERR_INIT_OPEN,
  _TB_ERR_MEM,
  _TB_ERR_NO_EVENT,
  _TB_ERR_NO_TERM,
  _TB_ERR_NOT_INIT,
  _TB_ERR_OUT_OF_BOUNDS,
  _TB_ERR_READ,
  _TB_ERR_RESIZE_IOCTL,
  _TB_ERR_RESIZE_PIPE,
  _TB_ERR_RESIZE_SIGACTION,
  _TB_ERR_POLL,
  _TB_ERR_TCGETATTR,
  _TB_ERR_TCSETATTR,
  _TB_ERR_UNSUPPORTED_TERM,
  _TB_ERR_RESIZE_WRITE,
  _TB_ERR_RESIZE_POLL,
  _TB_ERR_RESIZE_READ,
  _TB_ERR_RESIZE_SSCANF,
  _TB_ERR_CAP_COLLISION ::
    Int
_TB_OK = 0
_TB_ERR = -1
_TB_ERR_NEED_MORE = -2
_TB_ERR_INIT_ALREADY = -3
_TB_ERR_INIT_OPEN = -4
_TB_ERR_MEM = -5
_TB_ERR_NO_EVENT = -6
_TB_ERR_NO_TERM = -7
_TB_ERR_NOT_INIT = -8
_TB_ERR_OUT_OF_BOUNDS = -9
_TB_ERR_READ = -10
_TB_ERR_RESIZE_IOCTL = -11
_TB_ERR_RESIZE_PIPE = -12
_TB_ERR_RESIZE_SIGACTION = -13
_TB_ERR_POLL = -14
_TB_ERR_TCGETATTR = -15
_TB_ERR_TCSETATTR = -16
_TB_ERR_UNSUPPORTED_TERM = -17
_TB_ERR_RESIZE_WRITE = -18
_TB_ERR_RESIZE_POLL = -19
_TB_ERR_RESIZE_READ = -20
_TB_ERR_RESIZE_SSCANF = -21
_TB_ERR_CAP_COLLISION = -22

_TB_ERR_SELECT, _TB_ERR_RESIZE_SELECT :: Int
_TB_ERR_SELECT = _TB_ERR_POLL
_TB_ERR_RESIZE_SELECT = _TB_ERR_RESIZE_POLL

_TB_FUNC_EXTRACT_PRE, _TB_FUNC_EXTRACT_POST :: Int
_TB_FUNC_EXTRACT_PRE = 0
_TB_FUNC_EXTRACT_POST = 1
