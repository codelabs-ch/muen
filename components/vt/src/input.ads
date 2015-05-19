--
--  Copyright (C) 2013, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Interfaces;

package Input
is

   --  Keyboard and mouse button symbol type, see include/input.h.
   type Keysym_Type is
     (KEY_RESERVED,
      KEY_ESC,
      KEY_1,
      KEY_2,
      KEY_3,
      KEY_4,
      KEY_5,
      KEY_6,
      KEY_7,
      KEY_8,
      KEY_9,
      KEY_0,
      KEY_MINUS,
      KEY_EQUAL,
      KEY_BACKSPACE,
      KEY_TAB,
      KEY_Q,
      KEY_W,
      KEY_E,
      KEY_R,
      KEY_T,
      KEY_Y,
      KEY_U,
      KEY_I,
      KEY_O,
      KEY_P,
      KEY_LEFTBRACE,
      KEY_RIGHTBRACE,
      KEY_ENTER,
      KEY_LEFTCTRL,
      KEY_A,
      KEY_S,
      KEY_D,
      KEY_F,
      KEY_G,
      KEY_H,
      KEY_J,
      KEY_K,
      KEY_L,
      KEY_SEMICOLON,
      KEY_APOSTROPHE,
      KEY_GRAVE,
      KEY_LEFTSHIFT,
      KEY_BACKSLASH,
      KEY_Z,
      KEY_X,
      KEY_C,
      KEY_V,
      KEY_B,
      KEY_N,
      KEY_M,
      KEY_COMMA,
      KEY_DOT,
      KEY_SLASH,
      KEY_RIGHTSHIFT,
      KEY_KPASTERISK,
      KEY_LEFTALT,
      KEY_SPACE,
      KEY_CAPSLOCK,
      KEY_F1,
      KEY_F2,
      KEY_F3,
      KEY_F4,
      KEY_F5,
      KEY_F6,
      KEY_F7,
      KEY_F8,
      KEY_F9,
      KEY_F10,
      KEY_NUMLOCK,
      KEY_SCROLLLOCK,
      KEY_KP7,
      KEY_KP8,
      KEY_KP9,
      KEY_KPMINUS,
      KEY_KP4,
      KEY_KP5,
      KEY_KP6,
      KEY_KPPLUS,
      KEY_KP1,
      KEY_KP2,
      KEY_KP3,
      KEY_KP0,
      KEY_KPDOT,
      KEY_UNASSIGNED_84,       --  84 is unassigned
      KEY_ZENKAKUHANKAKU,
      KEY_102ND,
      KEY_F11,
      KEY_F12,
      KEY_RO,
      KEY_KATAKANA,
      KEY_HIRAGANA,
      KEY_HENKAN,
      KEY_KATAKANAHIRAGANA,
      KEY_MUHENKAN,
      KEY_KPJPCOMMA,
      KEY_KPENTER,
      KEY_RIGHTCTRL,
      KEY_KPSLASH,
      KEY_SYSRQ,
      KEY_RIGHTALT,
      KEY_LINEFEED,
      KEY_HOME,
      KEY_UP,
      KEY_PAGEUP,
      KEY_LEFT,
      KEY_RIGHT,
      KEY_END,
      KEY_DOWN,
      KEY_PAGEDOWN,
      KEY_INSERT,
      KEY_DELETE,
      KEY_MACRO,
      KEY_MUTE,
      KEY_VOLUMEDOWN,
      KEY_VOLUMEUP,
      KEY_POWER,
      KEY_KPEQUAL,
      KEY_KPPLUSMINUS,
      KEY_PAUSE,
      KEY_SCALE,
      KEY_KPCOMMA,
      KEY_HANGEUL,
      KEY_HANJA,
      KEY_YEN,
      KEY_LEFTMETA,
      KEY_RIGHTMETA,
      KEY_COMPOSE,
      KEY_STOP,
      KEY_AGAIN,
      KEY_PROPS,
      KEY_UNDO,
      KEY_FRONT,
      KEY_COPY,
      KEY_OPEN,
      KEY_PASTE,
      KEY_FIND,
      KEY_CUT,
      KEY_HELP,
      KEY_MENU,
      KEY_CALC,
      KEY_SETUP,
      KEY_SLEEP,
      KEY_WAKEUP,
      KEY_FILE,
      KEY_SENDFILE,
      KEY_DELETEFILE,
      KEY_XFER,
      KEY_PROG1,
      KEY_PROG2,
      KEY_WWW,
      KEY_MSDOS,
      KEY_SCREENLOCK,
      KEY_DIRECTION,
      KEY_CYCLEWINDOWS,
      KEY_MAIL,
      KEY_BOOKMARKS,
      KEY_COMPUTER,
      KEY_BACK,
      KEY_FORWARD,
      KEY_CLOSECD,
      KEY_EJECTCD,
      KEY_EJECTCLOSECD,
      KEY_NEXTSONG,
      KEY_PLAYPAUSE,
      KEY_PREVIOUSSONG,
      KEY_STOPCD,
      KEY_RECORD,
      KEY_REWIND,
      KEY_PHONE,
      KEY_ISO,
      KEY_CONFIG,
      KEY_HOMEPAGE,
      KEY_REFRESH,
      KEY_EXIT,
      KEY_MOVE,
      KEY_EDIT,
      KEY_SCROLLUP,
      KEY_SCROLLDOWN,
      KEY_KPLEFTPAREN,
      KEY_KPRIGHTPAREN,
      KEY_NEW,
      KEY_REDO,
      KEY_F13,
      KEY_F14,
      KEY_F15,
      KEY_F16,
      KEY_F17,
      KEY_F18,
      KEY_F19,
      KEY_F20,
      KEY_F21,
      KEY_F22,
      KEY_F23,
      KEY_F24,
      KEY_UNASSIGNED_195,      --  195 is unassigned
      KEY_UNASSIGNED_196,      --  196 is unassigned
      KEY_UNASSIGNED_197,      --  197 is unassigned
      KEY_UNASSIGNED_198,      --  198 is unassigned
      KEY_UNASSIGNED_199,      --  199 is unassigned
      KEY_PLAYCD,
      KEY_PAUSECD,
      KEY_PROG3,
      KEY_PROG4,
      KEY_DASHBOARD,
      KEY_SUSPEND,
      KEY_CLOSE,
      KEY_PLAY,
      KEY_FASTFORWARD,
      KEY_BASSBOOST,
      KEY_PRINT,
      KEY_HP,
      KEY_CAMERA,
      KEY_SOUND,
      KEY_QUESTION,
      KEY_EMAIL,
      KEY_CHAT,
      KEY_SEARCH,
      KEY_CONNECT,
      KEY_FINANCE,
      KEY_SPORT,
      KEY_SHOP,
      KEY_ALTERASE,
      KEY_CANCEL,
      KEY_BRIGHTNESSDOWN,
      KEY_BRIGHTNESSUP,
      KEY_MEDIA,
      KEY_SWITCHVIDEOMODE,
      KEY_KBDILLUMTOGGLE,
      KEY_KBDILLUMDOWN,
      KEY_KBDILLUMUP,
      KEY_SEND,
      KEY_REPLY,
      KEY_FORWARDMAIL,
      KEY_SAVE,
      KEY_DOCUMENTS,
      KEY_BATTERY,
      KEY_BLUETOOTH,
      KEY_WLAN,
      KEY_UWB,
      KEY_UNKNOWN,
      KEY_VIDEO_NEXT,
      KEY_VIDEO_PREV,
      KEY_BRIGHTNESS_CYCLE,
      KEY_BRIGHTNESS_ZERO,
      KEY_DISPLAY_OFF,
      KEY_WIMAX,
      KEY_RFKILL,
      KEY_MICMUTE,
      KEY_UNASSIGNED_249,      --  249 is unassigned
      KEY_UNASSIGNED_250,      --  250 is unassigned
      KEY_UNASSIGNED_251,      --  251 is unassigned
      KEY_UNASSIGNED_252,      --  252 is unassigned
      KEY_UNASSIGNED_253,      --  253 is unassigned
      KEY_UNASSIGNED_254,      --  254 is unassigned
      KEY_UNASSIGNED_255,      --  255 is unassigned
      BTN_0,
      BTN_1,
      BTN_2,
      BTN_3,
      BTN_4,
      BTN_5,
      BTN_6,
      BTN_7,
      BTN_8,
      BTN_9,
      BTN_UNASSIGNED_266,      --  266 is unassigned
      BTN_UNASSIGNED_267,      --  267 is unassigned
      BTN_UNASSIGNED_268,      --  268 is unassigned
      BTN_UNASSIGNED_269,      --  269 is unassigned
      BTN_UNASSIGNED_270,      --  270 is unassigned
      BTN_UNASSIGNED_271,      --  271 is unassigned
      BTN_LEFT,
      BTN_RIGHT,
      BTN_MIDDLE,
      BTN_SIDE,
      BTN_EXTRA,
      BTN_FORWARD,
      BTN_BACK,
      BTN_TASK);

   type Event_Kind is
     (EVENT_RESET,
      EVENT_MOTION,
      EVENT_WHEEL,
      EVENT_PRESS,
      EVENT_RELEASE);

   --  Input event as produced by the keyboard or mouse.
   type Input_Event_Type is record
      Event_Type : Event_Kind;
      Keycode    : Keysym_Type;
      Relative_X : Interfaces.Integer_32;
      Relative_Y : Interfaces.Integer_32;
      LED_State  : Interfaces.Unsigned_32;
      Key_Count  : Interfaces.Unsigned_32;
   end record;

   Null_Input_Event : constant Input.Input_Event_Type;

private

   for Event_Kind'Size use 32;

   for Input_Event_Type use record
      Event_Type at  0 range 0 .. 31;
      Keycode    at  4 range 0 .. 31;
      Relative_X at  8 range 0 .. 31;
      Relative_Y at 12 range 0 .. 31;
      LED_State  at 16 range 0 .. 31;
      Key_Count  at 20 range 0 .. 31;
   end record;
   for Input_Event_Type'Size use 6 * 4 * 8;

   Null_Input_Event : constant Input.Input_Event_Type
     := (Event_Type => EVENT_RESET,
         Keycode    => KEY_RESERVED,
         Relative_X => 0,
         Relative_Y => 0,
         LED_State  => 0,
         Key_Count  => 0);

end Input;
