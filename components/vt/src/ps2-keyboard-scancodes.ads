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

with SK;

with Input;

package PS2.Keyboard.Scancodes
is

   --  Scancode to key symbol translation,
   --  see http://www.win.tue.nl/~aeb/linux/kbd/scancodes-1.html

   use Input;

   type Scancode_Map_Type is array (SK.Byte) of Input.Keysym_Type;

   Scancode_Map : constant Scancode_Map_Type :=
     (16#00# => KEY_RESERVED,
      16#01# => KEY_ESC,
      16#02# => KEY_1,
      16#03# => KEY_2,
      16#04# => KEY_3,
      16#05# => KEY_4,
      16#06# => KEY_5,
      16#07# => KEY_6,
      16#08# => KEY_7,
      16#09# => KEY_8,
      16#0a# => KEY_9,
      16#0b# => KEY_0,
      16#0c# => KEY_MINUS,
      16#0d# => KEY_EQUAL,
      16#0e# => KEY_BACKSPACE,
      16#0f# => KEY_TAB,
      16#10# => KEY_Q,
      16#11# => KEY_W,
      16#12# => KEY_E,
      16#13# => KEY_R,
      16#14# => KEY_T,
      16#15# => KEY_Y,
      16#16# => KEY_U,
      16#17# => KEY_I,
      16#18# => KEY_O,
      16#19# => KEY_P,
      16#1a# => KEY_LEFTBRACE,
      16#1b# => KEY_RIGHTBRACE,
      16#1c# => KEY_ENTER,
      16#1d# => KEY_LEFTCTRL,
      16#1e# => KEY_A,
      16#1f# => KEY_S,
      16#20# => KEY_D,
      16#21# => KEY_F,
      16#22# => KEY_G,
      16#23# => KEY_H,
      16#24# => KEY_J,
      16#25# => KEY_K,
      16#26# => KEY_L,
      16#27# => KEY_SEMICOLON,
      16#28# => KEY_APOSTROPHE,
      16#29# => KEY_GRAVE,
      16#2a# => KEY_LEFTSHIFT,
      16#2b# => KEY_BACKSLASH,
      16#2c# => KEY_Z,
      16#2d# => KEY_X,
      16#2e# => KEY_C,
      16#2f# => KEY_V,
      16#30# => KEY_B,
      16#31# => KEY_N,
      16#32# => KEY_M,
      16#33# => KEY_COMMA,
      16#34# => KEY_DOT,
      16#35# => KEY_SLASH,
      16#36# => KEY_RIGHTSHIFT,
      16#37# => KEY_KPASTERISK,
      16#38# => KEY_LEFTALT,
      16#39# => KEY_SPACE,
      16#3a# => KEY_CAPSLOCK,
      16#3b# => KEY_F1,
      16#3c# => KEY_F2,
      16#3d# => KEY_F3,
      16#3e# => KEY_F4,
      16#3f# => KEY_F5,
      16#40# => KEY_F6,
      16#41# => KEY_F7,
      16#42# => KEY_F8,
      16#43# => KEY_F9,
      16#44# => KEY_F10,
      16#45# => KEY_NUMLOCK,
      16#46# => KEY_SCROLLLOCK,
      16#47# => KEY_KP7,
      16#48# => KEY_KP8,
      16#49# => KEY_KP9,
      16#4a# => KEY_KPMINUS,
      16#4b# => KEY_KP4,
      16#4c# => KEY_KP5,
      16#4d# => KEY_KP6,
      16#4e# => KEY_KPPLUS,
      16#4f# => KEY_KP1,
      16#50# => KEY_KP2,
      16#51# => KEY_KP3,
      16#52# => KEY_KP0,
      16#53# => KEY_KPDOT,
      16#56# => KEY_102ND,
      16#57# => KEY_F11,
      16#58# => KEY_F12,
      others => KEY_UNKNOWN);

   Escaped_Scancode_Map : constant Scancode_Map_Type :=
     (16#1c# => KEY_KPENTER,
      16#1d# => KEY_RIGHTCTRL,
      16#20# => KEY_MUTE,
      16#2e# => KEY_VOLUMEDOWN,
      16#30# => KEY_VOLUMEUP,
      16#35# => KEY_KPSLASH,
      16#37# => KEY_SYSRQ,
      16#38# => KEY_RIGHTALT,
      16#47# => KEY_HOME,
      16#48# => KEY_UP,
      16#49# => KEY_PAGEUP,
      16#4b# => KEY_LEFT,
      16#4d# => KEY_RIGHT,
      16#4f# => KEY_END,
      16#50# => KEY_DOWN,
      16#51# => KEY_PAGEDOWN,
      16#52# => KEY_INSERT,
      16#53# => KEY_DELETE,
      16#5b# => KEY_LEFTMETA,
      16#5c# => KEY_RIGHTMETA,
      16#5d# => KEY_MENU,
      16#5e# => KEY_POWER,
      16#5f# => KEY_SLEEP,
      16#63# => KEY_WAKEUP,
      others => KEY_UNKNOWN);

end PS2.Keyboard.Scancodes;
