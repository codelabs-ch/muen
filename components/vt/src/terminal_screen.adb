--
--  Copyright (C) 2013-2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.Console_VGA;
with SK.Console;
with SK.Strings;

with Log;

package body Terminal_Screen
is

   package Ifa renames Interfaces;

   --  Debug flag.
   pragma $Release_Warnings (Off, "constant * is not referenced");
   D : constant Boolean := False;
   pragma $Release_Warnings (On, "constant * is not referenced");

   Semicolon : constant := 16#3b#;

   type Width_Type  is range 1 .. 80;
   type Height_Type is range 1 .. 25;

   package VGA is new SK.Console_VGA
     (Width_Type    => Width_Type,
      Height_Type   => Height_Type,
      Base_Address  => Base_Address,
      Cursor_Offset => Cursor_Offset);

   package Text_IO is new SK.Console
     (Initialize      => VGA.Init,
      Output_New_Line => VGA.New_Line,
      Output_Char     => VGA.Put_Char);

   Color_Table : constant array (SK.Byte range 0 .. 7) of
     VGA.VGA_Color_Type
       := (0 => VGA.Black,
           1 => VGA.Red,
           2 => VGA.Green,
           3 => VGA.Yellow,
           4 => VGA.Blue,
           5 => VGA.Magenta,
           6 => VGA.Cyan,
           7 => VGA.White);

   --  Print 'unknown character' message for given FSM state.
   procedure Print_Unknown
     (State : String;
      Char  : SK.Byte);

   pragma $Release_Warnings (Off, "procedure * is not referenced");
   --  Print stored CSI parameters.
   procedure Print_CSI_Params;
   pragma $Release_Warnings (On, "procedure * is not referenced");

   --  Execute CSI control function.
   procedure CSI_Dispatch (Char : SK.Byte);

   --  CSI Select Graphic Rendition.
   procedure CSI_Select_SGR;

   --  CSI Erase Display.
   procedure CSI_Erase_Display;

   --  Add CSI parameter to terminal state.
   procedure CSI_Add_Param (Char : SK.Byte);

   --  Return collected N, M height/width parameters. Display error message
   --  with given CSI sequence name if parameter count does not match.
   procedure CSI_Get_Height_Width
     (N    : out Height_Type;
      M    : out Width_Type;
      Name :     String);

   --  Return collected N, M height parameters. Display error message with
   --  given CSI sequence name if parameter count does not match.
   procedure CSI_Get_Height
     (N    : out Height_Type;
      M    : out Height_Type;
      Name :     String);

   --  Return collected height parameter. Display error message with given CSI
   --  sequence name if parameter count does not match.
   function CSI_Get_Height (Name : String) return Height_Type;

   --  Return collected width parameter. Display error message with given CSI
   --  sequence name if parameter count does not match.
   function CSI_Get_Width (Name : String) return Width_Type;

   --  Convert given CSI parameter to console height. Returns Height_Type'First
   --  if the conversion fails.
   function To_Height (Param : CSI_Param_Value_Type) return Height_Type;

   --  Convert given CSI parameter to console width. Returns Width_Type'First
   --  if the conversion fails.
   function To_Width (Param : CSI_Param_Value_Type) return Width_Type;

   --  Execute ESC sequence.
   procedure ESC_Dispatch (Char : SK.Byte);

   -------------------------------------------------------------------------

   procedure CSI_Add_Param (Char : SK.Byte)
   is
      use type SK.Byte;
   begin
      if Char = Semicolon then
         if Fsm.CSI_Param_Idx = CSI_Param_Range'Last then
            Log.Text_IO.Put_Line (Item => "!! Ignoring extra CSI parameter");
         else
            Fsm.CSI_Param_Idx := Fsm.CSI_Param_Idx + 1;
         end if;
         return;
      end if;

      declare
         Value : Natural := Natural (Char and 16#f#);
      begin
         if Fsm.CSI_Param_Idx = CSI_Empty_Params then
            Fsm.CSI_Param_Idx := Fsm.CSI_Param_Idx + 1;
            Fsm.CSI_Params (Fsm.CSI_Param_Idx) := CSI_Param_Value_Type (Value);
         else
            Value := Natural (Fsm.CSI_Params (Fsm.CSI_Param_Idx)) * 10 + Value;
            if Value <= Natural (CSI_Param_Value_Type'Last) then
               Fsm.CSI_Params (Fsm.CSI_Param_Idx) := CSI_Param_Value_Type
                 (Value);
            else
               Log.Text_IO.Put_Line (Item => "!! Overflow in CSI_Add_Param");
            end if;
         end if;
      end;
   end CSI_Add_Param;

   -------------------------------------------------------------------------

   procedure CSI_Dispatch (Char : SK.Byte)
   is
   begin
      pragma Debug (D, Log.Text_IO.Put_Line
                    (Item => "* CSI_Dispatch "
                     & SK.Strings.Img (Char)));
      pragma Debug (D, Print_CSI_Params);

      case Char
      is
         when 16#41# =>  --  CSI n A: CUU - Cursor Up
            VGA.Cursor_Up (N => CSI_Get_Height (Name => "CSI A"));
         when 16#42# =>  --  CSI n B: CUD - Cursor Down
            VGA.Cursor_Down (N => CSI_Get_Height (Name => "CSI B"));
         when 16#43# =>  --  CSI n C: CUF - Cursor Forward
            VGA.Cursor_Forward (N => CSI_Get_Width (Name => "CSI C"));
         when 16#44# =>  --  CSI n D: CUB - Cursor Back
            VGA.Cursor_Back (N => CSI_Get_Width (Name => "CSI D"));
         when 16#48# | 16#66# =>  --  CSI n ; m H|f: CUP - Cursor position
            declare
               N : Height_Type := Height_Type'First;
               M : Width_Type  := Width_Type'First;
            begin
               CSI_Get_Height_Width (N    => N,
                                     M    => M,
                                     Name => "CSI H");
               VGA.Set_Position (X => M,
                                 Y => N);
            end;
         when 16#4a# =>  --  CSI J: ED - Erase Display
            CSI_Erase_Display;
         when 16#4b# =>  --  CSI K: EL - Erase in Line
            VGA.Delete_Line_From_Cursor;
         when 16#63# =>  --  CSI c: RIS - Reset To Initial State
            Init;        --  Label omitted for now
         when 16#68# =>  --  CSI h: Set mode - ignore
            null;
         when 16#6c# =>  --  CSI l: Reset mode - ignore
            null;
         when 16#6d# =>  --  CSI n m: SGR - Select Graphic Rendition
            CSI_Select_SGR;
         when 16#72# =>  --  CSI n m r: DECSTBM - Set Top and Bottom Margins
            declare
               N : Height_Type := Height_Type'First;
               M : Height_Type := Height_Type'First;
            begin
               CSI_Get_Height (N    => N,
                               M    => M,
                               Name => "CSI r");
               VGA.Set_Scrolling_Margins (Top    => N,
                                          Bottom => M);
            end;
         when others =>
            Print_Unknown
              (State => "CSI_Dispatch",
               Char  => Char);
      end case;
   end CSI_Dispatch;

   -------------------------------------------------------------------------

   procedure CSI_Erase_Display
   is
   begin
      if Fsm.CSI_Param_Idx = CSI_Empty_Params then
         VGA.Delete_Screen_From_Cursor;
      end if;

      case Fsm.CSI_Params (1)
      is
         when 0      => VGA.Delete_Screen_From_Cursor;
         when 2      => VGA.Init;
         when others =>
            Log.Text_IO.Put_Line
              (Item => "!! Unsupported param "
               & SK.Strings.Img (Ifa.Unsigned_8 (Fsm.CSI_Params (1)))
               & " in CSI J");
      end case;
   end CSI_Erase_Display;

   -------------------------------------------------------------------------

   function CSI_Get_Height (Name : String) return Height_Type
   is
      Result : Height_Type := Height_Type'First;
   begin
      if Fsm.CSI_Param_Idx = 1 then
         Result := To_Height (Param => Fsm.CSI_Params (1));
      elsif Fsm.CSI_Param_Idx /= CSI_Empty_Params then
         Log.Text_IO.Put
           (Item => "!! Unsupported parameter count "
            & SK.Strings.Img (Ifa.Unsigned_8 (Fsm.CSI_Param_Idx)) & " in ");
         Log.Text_IO.Put_Line (Item => Name);
      end if;

      return Result;
   end CSI_Get_Height;

   -------------------------------------------------------------------------

   procedure CSI_Get_Height
     (N    : out Height_Type;
      M    : out Height_Type;
      Name :     String)
   is
   begin
      N := Height_Type'First;
      M := Height_Type'First;

      if Fsm.CSI_Param_Idx = 2 then
         N := To_Height (Param => Fsm.CSI_Params (1));
         M := To_Height (Param => Fsm.CSI_Params (2));
      elsif Fsm.CSI_Param_Idx /= CSI_Empty_Params then
         Log.Text_IO.Put
           (Item => "!! Unsupported parameter count "
            & SK.Strings.Img (Ifa.Unsigned_8 (Fsm.CSI_Param_Idx)) & " in ");
         Log.Text_IO.Put_Line (Item => Name);
      end if;
   end CSI_Get_Height;

   -------------------------------------------------------------------------

   procedure CSI_Get_Height_Width
     (N    : out Height_Type;
      M    : out Width_Type;
      Name :     String)
   is
   begin
      N := Height_Type'First;
      M := Width_Type'First;

      if Fsm.CSI_Param_Idx = 2 then
         N := To_Height (Param => Fsm.CSI_Params (1));
         M := To_Width  (Param => Fsm.CSI_Params (2));
      elsif Fsm.CSI_Param_Idx /= CSI_Empty_Params then
         Log.Text_IO.Put
           (Item => "!! Unsupported parameter count "
            & SK.Strings.Img (Ifa.Unsigned_8 (Fsm.CSI_Param_Idx)) & " in ");
         Log.Text_IO.Put_Line (Item => Name);
      end if;
   end CSI_Get_Height_Width;

   -------------------------------------------------------------------------

   function CSI_Get_Width (Name : String) return Width_Type
   is
      Result : Width_Type := Width_Type'First;
   begin
      if Fsm.CSI_Param_Idx = 1 then
         Result := To_Width (Param => Fsm.CSI_Params (1));
      elsif Fsm.CSI_Param_Idx /= CSI_Empty_Params then
         Log.Text_IO.Put_Line
           (Item => "!! Unsupported parameter count "
            & SK.Strings.Img (Ifa.Unsigned_8 (Fsm.CSI_Param_Idx)) & " in ");
         Log.Text_IO.Put_Line (Item => Name);
      end if;

      return Result;
   end CSI_Get_Width;

   -------------------------------------------------------------------------

   procedure CSI_Select_SGR
   is
      use type SK.Byte;
   begin
      if Fsm.CSI_Param_Idx = CSI_Empty_Params then
         Log.Text_IO.Put_Line (Item => "!! Empty CSI params");
         return;
      end if;

      for Idx in CSI_Param_Range'First .. Fsm.CSI_Param_Idx loop
         declare
            Param : constant CSI_Param_Value_Type := Fsm.CSI_Params (Idx);
         begin
            case Param
            is
               --  SGR (Select Graphic Rendition) parameters,
               --  see http://en.wikipedia.org/wiki/ANSI_escape_code.

               when 0  =>
                  VGA.Set_Text_Color (Color => VGA.Light_Grey);
                  VGA.Set_Bkg_Color  (Color => VGA.Black);
               when 7  => VGA.Swap_Text_With_Bkg_Color;
               when 27 => VGA.Swap_Text_With_Bkg_Color;
               when 30 .. 37 =>
                  VGA.Set_Text_Color
                    (Color => Color_Table (SK.Byte (Param) - 30));
               when 39 =>
                  VGA.Set_Text_Color (Color => VGA.Light_Grey);
               when 40 .. 47 =>
                  VGA.Set_Bkg_Color
                    (Color => Color_Table (SK.Byte (Param) - 40));
               when 49 =>
                  VGA.Set_Bkg_Color (Color => VGA.Black);
               when 1 | 10 => null;
               when others =>
                  Print_Unknown
                    (State => "CSI_Select_SGR",
                     Char  => SK.Byte (Param));
            end case;
         end;
      end loop;
   end CSI_Select_SGR;

   -------------------------------------------------------------------------

   procedure Disable_Cursor_Update
   is
   begin
      VGA.Disable_Cursor_Update;
   end Disable_Cursor_Update;

   -------------------------------------------------------------------------

   procedure Enable_Cursor_Update
   is
   begin
      VGA.Enable_Cursor_Update;
   end Enable_Cursor_Update;

   -------------------------------------------------------------------------

   procedure ESC_Dispatch (Char : SK.Byte)
   is
   begin
      pragma Debug (D, Log.Text_IO.Put_Line
                    (Item => "* ESC_Dispatch " & SK.Strings.Img (Char)
                     & ", intermediate " & SK.Strings.Img (Fsm.ESC_Collect)));

      case Char
      is
         when 16#3d# =>  --  DECKPAM - ignore
            null;
         when others =>
            Print_Unknown
              (State => "ESC_Dispatch",
               Char  => Char);
      end case;
   end ESC_Dispatch;

   -------------------------------------------------------------------------

   procedure Init
     (Label   : String  := "";
      Trusted : Boolean := False)
   is
   begin
      Text_IO.Init;
      if Label'Length > 0 then
         if Trusted then
            VGA.Set_Bkg_Color (Color => VGA.Red);
         else
            VGA.Set_Bkg_Color (Color => VGA.Light_Grey);
         end if;

         VGA.Set_Text_Color (Color => VGA.White);
         Text_IO.Put_String (Item => Label);
         VGA.Delete_Line_From_Cursor;
         VGA.New_Line;
         VGA.Set_Scrolling_Margins (Top    => Height_Type'First + 1,
                                    Bottom => Height_Type'Last);
         VGA.Set_Bkg_Color (Color => VGA.Black);
         VGA.Set_Text_Color (Color => VGA.Light_Grey);
      end if;
   end Init;

   ----------------------------------------------------------------------

   procedure Print_CSI_Params
   is
   begin
      if Fsm.CSI_Param_Idx = CSI_Empty_Params then
         return;
      end if;

      Log.Text_IO.Put (Item => "CSI params ");
      for I in CSI_Param_Range'First .. Fsm.CSI_Param_Idx loop
         Log.Text_IO.Put (Item => SK.Strings.Img
                          (Ifa.Unsigned_16 (Fsm.CSI_Params (I))));
         Log.Text_IO.Put (Item => ' ');
      end loop;
      Log.Text_IO.New_Line;
   end Print_CSI_Params;

   ----------------------------------------------------------------------

   procedure Print_Unknown
     (State : String;
      Char  : SK.Byte)
   is
   begin
      Log.Text_IO.Put      (Item => State);
      Log.Text_IO.Put_Line (Item => ": Unknown character "
                            & SK.Strings.Img (Char));
   end Print_Unknown;

   -------------------------------------------------------------------------

   function To_Height (Param : CSI_Param_Value_Type) return Height_Type
   is
   begin
      case Param
      is
         when 0 => return Height_Type'First;
         when CSI_Param_Value_Type (Height_Type'First) ..
              CSI_Param_Value_Type (Height_Type'Last) =>
            return Height_Type (Param);
         when others => return Height_Type'Last;
      end case;
   end To_Height;

   -------------------------------------------------------------------------

   function To_Width (Param : CSI_Param_Value_Type) return Width_Type
   is
   begin
      case Param
      is
         when 0 => return Width_Type'First;
         when CSI_Param_Value_Type (Width_Type'First) ..
              CSI_Param_Value_Type (Width_Type'Last) =>
            return Width_Type (Param);
         when others => return Width_Type'Last;
      end case;
   end To_Width;

   -------------------------------------------------------------------------

   procedure Update (Char : Character)
   is
      Pos : constant SK.Byte := SK.Byte (Character'Pos (Char));
   begin

      --  Video Terminal Parser, see http://vt100.net/emu/vt500_parser.png

      case Fsm.State
      is
         when State_Ground =>

            --  GROUND

            case Pos
            is
               when 16#20# .. 16#7f# =>

                  --  Printable

                  Text_IO.Put_Char (Item => Char);
               when 16#00# | 16#07# =>

                  --  NUL, BEL -> ignore

                  null;
               when 16#08# =>

                  --  BS

                  VGA.Cursor_Back;
               when 16#09# =>

                  --  HT (Horizontal Tab)

                  VGA.Put_Tab;
               when 16#0a# =>

                  --  LF

                  Text_IO.New_Line;
               when 16#0d# =>

                  --  CR

                  VGA.Set_Position (X => Width_Type'First);
               when 16#1b# =>
                  Fsm.State := State_Escape;
               when others =>
                  Print_Unknown
                    (State => "Ground",
                     Char  => Pos);
            end case;
         when State_Escape =>

            --  ESCAPE

            Fsm := Null_State;

            case Pos
            is
               when 16#20# .. 16#2f# =>
                  Fsm.State       := State_Escape_Intermediate;
                  Fsm.ESC_Collect := Pos;
               when 16#30# .. 16#4f#
                  | 16#51# .. 16#57#
                  | 16#59#
                  | 16#5a#
                  | 16#5c#
                  | 16#60# .. 16#7e# =>
                  ESC_Dispatch (Char => Pos);
                  Fsm := Null_State;
               when 16#5b# =>
                  Fsm.State := State_CSI_Entry;
               when others =>
                  Print_Unknown
                    (State => "Escape",
                     Char  => Pos);
            end case;
         when State_Escape_Intermediate =>

            --  ESCAPE intermediate

            case Pos
            is
               when 16#30# .. 16#7e# =>
                  ESC_Dispatch (Char => Pos);
                  Fsm := Null_State;
               when 16#7f# => null;
               when others =>
                  Print_Unknown
                    (State => "Escape intermediate",
                     Char  => Pos);
            end case;
         when State_CSI_Entry =>

            --  CSI entry

            Fsm := Null_State;

            case Pos
            is
               when 16#30# .. 16#39# | Semicolon =>
                  Fsm.State := State_CSI_Param;
                  CSI_Add_Param (Char => Pos);
               when 16#3c# .. 16#3f# =>
                  Fsm.State := State_CSI_Param;
                  Fsm.CSI_Collect := Pos;
               when 16#40# .. 16#7e# =>
                  CSI_Dispatch (Char => Pos);
                  Fsm := Null_State;
               when others =>
                  Print_Unknown
                    (State => "CSI entry",
                     Char  => Pos);
            end case;
         when State_CSI_Param =>

            --  CSI param

            case Pos
            is
               when 16#30# .. 16#39# | Semicolon =>
                  CSI_Add_Param (Char => Pos);
               when 16#40# .. 16#7e# =>
                  CSI_Dispatch (Char => Pos);
                  Fsm := Null_State;
               when others =>
                  Print_Unknown
                    (State => "CSI param",
                     Char  => Pos);
            end case;
      end case;
   end Update;

   -------------------------------------------------------------------------

   procedure Update_Cursor
   is
   begin
      VGA.Update_Cursor;
   end Update_Cursor;

end Terminal_Screen;
