--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.IO;

with Log;
with Screens;
with Channels;

package body Terminals
is

   Active_Slot : Slot_Range := Slot_Range'First;
   pragma Atomic (Active_Slot);

   VGA_CRT_Register       : constant := 16#3d4#;
   VGA_CRT_Data           : constant := 16#3d5#;
   VGA_CRT_Idx_Start_High : constant := 16#0c#;
   VGA_CRT_Idx_Start_Low  : constant := 16#0d#;

   type Scancode_Map is array (SK.Byte'Range) of Character;

   Char_Map : constant Scancode_Map
     := (2      => '1',
         3      => '2',
         4      => '3',
         5      => '4',
         6      => '5',
         7      => '6',
         8      => '7',
         9      => '8',
         10     => '9',
         11     => '0',
         12     => '-',
         13     => '=',
         14     => ASCII.BS,
         16     => 'q',
         17     => 'w',
         18     => 'e',
         19     => 'r',
         20     => 't',
         21     => 'z',
         22     => 'u',
         23     => 'i',
         24     => 'o',
         25     => 'p',
         26     => '[',
         27     => ']',
         28     => ASCII.LF,
         30     => 'a',
         31     => 's',
         32     => 'd',
         33     => 'f',
         34     => 'g',
         35     => 'h',
         36     => 'j',
         37     => 'k',
         38     => 'l',
         39     => ';',
         40     => ''',
         41     => '`',
         43     => ''',
         44     => 'y',
         45     => 'x',
         46     => 'c',
         47     => 'v',
         48     => 'b',
         49     => 'n',
         50     => 'm',
         51     => ',',
         52     => '.',
         53     => '-',
         55     => '*',
         57     => ' ',
         86     => '<',
         others => ' ');

   --  Read data from input channels if new data is present.
   procedure Update_In_Channels;

   --  Set VGA start address, see:
   --  www.phatcode.net/res/224/files/html/ch23/23-04.html
   procedure Set_VGA_Start (Address : SK.Word16);

   -------------------------------------------------------------------------

   function Get_Active_Slot return Slot_Range
   is
   begin
      return Active_Slot;
   end Get_Active_Slot;

   -------------------------------------------------------------------------

   procedure Initialize
   is
   begin
      Screens.Init;
      Channels.Init;
   end Initialize;

   -------------------------------------------------------------------------

   procedure Process_Scancode (Data : SK.Byte)
   is
      use type SK.Byte;
   begin
      case Data is
         when 1  =>
            Log.Text_IO.Init;
         when 59 =>
            Terminals.Set (Slot => 1);
            Log.Text_IO.Put_Line ("Switching to VT 1");
         when 60 =>
            Terminals.Set (Slot => 2);
            Log.Text_IO.Put_Line ("Switching to VT 2");
         when 61 =>
            Terminals.Set (Slot => 3);
            Log.Text_IO.Put_Line ("Switching to VT 3");
         when 64 =>
            Terminals.Set (Slot => 6);
            Log.Text_IO.Put_Line ("Switching to VT 6");
         when others =>
            if Active_Slot /= 1 then
               return;
            end if;

            if Data <= 86 then
               Channels.Write (Char_Map (Data));
            end if;
      end case;
   end Process_Scancode;

   -------------------------------------------------------------------------

   procedure Run
   is
      use type SK.Byte;
   begin
      loop
         Update_In_Channels;
      end loop;
   end Run;

   -------------------------------------------------------------------------

   procedure Set (Slot : Slot_Range)
   is
      use type SK.Word16;
   begin
      Active_Slot := Slot;
      Set_VGA_Start (Address => SK.Word16 (Slot - 1) * 16#800#);
   end Set;

   -------------------------------------------------------------------------

   procedure Set_VGA_Start (Address : SK.Word16)
   is
      use type SK.Word16;
   begin
      SK.IO.Outb (Port  => VGA_CRT_Register,
                  Value => VGA_CRT_Idx_Start_Low);
      SK.IO.Outb (Port  => VGA_CRT_Data,
                  Value => SK.Byte (Address));
      SK.IO.Outb (Port  => VGA_CRT_Register,
                  Value => VGA_CRT_Idx_Start_High);
      SK.IO.Outb (Port  => VGA_CRT_Data,
                  Value => SK.Byte (Address / 2 ** 8));
   end Set_VGA_Start;

   -------------------------------------------------------------------------

   procedure Update_In_Channels
   is
      use Channels;

      use type Channels.VT_Channel_Rdr.Result_Type;

      Data : Character;
      Res  : VT_Channel_Rdr.Result_Type;
   begin
      loop
         Read (Char   => Data,
               Result => Res);

         case Res is
            when VT_Channel_Rdr.Incompatible_Interface =>
               Log.Text_IO.Put_Line
                 ("Channel 1: Incompatible interface detected");
            when VT_Channel_Rdr.Epoch_Changed =>
               Log.Text_IO.Put_Line ("Channel 1: Epoch changed");
               Synchronize;
            when VT_Channel_Rdr.No_Data =>
               null;
            when VT_Channel_Rdr.Overrun_Detected =>
               Log.Text_IO.Put_Line ("Channel 1: Overrun detected");
            when VT_Channel_Rdr.Inactive =>
               Log.Text_IO.Put_Line ("Channel 1: Inactive");
            when VT_Channel_Rdr.Success =>
               Screens.Update (Char => Data);
         end case;

         exit when Res /= VT_Channel_Rdr.Success;
      end loop;
   end Update_In_Channels;

end Terminals;
