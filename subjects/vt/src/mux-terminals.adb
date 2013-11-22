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

with VT_Channels;

with Log;
with Mux.Screens;
with Mux.Channels;

package body Mux.Terminals
is

   Active_Slot : Slot_Range := Slot_Range'First;
   pragma Atomic (Active_Slot);

   VGA_CRT_Register       : constant := 16#3d4#;
   VGA_CRT_Data           : constant := 16#3d5#;
   VGA_CRT_Idx_Start_High : constant := 16#0c#;
   VGA_CRT_Idx_Start_Low  : constant := 16#0d#;

   --  Number of pending channel read requests.
   Pending_Requests : Natural := 0;
   pragma Atomic (Pending_Requests);

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
      Mux.Screens.Init;
      Mux.Screens.Set_Active (Screen => Active_Slot);
      Mux.Channels.Init;
   end Initialize;

   -------------------------------------------------------------------------

   procedure Process_Key (Event : Input.Key_Event_Type)
   is
      use Input;
   begin
      case Event.Key is
         when KEY_ESC  =>
            Log.Text_IO.Init;
         when KEY_F1 =>
            Set (Slot => 1);
            Log.Text_IO.Put_Line ("Switching to VT 1");
         when KEY_F2 =>
            Set (Slot => 2);
            Log.Text_IO.Put_Line ("Switching to VT 2");
         when KEY_F3 =>
            Set (Slot => 3);
            Log.Text_IO.Put_Line ("Switching to VT 3");
         when KEY_F4 =>
            Set (Slot => 4);
            Log.Text_IO.Put_Line ("Switching to VT 4");
         when others =>
            if Active_Slot /= 1 then
               return;
            end if;

            Mux.Channels.Write (Event);
      end case;
   end Process_Key;

   -------------------------------------------------------------------------

   procedure Queue_Request
   is
   begin
      Pending_Requests := Pending_Requests + 1;
   end Queue_Request;

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
      Mux.Screens.Set_Active (Screen => Slot);
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
      use Mux;
      use VT_Channels;

      use type VT_Channels.VT_Channel_Rdr.Result_Type;

      Data : Character;
      Res  : VT_Channel_Rdr.Result_Type;
   begin
      for C in Mux.Input_Channel_Range loop
         loop
            Channels.Read (Channel => C,
                           Char    => Data,
                           Result  => Res);

            case Res is
               when VT_Channel_Rdr.Incompatible_Interface =>
                  Log.Text_IO.Put_String (Item => "Channel ");
                  Log.Text_IO.Put_Byte   (Item => SK.Byte (C));
                  Log.Text_IO.Put_Line
                    (Item => ": Incompatible interface detected");
               when VT_Channel_Rdr.Epoch_Changed =>
                  Log.Text_IO.Put_String (Item => "Channel ");
                  Log.Text_IO.Put_Byte   (Item => SK.Byte (C));
                  Log.Text_IO.Put_Line   (Item => ": Epoch changed");
               when VT_Channel_Rdr.No_Data =>
                  null;
               when VT_Channel_Rdr.Overrun_Detected =>
                  Log.Text_IO.Put_String (Item => "Channel ");
                  Log.Text_IO.Put_Byte   (Item => SK.Byte (C));
                  Log.Text_IO.Put_Line   (Item => ": Overrun detected");
               when VT_Channel_Rdr.Inactive =>
                  Log.Text_IO.Put_String (Item => "Channel ");
                  Log.Text_IO.Put_Byte   (Item => SK.Byte (C));
                  Log.Text_IO.Put_Line   (Item => ": Inactive");
               when VT_Channel_Rdr.Success =>
                  Screens.Update
                    (Screen => Slot_Range (C),
                     Char   => Data);
            end case;

            exit when Res /= VT_Channel_Rdr.Success;
         end loop;
      end loop;
   end Update_In_Channels;

end Mux.Terminals;
