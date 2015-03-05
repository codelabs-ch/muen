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

with Terminal_Screen;

package body Mux.Screens
is

   package Term1_Package is new Terminal_Screen
     (Base_Address  => 16#000b_8000#,
      Cursor_Offset => 0);
   package T1 renames Term1_Package;

   package Term2_Package is new Terminal_Screen
     (Base_Address  => 16#000b_9000#,
      Cursor_Offset => 16#0800#);
   package T2 renames Term2_Package;

   package Term3_Package is new Terminal_Screen
     (Base_Address  => 16#000b_a000#,
      Cursor_Offset => 16#1000#);
   package T3 renames Term3_Package;

   package Term4_Package is new Terminal_Screen
     (Base_Address  => 16#000b_b000#,
      Cursor_Offset => 16#1800#);
   package T4 renames Term4_Package;

   package Term5_Package is new Terminal_Screen
     (Base_Address  => 16#000b_c000#,
      Cursor_Offset => 16#1800#);
   package T5 renames Term5_Package;

   package Term6_Package is new Terminal_Screen
     (Base_Address  => 16#000b_d000#,
      Cursor_Offset => 16#1800#);
   package T6 renames Term6_Package;

   -------------------------------------------------------------------------

   procedure Init
   is
   begin

      --  TODO: Read this from configuration.

      T1.Init (Label   => "NIC Linux",
               Trusted => False);
      T2.Init (Label   => "Storage Linux",
               Trusted => False);
   end Init;

   -------------------------------------------------------------------------

   procedure Set_Active (Screen : Slot_Range)
   is
   begin
      T1.Disable_Cursor_Update;
      T2.Disable_Cursor_Update;
      T3.Disable_Cursor_Update;
      T4.Disable_Cursor_Update;
      T5.Disable_Cursor_Update;
      T6.Disable_Cursor_Update;
      case Screen
      is
         when 1 =>
            T1.Enable_Cursor_Update;
            T1.Update_Cursor;
         when 2 =>
            T2.Enable_Cursor_Update;
            T2.Update_Cursor;
         when 3 =>
            T3.Enable_Cursor_Update;
            T3.Update_Cursor;
         when 4 =>
            T4.Enable_Cursor_Update;
            T4.Update_Cursor;
         when 5 =>
            T5.Enable_Cursor_Update;
            T5.Update_Cursor;
         when 6 =>
            T6.Enable_Cursor_Update;
            T6.Update_Cursor;
      end case;
   end Set_Active;

   -------------------------------------------------------------------------

   procedure Update
     (Screen : Slot_Range;
      Char   : Character)
   is
   begin
      case Screen
      is
         when 1 => T1.Update (Char => Char);
         when 2 => T2.Update (Char => Char);
         when 3 => T3.Update (Char => Char);
         when 4 => T4.Update (Char => Char);
         when 5 => T5.Update (Char => Char);
         when 6 => T6.Update (Char => Char);
      end case;
   end Update;

end Mux.Screens;
