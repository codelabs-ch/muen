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

   --  Terminal 1 screen.
   package Term1_Package is new Terminal_Screen
     (Base_Address => 16#000b_8000#);
   package T1 renames Term1_Package;
   Screen_1 : T1.Screen_Type;

   --  Terminal 2 screen.
   package Term2_Package is new Terminal_Screen
     (Base_Address => 16#000b_9000#);
   package T2 renames Term2_Package;
   Screen_2 : T2.Screen_Type;

   -------------------------------------------------------------------------

   procedure Init
   is
   begin
      T1.Init;
      T2.Init;
   end Init;

   -------------------------------------------------------------------------

   procedure Update
     (Screen : Input_Channel_Range;
      Char   : Character)
   is
   begin
      case Screen
      is
         when 1 =>
            T1.Update (Screen => Screen_1,
                       Char   => Char);
         when 2 =>
            T2.Update (Screen => Screen_2,
                       Char   => Char);
      end case;
   end Update;

end Mux.Screens;
