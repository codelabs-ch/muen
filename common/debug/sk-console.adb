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

package body SK.Console
is

   -------------------------------------------------------------------------

   procedure Init
   is
   begin
      Initialize;
   end Init;

   -------------------------------------------------------------------------

   procedure New_Line
   is
   begin
      Output_New_Line;
   end New_Line;

   -------------------------------------------------------------------------

   procedure Put_Char (Item : Character)
   is
   begin
      Output_Char (Item => Item);
   end Put_Char;

   -------------------------------------------------------------------------

   procedure Put_Line (Item : String)
   is
   begin
      Put_String (Item => Item);
      New_Line;
   end Put_Line;

   -------------------------------------------------------------------------

   procedure Put_String (Item : String)
   is
   begin
      for I in Item'Range loop
         Put_Char (Item => Item (I));
      end loop;
   end Put_String;

end SK.Console;
