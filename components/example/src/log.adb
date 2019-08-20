--
--  Copyright (C) 2019  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2019  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Debuglog.Client;

package body Log
is

   --  Print subject name.
   procedure Put_Name
   with
      Pre => Musinfo.Instance.Is_Valid;

   -------------------------------------------------------------------------

   procedure Put_Line (Item : String)
   is
   begin
      Put_Name;
      Debuglog.Client.Put (Item => ": ");
      Debuglog.Client.Put_Line (Item => Item);
   end Put_Line;

   -------------------------------------------------------------------------

   procedure Put_Name
   is
      Name : constant Musinfo.Name_Type := Musinfo.Instance.Subject_Name;
   begin
      for I in 1 .. Name.Length loop
         Debuglog.Client.Put (Item => Name.Data (Musinfo.Name_Index_Type (I)));
      end loop;
   end Put_Name;

end Log;
