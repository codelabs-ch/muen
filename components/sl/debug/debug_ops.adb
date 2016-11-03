--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Debug_Ops
is

   -------------------------------------------------------------------------

   procedure Put
     (Msg  : String;
      Name : Musinfo.Name_Type)
   is
   begin
      Debuglog.Client.Put (Item => Msg);
      Debuglog.Client.Put (Item => " '");
      Put_Name (Item => Name);
      Debuglog.Client.Put_Line (Item => "'");
   end Put;

   -------------------------------------------------------------------------

   procedure Put_Name (Item : Musinfo.Name_Type)
   is
   begin
      for I in 1 .. Item.Length loop
         Debuglog.Client.Put (Item => Item.Data (Musinfo.Name_Index_Type (I)));
      end loop;
   end Put_Name;

end Debug_Ops;
