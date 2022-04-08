--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK;

with Debuglog.Client;

package Log
is

   procedure Init
     (Epoch : Interfaces.Unsigned_64)
      renames Debuglog.Client.Init;

   procedure Put (Item : Character) renames Debuglog.Client.Put;
   procedure Put (Item : String)    renames Debuglog.Client.Put;
   procedure Put (Item : Boolean)   renames Debuglog.Client.Put;

   procedure Put_Line (Item : String) renames Debuglog.Client.Put_Line;

   procedure New_Line renames Debuglog.Client.New_Line;

   procedure Flush renames Debuglog.Client.Flush;

   subtype Width_Index_Range is Natural range 0 .. 2;

   --  Check PCI config space write width and display message if it exceeds the
   --  maximum width specified by width index.
   procedure Check_Warn_PCI_Write_Width
     (Value     : SK.Word32;
      Width_Idx : Width_Index_Range);

end Log;
