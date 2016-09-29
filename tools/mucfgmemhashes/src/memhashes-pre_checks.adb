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

with Mucfgcheck.Files;

package body Memhashes.Pre_Checks
is

   -------------------------------------------------------------------------

   procedure Clear renames Check_Procs.Clear;

   -------------------------------------------------------------------------

   function Get_Count return Natural renames Check_Procs.Get_Count;

   -------------------------------------------------------------------------

   procedure Register_All
   is
   begin
      Check_Procs.Register (Process => Mucfgcheck.Files.Files_Exist'Access);
   end Register_All;

   -------------------------------------------------------------------------

   procedure Run
     (Data      : Muxml.XML_Data_Type;
      Input_Dir : String)
   is
   begin
      Mucfgcheck.Files.Set_Input_Directory (Dir => Input_Dir);
      Check_Procs.Run (Data => Data);
   end Run;

end Memhashes.Pre_Checks;
