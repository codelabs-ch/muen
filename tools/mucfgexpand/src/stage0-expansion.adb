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

with Expanders.Components;
with Expanders.Platform;
with Expanders.Subjects;

package body Stage0.Expansion
is

   -------------------------------------------------------------------------

   procedure Clear
   is
   begin
      Procs.Clear;
   end Clear;

   -------------------------------------------------------------------------

   function Get_Count return Natural renames Procs.Get_Count;

   -------------------------------------------------------------------------

   procedure Register_All (Data : Muxml.XML_Data_Type)
   is
      use Expanders;

      pragma Unreferenced (Data);
   begin
      Procs.Register (Process => Subjects.Add_Missing_Elements'Access);
      Procs.Register (Process => Subjects.Merge_Bootparams'Access);
      Procs.Register (Process => Platform.Add_Section_Skeleton'Access);

      Procs.Register (Process => Platform.Add_Subject_Device_Resources'Access);
      Procs.Register (Process => Platform.Resolve_Device_Aliases'Access);
      Procs.Register (Process => Platform.Resolve_Device_Classes'Access);

      --  Expand arrays before component libraries.

      Procs.Register (Process => Components.Add_Memory_Arrays'Access);
      Procs.Register (Process => Components.Add_Channel_Arrays'Access);

      Procs.Register (Process => Components.Add_Library_Resources'Access);
   end Register_All;

   -------------------------------------------------------------------------

   procedure Run (Data : in out Muxml.XML_Data_Type) renames Procs.Run;

end Stage0.Expansion;
