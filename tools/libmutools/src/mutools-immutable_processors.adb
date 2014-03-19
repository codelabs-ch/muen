--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Containers.Doubly_Linked_Lists;

package body Mutools.Immutable_Processors
is

   package Processor_Package is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Process_Procedure);

   Procs : Processor_Package.List;

   -------------------------------------------------------------------------

   procedure Clear
   is
   begin
      Procs.Clear;
   end Clear;

   -------------------------------------------------------------------------

   function Get_Count return Natural
   is
   begin
      return Natural (Procs.Length);
   end Get_Count;

   -------------------------------------------------------------------------

   procedure Register (Process : Process_Procedure)
   is
   begin
      Procs.Append (New_Item => Process);
   end Register;

   -------------------------------------------------------------------------

   procedure Run (Data : Param_Type)
   is
      Pos : Processor_Package.Cursor := Procs.First;
   begin
      while Processor_Package.Has_Element (Position => Pos) loop
         Processor_Package.Element (Position => Pos) (Data => Data);
         Processor_Package.Next (Position => Pos);
      end loop;
   end Run;

end Mutools.Immutable_Processors;
