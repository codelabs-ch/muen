--
--  Copyright (C) 2018  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2018  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

private with Ada.Strings.Unbounded;

private with GNAT.Command_Line;

with Interfaces;

with Paging;

package Ptcheck.Cmd_Line
is

   --  Init command line, use given tool description in usage output.
   procedure Init (Description : String);

   --  Return page table file.
   function Get_PT_File return String;

   --  Return page table type.
   function Get_PT_Type return Paging.Paging_Mode_Type;

   --  Return page table pointer.
   function Get_PT_Pointer return Interfaces.Unsigned_64;

   --  Return virtual address.
   function Get_Virtual_Address return Interfaces.Unsigned_64;

   Invalid_Cmd_Line : exception;

private

   PT_File      : Ada.Strings.Unbounded.Unbounded_String;
   PT_Type      : Paging.Paging_Mode_Type;
   PT_Pointer   : Interfaces.Unsigned_64;
   Virtual_Addr : Interfaces.Unsigned_64;

   Parser : GNAT.Command_Line.Opt_Parser
     := GNAT.Command_Line.Command_Line_Parser;

end Ptcheck.Cmd_Line;
