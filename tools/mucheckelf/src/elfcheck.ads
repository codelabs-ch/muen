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

private with Ada.Strings.Unbounded;

package Elfcheck
is

   --  Start the ELF check process.
   procedure Run (Policy_File, ELF_Binary : String);

   ELF_Error : exception;

private

   use Ada.Strings.Unbounded;

   type Section_Mapping_Type is record
      Region_Name  : Unbounded_String;
      Section_Name : Unbounded_String;
      Mapped       : Boolean;
      Present      : Boolean;
   end record;

   --  Mapping of memory region names to binary section names.
   --  These sections must be present in any given binary.
   Section_Map : array (1 .. 6) of aliased Section_Mapping_Type
     := (1 => (Region_Name  => To_Unbounded_String ("kernel_text"),
               Section_Name => To_Unbounded_String (".text"),
               Mapped       => True,
               Present      => False),
         2 => (Region_Name  => To_Unbounded_String ("kernel_data_0"),
               Section_Name => To_Unbounded_String (".data"),
               Mapped       => True,
               Present      => False),
         3 => (Region_Name  => To_Unbounded_String ("kernel_bss_0"),
               Section_Name => To_Unbounded_String (".bss"),
               Mapped       => True,
               Present      => False),
         4 => (Region_Name  => To_Unbounded_String ("kernel_ro"),
               Section_Name => To_Unbounded_String (".rodata"),
               Mapped       => True,
               Present      => False),
         5 => (Region_Name  => To_Unbounded_String ("kernel_global_data"),
               Section_Name => To_Unbounded_String (".globaldata"),
               Mapped       => True,
               Present      => False),
         6 => (Region_Name  => To_Unbounded_String ("kernel_text"),
               Section_Name => To_Unbounded_String (".trampoline"),
               Mapped       => False,
               Present      => False));

end Elfcheck;
