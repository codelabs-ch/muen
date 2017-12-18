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

with Ada.Strings.Unbounded;

with Muxml;
with Mulog;

with Mutools.Bfd;

with Bfd.Files;

with Elfcheck.Bfd_Utils;

package body Elfcheck
is
   use Ada.Strings.Unbounded;

   function S
     (Source : Unbounded_String)
      return String
      renames To_String;

   function U
     (Source : String)
      return Unbounded_String
      renames To_Unbounded_String;

   type Section_Mapping_Type is record
      Region_Name  : Unbounded_String;
      Section_Name : Unbounded_String;
      Mapped       : Boolean;
   end record;

   --  Mapping of memory region names to binary section names.
   Section_Map : constant array (1 .. 6) of Section_Mapping_Type
     := (1 => (Region_Name  => U ("kernel_text"),
               Section_Name => U (".text"),
               Mapped       => True),
         2 => (Region_Name  => U ("kernel_data_0"),
               Section_Name => U (".data"),
               Mapped       => True),
         3 => (Region_Name  => U ("kernel_bss_0"),
               Section_Name => U (".bss"),
               Mapped       => True),
         4 => (Region_Name  => U ("kernel_ro"),
               Section_Name => U (".rodata"),
               Mapped       => True),
         5 => (Region_Name  => U ("kernel_global_data"),
               Section_Name => U (".globaldata"),
               Mapped       => True),
         6 => (Region_Name  => U ("kernel_text"),
               Section_Name => U (".trampoline"),
               Mapped       => False));

   -------------------------------------------------------------------------

   procedure Run (Policy_File, ELF_Binary : String)
   is
      Policy : Muxml.XML_Data_Type;
      Fd     : Bfd.Files.File_Type;
   begin
      Mulog.Log (Msg => "Processing policy '" & Policy_File & "'");
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => Policy_File);

      Mulog.Log (Msg => "Checking binary '" & ELF_Binary & "'");
      Mutools.Bfd.Open (Filename   => ELF_Binary,
                        Descriptor => Fd);

      for M of Section_Map loop
         Bfd_Utils.Check_Section
           (Policy       => Policy,
            Region_Name  => S (M.Region_Name),
            Section      => Bfd_Utils.Get_Section
              (Descriptor => Fd,
               Name       => S (M.Section_Name)),
            Mapped        => M.Mapped);
      end loop;

      Bfd.Files.Close (File => Fd);

      Mulog.Log (Msg => "Check of ELF binary '" & ELF_Binary & "' successful");

   exception
      when others =>
         Bfd.Files.Close (File => Fd);
         raise;
   end Run;

end Elfcheck;
