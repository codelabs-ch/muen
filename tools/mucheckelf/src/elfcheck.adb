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

with Interfaces;

with Ada.Strings.Unbounded;

with Muxml;
with Mulog;

with Mutools.Bfd;

with Bfd.Files;
with Bfd.Sections;
with Bfd.Constants;

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
      Present      : Boolean;
   end record;

   type Section_Mapping_Access is access all Section_Mapping_Type;

   --  Mapping of memory region names to binary section names.
   Section_Map : array (1 .. 6) of aliased Section_Mapping_Type
     := (1 => (Region_Name  => U ("kernel_text"),
               Section_Name => U (".text"),
               Mapped       => True,
               Present      => False),
         2 => (Region_Name  => U ("kernel_data_0"),
               Section_Name => U (".data"),
               Mapped       => True,
               Present      => False),
         3 => (Region_Name  => U ("kernel_bss_0"),
               Section_Name => U (".bss"),
               Mapped       => True,
               Present      => False),
         4 => (Region_Name  => U ("kernel_ro"),
               Section_Name => U (".rodata"),
               Mapped       => True,
               Present      => False),
         5 => (Region_Name  => U ("kernel_global_data"),
               Section_Name => U (".globaldata"),
               Mapped       => True,
               Present      => False),
         6 => (Region_Name  => U ("kernel_text"),
               Section_Name => U (".trampoline"),
               Mapped       => False,
               Present      => False));

   --  Return section mapping info for section with given name. If no matching
   --  information is found, null is returned.
   function Get_Mapping (Name : String) return Section_Mapping_Access;

   --  Raise exception if not all sections in Section_Map are found.
   procedure Check_Section_Presence;

   -------------------------------------------------------------------------

   procedure Check_Section_Presence
   is
   begin
      for M of Section_Map loop
         if not M.Present then
            raise ELF_Error with "Required section '" & S (M.Section_Name)
              & "' not present";
         end if;
      end loop;
   end Check_Section_Presence;

   -------------------------------------------------------------------------

   function Get_Mapping (Name : String) return Section_Mapping_Access
   is
   begin
      for M of Section_Map loop
         if M.Section_Name = Name then
            return M'Access;
         end if;
      end loop;

      return null;
   end Get_Mapping;

   -------------------------------------------------------------------------

   procedure Run (Policy_File, ELF_Binary : String)
   is
      Policy   : Muxml.XML_Data_Type;
      Fd       : Bfd.Files.File_Type;
      Sections : Bfd.Sections.Section_Iterator;
   begin
      Mulog.Log (Msg => "Processing policy '" & Policy_File & "'");
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => Policy_File);

      Mulog.Log (Msg => "Checking binary '" & ELF_Binary & "'");
      Mutools.Bfd.Open (Filename   => ELF_Binary,
                        Descriptor => Fd);

      Sections := Bfd.Sections.Get_Sections (File => Fd);

      while Bfd.Sections.Has_Element (Iter => Sections) loop
         declare
            use type Bfd.Section_Flags;

            Section : constant Bfd.Sections.Section
              := Bfd.Sections.Element (Iter => Sections);
            Name    : constant String := Bfd.Sections.Get_Name (S => Section);
            Mapping : constant Section_Mapping_Access
              := Get_Mapping (Name => Name);
         begin

            --  Allow debug sections for now.

            if (Section.Flags and Bfd.Constants.SEC_DEBUGGING) = 0 then
               if Mapping = null then
                  raise ELF_Error with "Unexpected ELF section '" & Name & "'";
               end if;

               Bfd_Utils.Check_Section
                 (Policy      => Policy,
                  Region_Name => S (Mapping.Region_Name),
                  Section     => Section,
                  Mapped      => Mapping.Mapped);
               Mapping.Present := True;
            end if;
         end;

         Bfd.Sections.Next (Iter => Sections);
      end loop;

      Check_Section_Presence;

      Bfd_Utils.Check_Entry_Point
        (Address => Interfaces.Unsigned_64
           (Bfd.Files.Get_Start_Address (File => Fd)));

      Bfd.Files.Close (File => Fd);

      Mulog.Log (Msg => "Check of ELF binary '" & ELF_Binary & "' successful");

   exception
      when others =>
         Bfd.Files.Close (File => Fd);
         raise;
   end Run;

end Elfcheck;
