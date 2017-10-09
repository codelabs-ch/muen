--
--  Copyright (C) 2017  secunet Security Networks AG
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

with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

with Interfaces;
use type Interfaces.Unsigned_64;

with Mulog;

with Muxml;

with Mutools.Utils;
with Mutools.Constants;

with Bin_Split.Utils;
with Bin_Split.Cmd_Line;
with Bin_Split.Spec;
with Bin_Split.Binary;
use type Bin_Split.Binary.Section_Flags;

package body Bin_Split.Run
is

   use Ada.Strings.Unbounded;

   --------------------------------------------------------------------------

   function S (Source : Unbounded_String) return String
     renames To_String;

   --------------------------------------------------------------------------

   function U (Source : String) return Unbounded_String
     renames To_Unbounded_String;

   --------------------------------------------------------------------------

   procedure Check_Alignment (Section : Bin_Split.Binary.Sections.Section)
   is
      Page : constant Interfaces.Unsigned_64 := Mutools.Constants.Page_Size;
   begin
      if Bin_Split.Binary.Sections.Get_Vma (Section) mod Page /= 0 then
         raise Bin_Split.Bin_Split_Error
           with "Section '" & Bin_Split.Binary.Sections.Get_Name (Section)
             & "' is not page-aligned.";
      end if;

      if Bin_Split.Binary.Sections.Get_Vma (Section)
         /= Bin_Split.Binary.Sections.Get_Lma (Section)
      then
         raise Bin_Split.Bin_Split_Error
           with "LMA address of section '"
             & Bin_Split.Binary.Sections.Get_Name (Section)
             & "' is not equal to its VMA address.";
      end if;
   end Check_Alignment;

   --------------------------------------------------------------------------

   procedure Check_Flags
     (Sec_Info   : Bin_Split.Types.Section_Info;
      Descriptor : Bin_Split.Binary.Files.File_Type)
   is
      Sec : constant Bin_Split.Binary.Sections.Section
        := Bin_Split.Binary.Sections.Get_Section
          (Descriptor   => Descriptor,
           Section_Name => S (Sec_Info.Name));
   begin
      if (Bin_Split.Binary.Sections.Get_Flags (Sec) and Sec_Info.Flags)
         /= Sec_Info.Flags
      then
         raise Bin_Split.Bin_Split_Error
           with "Unexpected flags for section '"
             & Bin_Split.Binary.Sections.Get_Name (Sec)
             & Mutools.Utils.To_Hex
                (Number => Interfaces.Unsigned_64 (Sec_Info.Flags))
             & " /= "
             & Mutools.Utils.To_Hex
                (Number => Interfaces.Unsigned_64
                   (Bin_Split.Binary.Sections.Get_Flags (Sec)))
             & ".";
      end if;

      if (Bin_Split.Binary.Sections.Get_Flags (Sec) and Bin_Split.Binary.Debug)
         /= 0
      then
         raise Bin_Split.Bin_Split_Error
           with "Section '"
             & Bin_Split.Binary.Sections.Get_Name (Sec)
             & "' is not expected to carry a debugging flag.";
      end if;
   end Check_Flags;

   --------------------------------------------------------------------------

   procedure Check_Section_Names
     (Descriptor : Bin_Split.Binary.Files.File_Type)
   is
      package BS renames Bin_Split.Binary.Sections;

      Sect_It           : BS.Section_Iterator
        := Bin_Split.Binary.Sections.Get_Sections (Descriptor);
      Section_Infos : constant Bin_Split.Types.SI_Array
        := Get_Section_Infos;
   begin
      while BS.Has_Element (Sect_It) loop
         declare
            Sec : constant Bin_Split.Binary.Sections.Section
              := BS.Element (Sect_It);
            Found_Section : Boolean
              := False;
         begin
            if (BS.Get_Flags (Sec) and Bin_Split.Binary.Debug) /= 0
            then
               Mulog.Log
                 (Level => Mulog.Debug,
                  Msg   => "Ignoring debugging section '"
                    & BS.Get_Name (Sec) & "'.");
            else
               Loop_Section_Infos: for SI of Section_Infos loop
                  if S (SI.Name) = BS.Get_Name (Sec) then
                     Found_Section := True;
                     exit Loop_Section_Infos;
                  end if;
               end loop Loop_Section_Infos;

               if not Found_Section
               then
                  raise Bin_Split.Bin_Split_Error
                  with "Unexpected section name '" & BS.Get_Name (Sec) & "'.";
               end if;
            end if;
         end;

         BS.Next (Sect_It);
      end loop;
   end Check_Section_Names;

   --------------------------------------------------------------------------

   function Get_Section_Infos return Bin_Split.Types.SI_Array
   is
      package B renames Binary;

      C_A_L : constant Bin_Split.Binary.Section_Flags
        := B.Contents or B.Alloc or B.Load;

      C_A_L_RO : constant Bin_Split.Binary.Section_Flags
        := C_A_L or B.Readonly;

      Sections : constant Bin_Split.Types.SI_Array
        := ((Name => U (".text"),
             Write_To_File => True,
             Flags => C_A_L_RO or B.Code,
             Fill => False,
             Writable => False,
             Executable => True),
            (Name => U (".rodata"),
             Write_To_File => True,
             Flags => C_A_L_RO or B.Data,
             Fill => False,
             Writable => False,
             Executable => False),
            (Name => U (".data"),
             Write_To_File => True,
             Flags => C_A_L or B.Data,
             Fill => False,
             Writable => True,
             Executable => False),
            (Name => U (".bss"),
             Write_To_File => False,
             Flags => B.Alloc,
             Fill => False,
             Writable => True,
             Executable => False),
            (Name => U (".stack"),
             Write_To_File => False,
             Flags => B.Alloc,
             Fill => True,
             Writable => True,
             Executable => False));
   begin
      return Sections;
   end Get_Section_Infos;

   --------------------------------------------------------------------------

   procedure Run (Spec_File, Binary_File, Output_Spec_File : String)
   is

      package BS renames Bin_Split.Binary.Sections;

      Spec       : Muxml.XML_Data_Type;
      Descriptor : Bin_Split.Binary.Files.File_Type;

      Base_Name : constant String := Ada.Directories.Base_Name (Binary_File);
   begin
      Bin_Split.Binary.Files.Open (Filename   => Binary_File,
                         Descriptor => Descriptor);

      Mulog.Log (Msg => "Processing cspec file '" & Spec_File & "'");
      Muxml.Parse (Data => Spec,
                   Kind => Muxml.None,  --  TODO: set to correct schema.
                   File => Spec_File);

      Check_Section_Names (Descriptor => Descriptor);

      for SI of Get_Section_Infos loop
         declare
            Sec : constant BS.Section
              := BS.Get_Section (Descriptor   => Descriptor,
                                 Section_Name => S (SI.Name));

            Section_Name : constant String
              := Ada.Strings.Fixed.Trim
                (Source => S (SI.Name),
                 Left   => Ada.Strings.Maps.To_Set (Singleton => '.'),
                 Right  => Ada.Strings.Maps.Null_Set);

            Output_File_Name : constant String
              := Base_Name & "_" & Section_Name;

            Size : constant Interfaces.Unsigned_64
              := Bin_Split.Utils.Round_To_Page (Address => BS.Get_Size (Sec));
         begin
            Check_Alignment (Section => Sec);

            Check_Flags (Sec_Info => SI,
                         Descriptor => Descriptor);

            Mulog.Log (Msg => "Found Section '" & BS.Get_Name (Sec)
                         & "' with size "
                         & Mutools.Utils.To_Hex
                         (Number => BS.Get_Size (Sec))
                         & " @ "
                         & Mutools.Utils.To_Hex
                         (Number => BS.Get_Lma (Sec)));

            if SI.Fill then
               Bin_Split.Spec.Add_Fill_Entry
                 (Spec            => Spec,
                  Logical         => Section_Name,
                  Size            => Size,
                  Virtual_Address => BS.Get_Vma (Sec),
                  Writable        => SI.Writable,
                  Executable      => SI.Executable,
                  Fill_Pattern    => 16#0#);
            else
               Bin_Split.Spec.Add_File_Entry
                 (Spec            => Spec,
                  Logical         => Section_Name,
                  Size            => Size,
                  Virtual_Address => BS.Get_Vma (Sec),
                  File_Name       => Output_File_Name,
                  Writable        => SI.Writable,
                  Executable      => SI.Executable);

                  Bin_Split.Binary.Files.Write_Section
                    (Info             => SI,
                     Output_File_Name => Bin_Split.Cmd_Line.With_Output_Dir
                       (Filename      => Output_File_Name),
                     Descriptor       => Descriptor);
            end if;
         end;
      end loop;

      Muxml.Write
        (Data => Spec,
         Kind => Muxml.None,
         File => Bin_Split.Cmd_Line.With_Output_Dir
           (Filename => Output_Spec_File));

   exception
      when others =>
         Bin_Split.Binary.Files.Close (File => Descriptor);
         raise;
   end Run;

end Bin_Split.Run;
