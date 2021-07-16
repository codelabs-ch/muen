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
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

with GNAT.SHA256;

with Mulog;

with Muxml.Utils;

with Mutools.Utils;
with Mutools.Constants;
with Mutools.Bfd;

with Bin_Split.Utils;
with Bin_Split.Spec;
with Bin_Split.Files;

package body Bin_Split.Run
is

   function S (Source : Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   --------------------------------------------------------------------------

   procedure Check_Alignment (Section : Bfd.Sections.Section)
   is
      use type Bfd.Unsigned_64;

      package MC renames Mutools.Constants;
   begin
      if Section.Vma mod MC.Page_Size /= 0 then
         raise Bin_Split_Error
           with "Section '" & Bfd.Sections.Get_Name (Section)
             & "' is not page-aligned";
      end if;

      if Section.Vma /= Section.Lma then
         raise Bin_Split_Error
           with "LMA address of section '"
             & Bfd.Sections.Get_Name (Section)
             & "' is not equal to its VMA address";
      end if;
   end Check_Alignment;

   --------------------------------------------------------------------------

   procedure Check_Flags
     (Sec_Info   : Section_Info;
      Descriptor : Bfd.Files.File_Type)
   is
      Sec : constant Bfd.Sections.Section
        := Bfd.Sections.Find_Section (File => Descriptor,
                                      Name => S (Sec_Info.Name));
   begin
      if Sec.Flags /= Sec_Info.Flags then
         raise Bin_Split_Error
           with "Unexpected flags for section '"
             & Bfd.Sections.Get_Name (Sec)
             & "': "
             & Mutools.Utils.To_Hex
                (Number => Interfaces.Unsigned_64 (Sec_Info.Flags))
             & " /= "
             & Mutools.Utils.To_Hex
                (Number => Interfaces.Unsigned_64
                   (Sec.Flags));
      end if;
   end Check_Flags;

   --------------------------------------------------------------------------

   procedure Check_Section_Names (Descriptor : Bfd.Files.File_Type)
   is
      package BS renames Bfd.Sections;

      Sect_It : BS.Section_Iterator
        := Bfd.Sections.Get_Sections (Descriptor);
   begin
      while BS.Has_Element (Sect_It) loop
         declare
            Sec : constant Bfd.Sections.Section
              := BS.Element (Sect_It);
         begin
            if (Sec.Flags and Bfd.Constants.SEC_DEBUGGING) = 0
              and then (not Is_Valid_Section
                          (Section_Name  => Bfd.Sections.Get_Name (Sec),
                           Section_Infos => Section_Infos))
            then
               raise Bin_Split_Error
                 with "Unexpected section name '"
                   & Bfd.Sections.Get_Name (Sec) & "'";
            end if;
         end;

         BS.Next (Sect_It);
      end loop;
   end Check_Section_Names;

   --------------------------------------------------------------------------

   function Get_Binary_Section
     (Descriptor :     Bfd.Files.File_Type;
      Sec_Info   :     Section_Info;
      Sec        : out Bfd.Sections.Section)
      return Boolean
   is
   begin
      Sec := Bfd.Sections.Find_Section
        (File => Descriptor,
         Name => S (Sec_Info.Name));
      return True;

   exception
      when Bfd.NOT_FOUND =>
         if not Sec_Info.Optional then
            raise Bin_Split_Error with "Required section '"
              & S (Sec_Info.Name) & "' not found in specified binary";
         end if;
         return False;
   end Get_Binary_Section;

   -------------------------------------------------------------------------

   function Get_Start_Address
     (Descriptor : Bfd.Files.File_Type)
      return Interfaces.Unsigned_64
   is
   begin
      return Interfaces.Unsigned_64
        (Bfd.Files.Get_Start_Address (File => Descriptor));
   end Get_Start_Address;

   --------------------------------------------------------------------------

   function Is_Valid_Section
     (Section_Name  : String;
      Section_Infos : SI_Array)
      return Boolean
   is
      Found_Section : Boolean := False;
   begin
      Loop_Section_Infos :
      for SI of Section_Infos loop
         if S (SI.Name) = Section_Name then
            Found_Section := True;
            exit Loop_Section_Infos;
         end if;
      end loop Loop_Section_Infos;

      return Found_Section;
   end Is_Valid_Section;

   --------------------------------------------------------------------------

   procedure Run
     (Spec_File   : String;
      Binary_File : String;
      Output_Spec : String;
      Output_Dir  : String := "")
   is
      package BS renames Bfd.Sections;

      Spec       : Muxml.XML_Data_Type;
      Descriptor : Bfd.Files.File_Type;
      Bin_Sec    : BS.Section;
      Base_Name  : constant String := Ada.Directories.Base_Name (Binary_File);
   begin
      Utils.Make_Output_Directory (Dir_Name => Output_Dir);

      Mutools.Bfd.Open (Filename   => Binary_File,
                        Descriptor => Descriptor);

      Mulog.Log (Msg => "Processing component specification '"
                 & Spec_File & "'");

      Muxml.Parse (Data => Spec,
                   Kind => Muxml.Component,
                   File => Spec_File);

      Check_Section_Names (Descriptor => Descriptor);

      for SI of Section_Infos loop
         if Get_Binary_Section (Descriptor => Descriptor,
                                Sec_Info   => SI,
                                Sec        => Bin_Sec)
         then
            declare
               Section_Name : constant String
                 := Ada.Strings.Fixed.Trim
                   (Source => S (SI.Name),
                    Left   => Ada.Strings.Maps.To_Set (Singleton => '.'),
                    Right  => Ada.Strings.Maps.Null_Set);
               Output_File_Name : constant String
                 := Base_Name & "_" & Section_Name;
               Size : constant Interfaces.Unsigned_64
                 := Utils.Round_Up (Address => Interfaces.Unsigned_64
                                    (Bin_Sec.Size));
               Hash : GNAT.SHA256.Message_Digest;
            begin
               Check_Alignment (Section => Bin_Sec);

               Check_Flags (Sec_Info   => SI,
                            Descriptor => Descriptor);

               if SI.Write_To_File then
                  Files.Write_Section
                    (Info             => SI,
                     Output_File_Name => Output_Dir & "/" & Output_File_Name,
                     Descriptor       => Descriptor,
                     Hash             => Hash);

                  Bin_Split.Spec.Add_File_Entry
                    (Spec            => Spec,
                     Logical         => Section_Name,
                     Size            => Size,
                     Hash            => "16#" & Hash & "#",
                     Virtual_Address => Interfaces.Unsigned_64 (Bin_Sec.Vma),
                     File_Name       => Output_File_Name,
                     Writable        => SI.Writable,
                     Executable      => SI.Executable);
               else
                  Bin_Split.Spec.Add_Fill_Entry
                    (Spec            => Spec,
                     Logical         => Section_Name,
                     Size            => Size,
                     Virtual_Address => Interfaces.Unsigned_64 (Bin_Sec.Vma),
                     Writable        => SI.Writable,
                     Executable      => SI.Executable,
                     Fill_Pattern    => SI.Fill_Pattern);
               end if;
            end;
         else
            Mulog.Log (Msg => "Missing optional section '" & S (SI.Name)
                       & "' ignored");
         end if;
      end loop;

      declare
         Entry_Point_Str : constant String
           := Muxml.Utils.Get_Element_Value
             (Doc   => Spec.Doc,
              XPath => "/component/requires/vcpu/registers/gpr/rip");
         Entry_Point : Interfaces.Unsigned_64;
      begin
         if Entry_Point_Str'Length > 0 then
            Mulog.Log (Msg => "Entry point address provided in component spec:"
                       & " " & Entry_Point_Str);
         else
            Entry_Point := Get_Start_Address (Descriptor => Descriptor);
            Mulog.Log (Msg => "Setting entry point to " & Mutools.Utils.To_Hex
                       (Number => Entry_Point));
            Bin_Split.Spec.Set_RIP (Spec        => Spec,
                                    Entry_Point => Entry_Point);
         end if;
      end;

      Mulog.Log (Msg => "Writing output component spec '" & Output_Spec & "'");

      Muxml.Write
        (Data => Spec,
         Kind => Muxml.Component,
         File => Output_Spec);

   exception
      when others =>
         Bfd.Files.Close (File => Descriptor);
         raise;
   end Run;

end Bin_Split.Run;
