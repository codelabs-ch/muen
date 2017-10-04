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

with Ada.Streams.Stream_IO;
with Ada.Directories;

with Interfaces;
use type Interfaces.Unsigned_64;

with Mulog;

with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;

with Bfd;
use type Bfd.Section_Flags;
use type Bfd.Unsigned_64;
with Bfd.Constants;

with Mutools.Utils;
with Mutools.Files;
with Mutools.Constants;

with Bin_Split.Utils;

package body Bin_Split
is

   use Ada.Strings.Unbounded;

   --------------------------------------------------------------------------

   function S (Source : Unbounded_String) return String
     renames To_String;

   --------------------------------------------------------------------------

   function U (Source : String) return Unbounded_String
     renames To_Unbounded_String;

   --------------------------------------------------------------------------

   procedure Add_Entry
     (Spec                  : Muxml.XML_Data_Type;
      Logical               : String;
      Writable, Executable  : Boolean;
      Fill                  : Boolean         := False;
      Hash, File_Name       : String          := "";
      Fill_Pattern          : Interfaces.Unsigned_64 := 0;
      Size, Virtual_Address : Interfaces.Unsigned_64)
   is
      Root, Child, Grand_Child, Other_Grand_Child : DOM.Core.Element;
   begin
      Root := DOM.Core.Documents.Get_Element (Spec.Doc);

      Child := DOM.Core.Documents.Create_Element (Spec.Doc, "memory");
      Child := DOM.Core.Nodes.Append_Child (Root, Child);

      if Fill then
         Grand_Child := DOM.Core.Documents.Create_Element
           (Spec.Doc,
            "fill");
         Grand_Child := DOM.Core.Nodes.Append_Child (Child, Grand_Child);

         DOM.Core.Elements.Set_Attribute
           (Grand_Child,
            "pattern",
            Mutools.Utils.To_Hex (Number => Fill_Pattern));
      else
         Grand_Child := DOM.Core.Documents.Create_Element
           (Spec.Doc,
            "file");
         Grand_Child := DOM.Core.Nodes.Append_Child (Child, Grand_Child);

         DOM.Core.Elements.Set_Attribute
           (Grand_Child, "filename", File_Name);

         if Hash /= "" then
            Other_Grand_Child := DOM.Core.Documents.Create_Element
              (Spec.Doc, "hash");
            Other_Grand_Child := DOM.Core.Nodes.Append_Child
              (Child, Other_Grand_Child);

            DOM.Core.Elements.Set_Attribute
              (Other_Grand_Child,
               "value", Hash);
         end if;
      end if;

      DOM.Core.Elements.Set_Attribute (Child, "logical", Logical);

      DOM.Core.Elements.Set_Attribute
        (Child,
         "size",
         Mutools.Utils.To_Hex (Number => Size));

      DOM.Core.Elements.Set_Attribute
        (Child,
         "virtualAddress",
         Mutools.Utils.To_Hex
           (Number => Virtual_Address));

      DOM.Core.Elements.Set_Attribute
        (Child, "executable", (if Executable then "true" else "false"));

      DOM.Core.Elements.Set_Attribute
        (Child, "writable", (if Writable then "true" else "false"));
   end Add_Entry;

   --------------------------------------------------------------------------

   procedure Check_Alignment (Section : Bfd.Sections.Section)
   is
      Page : constant Bfd.Unsigned_64 := Mutools.Constants.Page_Size;
   begin
      if Section.Vma mod Page /= 0 then
         raise Bin_Split_Error
           with "Section '" & Bfd.Sections.Get_Name (Section)
             & "' is not page-aligned.";
      end if;

      if Section.Vma /= Section.Lma then
         raise Bin_Split_Error
           with "LMA address of section '" & Bfd.Sections.Get_Name (Section)
             & "' is not equal to its VMA address.";
      end if;
   end Check_Alignment;

   --------------------------------------------------------------------------

   procedure Check_Flags
     (Sec_Info   : Section_Info;
      Descriptor : Bfd.Files.File_Type)
   is
      Sec : constant Bfd.Sections.Section
        := Get_Bfd_Section (Descriptor   => Descriptor,
                            Section_Name => S (Sec_Info.Name));
   begin
      if (Sec.Flags and Sec_Info.Flags) /= Sec_Info.Flags then
         raise Bin_Split_Error
           with "Unexpected flags for section '" & Bfd.Sections.Get_Name (Sec)
             & Mutools.Utils.To_Hex
                 (Number => Interfaces.Unsigned_64 (Sec_Info.Flags))
             & " /= "
             & Mutools.Utils.To_Hex
                 (Number => Interfaces.Unsigned_64 (Sec.Flags))
             & ".";
      end if;
   end Check_Flags;

   --------------------------------------------------------------------------

   procedure Check_Section_Names (Descriptor : Bfd.Files.File_Type)
   is
      package BS renames Bfd.Sections;

      Sect_It           : BS.Section_Iterator := BS.Get_Sections (Descriptor);
      Compound_Sections : constant CSI_Array  := Get_Compound_Section_Infos;
   begin
      while BS.Has_Element (Sect_It) loop
         declare
            Sec           : constant Bfd.Sections.Section
              := BS.Element (Sect_It);
         begin
            Outer_Loop: for I in Compound_Sections'Range loop
               for SI of Compound_Sections (I).Infos.all loop
                  if S (SI.Name) = BS.Get_Name (Sec) then
                     exit Outer_Loop;
                  end if;
               end loop;

               if I = Compound_Sections'Last then
                  raise Bin_Split_Error
                    with "Unexpected section name '" & BS.Get_Name (Sec)
                      & "'.";
               end if;
            end loop Outer_Loop;

         end;
         BS.Next (Sect_It);
      end loop;
   end Check_Section_Names;

   --------------------------------------------------------------------------

   function Get_Bfd_Section
     (Descriptor   : Bfd.Files.File_Type;
      Section_Name : String)
      return Bfd.Sections.Section
   is
   begin
      return Bfd.Sections.Find_Section
        (File => Descriptor,
         Name => Section_Name);

   exception
      when Bfd.NOT_FOUND =>
         raise Bin_Split_Error with "Section '" & Section_Name & "' not found";
   end Get_Bfd_Section;

   --------------------------------------------------------------------------

   function Get_Compound_Section_Infos return CSI_Array
   is
      package BC renames Bfd.Constants;

      C_A_L : constant Bfd.Section_Flags
        := BC.SEC_HAS_CONTENTS or BC.SEC_ALLOC or BC.SEC_LOAD;

      C_A_L_RO : constant Bfd.Section_Flags
        := C_A_L or BC.SEC_READONLY;

      Sections : constant CSI_Array
        := ((Infos =>
               new SI_Array'(1 => (Name => U(".text"),
                                   Write_To_File => True,
                                   Flags => C_A_L_RO or BC.SEC_CODE)),
             Fill => False,
             Writable => False,
             Executable => True),
            (Infos =>
               new SI_Array'(1 => (Name => U(".rodata"),
                                   Write_To_File => True,
                                   Flags => C_A_L_RO or BC.SEC_DATA)),
             Fill => False,
             Writable => False,
             Executable => False),
            (Infos =>
               new SI_Array'(1 => (Name => U(".data"),
                                   Write_To_File => True,
                                   Flags => C_A_L or BC.SEC_DATA),
                             2 => (Name => U(".bss"),
                                   Write_To_File => False,
                                   Flags => BC.SEC_ALLOC)),
             Fill => False,
             Writable => True,
             Executable => False),
            (Infos => new SI_Array'(1 => (Name => U(".stack"),
                                          Write_To_File => False,
                                          Flags => BC.SEC_ALLOC)),
             Fill => True,
             Writable => True,
             Executable => False));
   begin
      return Sections;
   end Get_Compound_Section_Infos;

   --------------------------------------------------------------------------

   procedure Open
     (Filename   :     String;
      Descriptor : out Bfd.Files.File_Type)
   is
   begin
      Bfd.Files.Open (File => Descriptor,
                      Name => Filename);
      if not Bfd.Files.Check_Format
        (File   => Descriptor,
         Expect => Bfd.Files.OBJECT)
      then
         raise Bin_Split_Error
           with "File '" & Filename & "' is not a binary object file";
      end if;

   exception
      when Bfd.OPEN_ERROR =>
         raise Bin_Split_Error with "Unable to open file '" & Filename & "'";
   end Open;

   --------------------------------------------------------------------------

   procedure Run (Spec_File, Binary, Output_Spec : String)
   is
      package BS renames Bfd.Sections;

      Spec       : Muxml.XML_Data_Type;
      Descriptor : Bfd.Files.File_Type;

      Compound_Sections : constant CSI_Array := Get_Compound_Section_Infos;

      Base_Name : constant String := Ada.Directories.Base_Name (Binary);
   begin
      Open (Filename   => Binary,
            Descriptor => Descriptor);

      Mulog.Log (Msg => "Processing cspec file '" & Spec_File & "'");
      Muxml.Parse (Data => Spec,
                   Kind => Muxml.None,  --  TODO: set to correct schema.
                   File => Spec_File);

      --  Check whether there are unknown sections in binary.
      Check_Section_Names (Descriptor);

      for CSI of Compound_Sections loop
         declare
            Section_Name      : Unbounded_String;
            Size              : Interfaces.Unsigned_64 := 0;
            First_Bfd_Section : constant Bfd.Sections.Section
              := Get_Bfd_Section (Descriptor   => Descriptor,
                                  Section_Name =>
                                    S (CSI.Infos (CSI.Infos'First).Name));
            Address           : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64 (First_Bfd_Section.Vma);
         begin
            for SI of CSI.Infos.all loop
               declare
                  Sec : constant Bfd.Sections.Section
                    := Get_Bfd_Section (Descriptor   => Descriptor,
                                        Section_Name => S (SI.Name));
               begin
                  Section_Name
                    := Section_Name
                       & (if Section_Name = U ("") then U ("") else U ("_"))
                       & Tail (SI.Name, Length (SI.Name) - 1);
                  Size := Size + Interfaces.Unsigned_64 (Sec.Size);

                  Check_Alignment (Section => Sec);

                  --  Check if section's flags are set to expected values.
                  Check_Flags (Sec_Info => SI,
                               Descriptor => Descriptor);

                  Mulog.Log (Msg => "Found Section '"
                               & BS.Get_Name (Sec)
                               & "' with size"
                               & Mutools.Utils.To_Hex
                                   (Number =>
                                      Interfaces.Unsigned_64 (Sec.Size))
                               & " @ "
                               & Mutools.Utils.To_Hex
                                   (Number =>
                                      Interfaces.Unsigned_64 (Sec.Lma)));
               end;
            end loop;

            declare
               Output_File_Name : constant String
                 := Base_Name & "_" & S (Section_Name);
            begin
               Add_Entry (Spec            => Spec,
                          Logical         => S (Section_Name),
                          Size            => Bin_Split.Utils.Round_To_Page
                                               (Size),
                          Virtual_Address => Address,
                          File_Name       => Output_File_Name,
                          Writable        => CSI.Writable,
                          Executable      => CSI.Executable,
                          Fill            => CSI.Fill,
                          Fill_Pattern    => 16#0#);

               if not CSI.Fill then
                  Write_Compound_Section (Info             => CSI,
                                          Output_File_Name => Output_File_Name,
                                          Descriptor       => Descriptor);
               end if;
            end;
         end;
      end loop;

      Muxml.Write
        (Data => Spec,
         Kind => Muxml.None,
         File => Output_Spec);

   exception
      when others =>
         Bfd.Files.Close (File => Descriptor);
         raise;
   end Run;

   --------------------------------------------------------------------------

   procedure Write_Compound_Section
     (Info             : Compound_Section_Info;
      Output_File_Name : String;
      Descriptor       : Bfd.Files.File_Type)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Out_File: Ada.Streams.Stream_IO.File_Type;
   begin

      Mutools.Files.Open
        (Filename => Output_File_Name,
         File     => Out_File);

      for SI of Info.Infos.all loop
         if SI.Write_To_File then
            declare
               Sec : constant Bfd.Sections.Section
                 := Get_Bfd_Section (Descriptor   => Descriptor,
                                     Section_Name => S (SI.Name));
               Buf : Ada.Streams.Stream_Element_Array
                 (Ada.Streams.Stream_Element_Offset (1)
                  ..  Ada.Streams.Stream_Element_Offset (Sec.Size));
               Last : Ada.Streams.Stream_Element_Offset;
            begin
               Bfd.Sections.Get_Section_Contents
                 (File => Descriptor,
                  S    => Sec,
                  Item => Buf,
                  Last => Last);

               declare
                  Bytes_Read : constant Ada.Streams.Stream_Element_Offset
                    := Last - Buf'First + 1;
               begin
                  Mulog.Log (Level => Mulog.Debug,
                             Msg   => "Read " & Bytes_Read'Img & " bytes.");
               end;

               Ada.Streams.Stream_IO.Write
                 (File => Out_File,
                  Item => Ada.Streams.Stream_Element_Array (Buf));

               Mulog.Log (Level => Mulog.Debug,
                          Msg   =>
                            "Written section '" & S (SI.Name)
                            & "' to file '" & Output_File_Name & "'.");
            end;
         end if;
      end loop;

      Ada.Streams.Stream_IO.Close (Out_File);

   exception
      when others =>
         Ada.Streams.Stream_IO.Close (Out_File);
         raise;

   end Write_Compound_Section;

end Bin_Split;
