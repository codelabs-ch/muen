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
      Fill_Pattern          : Bfd.Unsigned_64 := Bfd.Unsigned_64 (0);
      Size, Virtual_Address : Bfd.Unsigned_64)
   is
      --  Necessary to avoid compiler warnings. :-/
      function UC (Source : String) return Unicode.CES.Byte_Sequence;

      function UC (Source : String) return Unicode.CES.Byte_Sequence is
      begin
         return Convert (Source, From => Get_By_Name("UTF-8"));
      end UC;

      Root, Child, Grand_Child, Other_Grand_Child : DOM.Core.Element;
   begin
      Root := Docs.Get_Element (Spec.Doc);

      Child := Docs.Create_Element (Spec.Doc, UC ("memory"));
      Child := Nodes.Append_Child (Root, Child);

      if Fill
      then
         Grand_Child := Docs.Create_Element (Spec.Doc, UC ("fill"));
         Grand_Child := Nodes.Append_Child (Child, Grand_Child);

         Elems.Set_Attribute
           (Grand_Child,
            UC ("pattern"),
            Mutools.Utils.To_Hex (Number => I.Unsigned_64 (Fill_Pattern)));
      else
         Grand_Child := Docs.Create_Element (Spec.Doc, UC ("file"));
         Grand_Child := Nodes.Append_Child (Child, Grand_Child);

         Elems.Set_Attribute (Grand_Child, UC ("filename"), File_Name);

         if Hash /= ""
         then
            Other_Grand_Child := Docs.Create_Element (Spec.Doc, UC ("hash"));
            Other_Grand_Child := Nodes.Append_Child (Child, Other_Grand_Child);

            Elems.Set_Attribute (Other_Grand_Child, UC ("value"), Hash);
         end if;
      end if;

      Elems.Set_Attribute (Child, UC ("logical"), UC (Logical));

      Elems.Set_Attribute
        (Child,
         UC ("size"),
         Mutools.Utils.To_Hex (Number => I.Unsigned_64 (Size)));

      Elems.Set_Attribute
        (Child,
         UC ("virtualAddress"),
         Mutools.Utils.To_Hex (Number => I.Unsigned_64 (Virtual_Address)));

      Elems.Set_Attribute
        (Child, UC ("executable"), (if Executable then "true" else "false"));

      Elems.Set_Attribute
        (Child, UC ("writable"), (if Writable then "true" else "false"));
   end Add_Entry;

   procedure Check_Address (Section : Bfd.Sections.Section)
   --------------------------------------------------------------------------

   is
      Page : constant Bfd.Unsigned_64 := 2**12;
   begin
      if not Section.Vma mod Page = 0 then
         raise Bin_Split_Error
           with "Address of section '" & Bfd.Sections.Get_Name (Section) &
           "' is not aligned to page boundaries.";
      end if;

      if not (Section.Vma = Section.Lma) then
         raise Bin_Split_Error
           with "LMA address of section '" & Bfd.Sections.Get_Name (Section) &
           "' is not equal to its VMA address.";
      end if;
   end Check_Address;

   procedure Check_Flags (Sec_Info   : Section_Info;
                          Descriptor : Bfd.Files.File_Type)
   --------------------------------------------------------------------------

   is
      Sec : constant Bfd.Sections.Section
        := Get_Bfd_Section (Descriptor   => Descriptor,
                            Section_Name => S(Sec_Info.Name));
   begin
      if not ((Sec.Flags and Sec_Info.Flags) = Sec_Info.Flags) then
         raise Bin_Split_Error
           with "Section '" & Bfd.Sections.Get_Name (Sec)
           & "' has wrong flags!";
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
            Found_Section : Boolean := False;
         begin
            for CSI of Compound_Sections loop
               for SI of CSI.Infos.all loop
                  if S (SI.Name) = BS.Get_Name (Sec)
                  then
                     Found_Section := True;
                     exit;
                  end if;
               end loop;
            end loop;

            if not Found_Section
            then
               raise Bin_Split_Error
                 with "Section name '" & BS.Get_Name (Sec) & "' not expected!";
            end if;
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

      C_A_L : constant Section_Flags
        := BC.SEC_HAS_CONTENTS or BC.SEC_ALLOC or BC.SEC_LOAD;

      C_A_L_RO : constant Section_Flags
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
           with "File '" & Filename & "' is not a binary object " & "file";
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
            Section_Name : Unbounded_String := U ("");
            Size         : Bfd.Unsigned_64  := 0;
            First_Bfd_Section: constant Bfd.Sections.Section
              := Get_Bfd_Section (Descriptor   => Descriptor,
                                  Section_Name =>
                                    S(CSI.Infos (CSI.Infos'First).Name));
            Address      : constant Bfd.Unsigned_64
              := First_Bfd_Section.Vma;
         begin
            for SI of CSI.Infos.all loop
               declare
                  Sec : constant Bfd.Sections.Section
                    := Get_Bfd_Section (Descriptor   => Descriptor,
                                        Section_Name => S(SI.Name));
               begin
                  Section_Name
                    := Section_Name
                       & (if Section_Name = U ("") then U ("") else U ("_"))
                       & Tail (SI.Name, Length (SI.Name) - 1);
                  Size := Size + Sec.Size;

                  --  Check whether section address is aligned.
                  Check_Address (Sec);

                  --  Check if section's flags are set to expected values.
                  Check_Flags (Sec_Info => SI, Descriptor => Descriptor);

                  Mulog.Log (Msg => "Found Section '"
                               & BS.Get_Name (Sec)
                               & "'; VMA: "
                               & Mutools.Utils.To_Hex
                               (Number => I.Unsigned_64 (Sec.Vma))
                               & "'; LMA: "
                               & Mutools.Utils.To_Hex
                               (Number => I.Unsigned_64 (Sec.Lma)));
               end;
            end loop;

            declare
               Output_File_Name : constant String
                 := Base_Name & "_" & S(Section_Name);
            begin
               Add_Entry (Spec            => Spec,
                          Logical         => S(Section_Name),
                          Size            => Bin_Split.Utils.Round_To_Page
                                               (Size),
                          Virtual_Address => Address,
                          File_Name       => Output_File_Name,
                          Writable        => CSI.Writable,
                          Executable      => CSI.Executable,
                          Fill            => CSI.Fill,
                          Fill_Pattern    => Bfd.Unsigned_64 (16#0#));

               if not CSI.Fill then
                  Write_Compound_Section (Info             => CSI,
                                          Output_File_Name => Output_File_Name,
                                          Descriptor       => Descriptor);
               end if;
            end;
         end;
      end loop;

      declare
         package TIO renames Ada.Text_IO;

         Out_File : TIO.File_Type;
      begin
         TIO.Create (File => Out_File,
                     Name => Output_Spec);

         Nodes.Write
           (Stream => TIO.Text_Streams.Stream (Out_File),
            Pretty_Print => True,
            N => Docs.Get_Element (Spec.Doc));

      exception
         when others =>
            TIO.Close (File => Out_File);
            raise;
      end;

      Bfd.Files.Close (File => Descriptor);

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
      package Seq_IO is new Ada.Sequential_IO
        (System.Storage_Elements.Storage_Element);

      Out_File: Seq_IO.File_Type;
   begin
      Seq_IO.Create (File => Out_File,
                     Name => Output_File_Name);

      for SI of Info.Infos.all loop
         if SI.Write_To_File then
            declare
               Sec  : constant Bfd.Sections.Section
                 := Get_Bfd_Section (Descriptor   => Descriptor,
                                     Section_Name => S(SI.Name));
               Buf  : Stream_Element_Array
                 (Stream_Element_Offset (1)
                    ..  Stream_Element_Offset(Sec.Size));
               Last : Stream_Element_Offset;
            begin
               Bfd.Sections.Get_Section_Contents
                 (File => Descriptor,
                  S    => Sec,
                  Item => Buf,
                  Last => Last);

               declare
                  Bytes_Read : constant Integer
                    := Integer (Last) - Integer (Buf'First) + 1;
               begin
                  Mulog.Log (Level => Mulog.Debug,
                             Msg   => "Read " & Bytes_Read'Image & " bytes.");
               end;

               for I in Buf'First .. Last loop
                  Seq_IO.Write (File => Out_File,
                                Item => System.Storage_Elements.Storage_Element
                                  (Buf (I)));
               end loop;

               Mulog.Log (Level => Mulog.Debug,
                          Msg   =>
                            "Written section '" & S(SI.Name)
                            & "' to file '" & Output_File_Name & "'.");
            end;
         end if;
      end loop;

      Seq_IO.Close (Out_File);

   exception
      when others =>
         Seq_IO.Close (Out_File);
         raise;

   end Write_Compound_Section;

end Bin_Split;
