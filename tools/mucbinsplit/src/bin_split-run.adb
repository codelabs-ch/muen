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

with Interfaces;
use type Interfaces.Unsigned_64;

with Mulog;

with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;

with Mutools.Utils;
with Mutools.Constants;

with Bin_Split.Utils;
with Bin_Split.Cmd_Line;
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
      Root := DOM.Core.Documents.Get_Element (Doc => Spec.Doc);

      Child := DOM.Core.Documents.Create_Element
        (Doc      => Spec.Doc,
         Tag_Name => "memory");

      Child := DOM.Core.Nodes.Append_Child (N => Root, New_Child => Child);

      if Fill then
         Grand_Child := DOM.Core.Documents.Create_Element
           (Doc      => Spec.Doc,
            Tag_Name =>"fill");

         Grand_Child := DOM.Core.Nodes.Append_Child
           (N         => Child,
            New_Child => Grand_Child);

         DOM.Core.Elements.Set_Attribute
           (Elem  => Grand_Child,
            Name  => "pattern",
            Value => Mutools.Utils.To_Hex (Number => Fill_Pattern));
      else
         Grand_Child := DOM.Core.Documents.Create_Element
           (Doc => Spec.Doc,
            Tag_Name => "file");

         Grand_Child := DOM.Core.Nodes.Append_Child
           (N         => Child,
            New_Child => Grand_Child);

         DOM.Core.Elements.Set_Attribute
           (Elem  => Grand_Child,
            Name  => "filename",
            Value => File_Name);

         if Hash /= "" then
            Other_Grand_Child := DOM.Core.Documents.Create_Element
              (Doc      => Spec.Doc,
               Tag_Name => "hash");

            Other_Grand_Child := DOM.Core.Nodes.Append_Child
              (N         => Child,
               New_Child => Other_Grand_Child);

            DOM.Core.Elements.Set_Attribute
              (Elem  => Other_Grand_Child,
               Name  => "value",
               Value => Hash);
         end if;
      end if;

      DOM.Core.Elements.Set_Attribute
        (Elem  => Child,
         Name  => "logical",
         Value => Logical);

      DOM.Core.Elements.Set_Attribute
        (Elem  => Child,
         Name  => "size",
         Value => Mutools.Utils.To_Hex (Number => Size));

      DOM.Core.Elements.Set_Attribute
        (Elem  => Child,
         Name  => "virtualAddress",
         Value => Mutools.Utils.To_Hex (Number => Virtual_Address));

      DOM.Core.Elements.Set_Attribute
        (Elem => Child,
         Name => "executable",
         Value => (if Executable then "true" else "false"));

      DOM.Core.Elements.Set_Attribute
        (Elem  => Child,
         Name  => "writable",
         Value => (if Writable then "true" else "false"));
   end Add_Entry;

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
   end Check_Flags;

   --------------------------------------------------------------------------

   procedure Check_Section_Names
     (Descriptor : Bin_Split.Binary.Files.File_Type)
   is
      package BS renames Bin_Split.Binary.Sections;

      Sect_It           : BS.Section_Iterator
        := Bin_Split.Binary.Sections.Get_Sections (Descriptor);
      Compound_Sections : constant Bin_Split.Types.CSI_Array
        := Get_Compound_Section_Infos;
   begin
      while BS.Has_Element (Sect_It) loop
         declare
            Sec           : constant Bin_Split.Binary.Sections.Section
              := BS.Element (Sect_It);
         begin
            Outer_Loop: for I in Compound_Sections'Range loop
               for SI of Compound_Sections (I).Infos.all loop
                  if S (SI.Name) = BS.Get_Name (Sec) then
                     exit Outer_Loop;
                  end if;
               end loop;

               if I = Compound_Sections'Last then
                  raise Bin_Split.Bin_Split_Error
                    with "Unexpected section name '" & BS.Get_Name (Sec)
                      & "'.";
               end if;
            end loop Outer_Loop;

         end;
         BS.Next (Sect_It);
      end loop;
   end Check_Section_Names;

   --------------------------------------------------------------------------

   function Get_Compound_Section_Infos return Bin_Split.Types.CSI_Array
   is
      package B renames Binary;

      C_A_L : constant Bin_Split.Binary.Section_Flags
        := B.Contents or B.Alloc or B.Load;

      C_A_L_RO : constant Bin_Split.Binary.Section_Flags
        := C_A_L or B.Readonly;

      Sections : constant Bin_Split.Types.CSI_Array
        := ((Infos =>
               new Bin_Split.Types.SI_Array'(1 => (Name => U (".text"),
                                         Write_To_File => True,
                                         Flags => C_A_L_RO or B.Code)),
             Fill => False,
             Writable => False,
             Executable => True),
            (Infos =>
               new Bin_Split.Types.SI_Array'(1 => (Name => U (".rodata"),
                                         Write_To_File => True,
                                         Flags => C_A_L_RO or B.Data)),
             Fill => False,
             Writable => False,
             Executable => False),
            (Infos =>
               new Bin_Split.Types.SI_Array'(1 => (Name => U (".data"),
                                         Write_To_File => True,
                                         Flags => C_A_L or B.Data),
                                   2 => (Name => U (".bss"),
                                         Write_To_File => False,
                                         Flags => B.Alloc)),
             Fill => False,
             Writable => True,
             Executable => False),
            (Infos => new Bin_Split.Types.SI_Array'(1 => (Name => U (".stack"),
                                                Write_To_File => False,
                                                Flags => B.Alloc)),
             Fill => True,
             Writable => True,
             Executable => False));
   begin
      return Sections;
   end Get_Compound_Section_Infos;

   --------------------------------------------------------------------------

   procedure Run (Spec_File, Binary_File, Output_Spec_File : String)
   is

      package BS renames Bin_Split.Binary.Sections;

      Spec       : Muxml.XML_Data_Type;
      Descriptor : Bin_Split.Binary.Files.File_Type;

      Compound_Sections : constant Bin_Split.Types.CSI_Array
        := Get_Compound_Section_Infos;

      Base_Name : constant String := Ada.Directories.Base_Name (Binary_File);
   begin
      Bin_Split.Binary.Files.Open (Filename   => Binary_File,
                         Descriptor => Descriptor);

      Mulog.Log (Msg => "Processing cspec file '" & Spec_File & "'");
      Muxml.Parse (Data => Spec,
                   Kind => Muxml.None,  --  TODO: set to correct schema.
                   File => Spec_File);

      --  Check whether there are unknown sections in binary.
      Check_Section_Names (Descriptor => Descriptor);

      for CSI of Compound_Sections loop
         declare
            Section_Name      : Unbounded_String;
            Size              : Interfaces.Unsigned_64 := 0;
            First_Bfd_Section : constant BS.Section
              := BS.Get_Section
                (Descriptor   => Descriptor,
                 Section_Name =>
                   S (CSI.Infos (CSI.Infos'First).Name));
            Address           : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64
                (BS.Get_Vma (First_Bfd_Section));
         begin
            for SI of CSI.Infos.all loop
               declare
                  Sec : constant BS.Section
                    := BS.Get_Section
                      (Descriptor   => Descriptor,
                       Section_Name => S (SI.Name));
               begin
                  Section_Name
                    := Section_Name
                       & (if Section_Name = U ("") then U ("") else U ("_"))
                       & Tail (SI.Name, Length (SI.Name) - 1);
                  Size := Size + BS.Get_Size (Sec);

                  Check_Alignment (Section => Sec);

                  --  Check if section's flags are set to expected values.
                  Check_Flags
                    (Sec_Info => SI,
                     Descriptor => Descriptor);

                  Mulog.Log (Msg => "Found Section '" & BS.Get_Name (Sec)
                               & "' with size "
                               & Mutools.Utils.To_Hex
                                   (Number => BS.Get_Size (Sec))
                               & " @ "
                               & Mutools.Utils.To_Hex
                                   (Number => BS.Get_Lma (Sec)));
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
                  Bin_Split.Binary.Files.Write_Compound_Section
                    (Info             => CSI,
                     Output_File_Name => Bin_Split.Cmd_Line.With_Output_Dir
                       (Filename      => Output_File_Name),
                     Descriptor       => Descriptor);
               end if;
            end;
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
