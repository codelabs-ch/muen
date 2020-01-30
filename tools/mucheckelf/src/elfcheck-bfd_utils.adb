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

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Utils;
with Mutools.Constants;

package body Elfcheck.Bfd_Utils
is

   -------------------------------------------------------------------------

   procedure Check_Entry_Point (Address : Interfaces.Unsigned_64)
   is
      use type Interfaces.Unsigned_64;
   begin
      if Address /= Mutools.Constants.Kernel_Entry_Point then
         raise ELF_Error with "Unexpected kernel entry address "
           & Mutools.Utils.To_Hex (Number => Address) & ", expected "
           & Mutools.Utils.To_Hex
           (Number => Mutools.Constants.Kernel_Entry_Point);
      end if;
   end Check_Entry_Point;

   -------------------------------------------------------------------------

   procedure Check_Section
     (Policy      : Muxml.XML_Data_Type;
      Region_Name : String;
      Section     : Bfd.Sections.Section;
      Mapped      : Boolean := True)
   is
      use type DOM.Core.Node;

      Section_Name  : constant String := Bfd.Sections.Get_Name (S => Section);
      Physical_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Policy.Doc,
           XPath => "/system/memory/memory[@name='"
           & Region_Name & "']");
      LMA, Size     : Interfaces.Unsigned_64;
   begin
      if Physical_Node = null then
         raise ELF_Error with "Physical memory region '" & Region_Name
           & "' not found in policy";
      end if;

      LMA := Interfaces.Unsigned_64'Value
        (DOM.Core.Elements.Get_Attribute
           (Elem => Physical_Node,
            Name => "physicalAddress"));
      Size := Interfaces.Unsigned_64'Value
        (DOM.Core.Elements.Get_Attribute
           (Elem => Physical_Node,
            Name => "size"));

      if not Mapped then
         Mulog.Log (Msg => "Validating binary section '" & Section_Name
                    & "' against physical memory region '"
                    & Region_Name & "'");
         Validate_LMA_In_Region
           (Section      => Section,
            Section_Name => Section_Name,
            Region_Name  => Region_Name,
            Address      => LMA,
            Size         => Size);
         return;
      end if;

      declare
         Virtual_Regions : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Policy.Doc,
              XPath => "//memory[@physical='" & Region_Name & "']");
         Count : constant Natural
           := DOM.Core.Nodes.Length (List => Virtual_Regions);
      begin
         if Count = 0 then
            raise ELF_Error with "Virtual memory region '" & Region_Name
              & "' not found in policy";
         end if;

         Mulog.Log (Msg => "Validating binary section '" & Section_Name
                    & "' against" & Count'Img & " virtual memory region"
                    & (if Count > 1 then "s" else "")
                    & " '" & Region_Name & "'");
         for I in 0 .. Count - 1 loop
            declare
               Memory_Region : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item
                   (List  => Virtual_Regions,
                    Index => I);
               VMA : constant Interfaces.Unsigned_64
                 := Interfaces.Unsigned_64'Value
                   (DOM.Core.Elements.Get_Attribute
                      (Elem => Memory_Region,
                       Name => "virtualAddress"));
               Read_Only : constant Boolean
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Memory_Region,
                    Name => "writable") = "false";
            begin
               Validate_Size
                 (Section      => Section,
                  Section_Name => Section_Name,
                  Region_Name  => Region_Name,
                  Size         => Size);
               Validate_LMA_In_Region
                 (Section      => Section,
                  Section_Name => Section_Name,
                  Region_Name  => Region_Name,
                  Address      => LMA,
                  Size         => Size);
               Validate_Permission
                 (Section      => Section,
                  Section_Name => Section_Name,
                  Region_Name  => Region_Name,
                  Read_Only    => Read_Only);
               Validate_VMA
                 (Section      => Section,
                  Section_Name => Section_Name,
                  Region_Name  => Region_Name,
                  Address      => VMA);
            end;
         end loop;
      end;
   end Check_Section;

   -------------------------------------------------------------------------

   function Get_Section
     (Descriptor : Bfd.Files.File_Type;
      Name       : String)
      return Bfd.Sections.Section
   is
   begin
      return Bfd.Sections.Find_Section
        (File => Descriptor,
         Name => Name);

   exception
      when Bfd.NOT_FOUND =>
         raise ELF_Error with "Section '" & Name & "' not found";
   end Get_Section;

   -------------------------------------------------------------------------

   procedure Validate_LMA_In_Region
     (Section      : Bfd.Sections.Section;
      Section_Name : String;
      Region_Name  : String;
      Address      : Interfaces.Unsigned_64;
      Size         : Interfaces.Unsigned_64)
   is
      use type Interfaces.Unsigned_64;

      Lma_Start : constant Interfaces.Unsigned_64
        := Interfaces.Unsigned_64 (Section.Lma);
      Lma_End   : constant Interfaces.Unsigned_64
        := Lma_Start + Interfaces.Unsigned_64 (Section.Size) - 1;
      Reg_End   : constant Interfaces.Unsigned_64
        := Address + Size - 1;
   begin
      if Lma_Start < Address or else Lma_End > Reg_End then
         raise ELF_Error with "Section '" & Section_Name & "' from "
           & Mutools.Utils.To_Hex (Number => Lma_Start) & " .. "
           & Mutools.Utils.To_Hex (Number => Lma_End)
           & " not within physical memory region '" & Region_Name & "' from "
           & Mutools.Utils.To_Hex (Number => Address) & " .. "
           & Mutools.Utils.To_Hex (Number => Reg_End);
      end if;
   end Validate_LMA_In_Region;

   -------------------------------------------------------------------------

   procedure Validate_Permission
     (Section      : Bfd.Sections.Section;
      Section_Name : String;
      Region_Name  : String;
      Read_Only    : Boolean)
   is
      use type Bfd.Section_Flags;

      Flag_Value    : constant Bfd.Section_Flags
        := (Section.Flags and Bfd.Sections.SEC_READONLY);
      Flag_Expected : constant Bfd.Section_Flags
        := (if Read_Only then Bfd.Sections.SEC_READONLY else 0);
   begin
      if Flag_Value /= Flag_Expected then
         raise ELF_Error with "Memory region '" & Region_Name & "' is "
           & (if Read_Only then "read-only" else "read-write") & " but section"
           & " '" & Section_Name & "' READONLY flag is "
           & (if Flag_Value = 0 then "not set" else "set");
      end if;
   end Validate_Permission;

   -------------------------------------------------------------------------

   procedure Validate_Size
     (Section      : Bfd.Sections.Section;
      Section_Name : String;
      Region_Name  : String;
      Size         : Interfaces.Unsigned_64)
   is
      use type Interfaces.Unsigned_64;
   begin
      if Size < Interfaces.Unsigned_64 (Section.Size) then
         raise ELF_Error with "Size of physical memory region '"
           & Region_Name & "' too small to store '" & Section_Name
           & "' section: "
           & Mutools.Utils.To_Hex (Number => Size) & " < "
           & Mutools.Utils.To_Hex (Number => Interfaces.Unsigned_64
                                   (Section.Size));
      end if;
   end Validate_Size;

   -------------------------------------------------------------------------

   procedure Validate_VMA
     (Section      : Bfd.Sections.Section;
      Section_Name : String;
      Region_Name  : String;
      Address      : Interfaces.Unsigned_64)
   is
      use type Interfaces.Unsigned_64;
   begin
      if Address /= Interfaces.Unsigned_64 (Section.Vma) then
         raise ELF_Error with "Memory region '" & Region_Name & "' virtual "
           & "address " & Mutools.Utils.To_Hex (Number => Address) & " not "
           & "equal section '" & Section_Name & "' VMA "
           & Mutools.Utils.To_Hex (Number => Interfaces.Unsigned_64
                                   (Section.Vma));
      end if;
   end Validate_VMA;

end Elfcheck.Bfd_Utils;
