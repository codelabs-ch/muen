--
--  Copyright (C) 2019  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2019  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with DOM.Core.Elements;
with DOM.Core.Nodes;

with Mutools.Constants;
with Mutools.Types;
with Mutools.Utils;

with Muxml.Utils;

with Cmd_Stream.Constants;

package body Cmd_Stream.Roots.Memory
is

   package CU renames Cmd_Stream.Utils;

   --  Address of next free Tau0 private page.
   Next_Priv_Page : Interfaces.Unsigned_64 := 16#4000_0000_0000#;

   --  Generate command stream for page tables of memory region specified by
   --  ID, base address, size and level.
   procedure Create_PTs
     (Stream_Doc   : in out CU.Stream_Document_Type;
      Region_Attr  :        CU.Attribute_Type;
      Last_Level   :        Natural;
      Base_Address :        Interfaces.Unsigned_64;
      Size         :        Interfaces.Unsigned_64);

   --  Generate command stream for file or fill content of memory region.
   procedure Add_Content
     (Stream_Doc   : in out CU.Stream_Document_Type;
      Content_Node :        DOM.Core.Node;
      Region_Attr  :        CU.Attribute_Type;
      Base_Address :        Interfaces.Unsigned_64;
      Size         :        Interfaces.Unsigned_64);

   --  Generate command stream for vacuous pages of specified memory region.
   procedure Add_Vacuous_Pages
     (Stream_Doc   : in out CU.Stream_Document_Type;
      Region_Attr  :        CU.Attribute_Type;
      Base_Address :        Interfaces.Unsigned_64;
      Size         :        Interfaces.Unsigned_64);
   -------------------------------------------------------------------------

   procedure Add_Content
     (Stream_Doc   : in out CU.Stream_Document_Type;
      Content_Node :        DOM.Core.Node;
      Region_Attr  :        CU.Attribute_Type;
      Base_Address :        Interfaces.Unsigned_64;
      Size         :        Interfaces.Unsigned_64)
   is
      use type Interfaces.Unsigned_64;

      type Content_Type is (File, Fill);

      Content_Str : constant String
        := DOM.Core.Elements.Get_Tag_Name (Elem => Content_Node);
      Content : constant Content_Type
        := Content_Type'Value (Content_Str);
      End_Addr : Interfaces.Unsigned_64
        := Base_Address + Size;
      Offset_Str : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Content_Node,
           Name => "offset");
      Content_Attrs : CU.Attribute_Array
        (1 .. (if Content = File then 5 else 4));
      Cmd_Name : constant String
        := "appendPagesMR" & (if Content = File then "File" else "Fill");

      Cur_Offset : Interfaces.Unsigned_64;
   begin
      CU.Clear_Region (Stream_Doc   => Stream_Doc,
                              Base_Address => Base_Address,
                              Size         => Size);

      Content_Attrs (1) := Region_Attr;
      Content_Attrs (2) := (Attr  => U ("basePage"),
                            Value => U (Mutools.Utils.To_Hex
                              (Number => Base_Address)));

      if Content = File then
         Content_Attrs (4) := (Attr  => U ("filename"),
                               Value => U (DOM.Core.Elements.Get_Attribute
                                 (Elem => Content_Node,
                                  Name => "filename")));
         Cur_Offset := (if Offset_Str = "none" then 0
                        else Interfaces.Unsigned_64'Value (Offset_Str));

         Content_Attrs (5) := (Attr  => U ("baseOffset"),
                               Value => U (Mutools.Utils.To_Hex
                                 (Number => Cur_Offset)));
         declare
            Size_Str : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Content_Node,
               Name => "size");
         begin
            if Size_Str'Length = 0 then
               declare
                  Mem_Name : constant String := DOM.Core.Elements.Get_Attribute
                    (Elem => DOM.Core.Nodes.Parent_Node (N => Content_Node),
                     Name => "name");
                  Filename : constant String := DOM.Core.Elements.Get_Attribute
                    (Elem => Content_Node,
                     Name => "filename");
               begin
                  raise Missing_Filesize with "File '" & Filename
                    & "' which is content of memory region '" & Mem_Name
                    & "' has no size attribute";
               end;
            end if;

            End_Addr := Base_Address + Interfaces.Unsigned_64'Value
              (Size_Str) - Cur_Offset;
         end;

         --  Round up to next 4K address.
         End_Addr := (End_Addr + (Mutools.Constants.Page_Size - 1))
           / Mutools.Constants.Page_Size;
         End_Addr := End_Addr * Mutools.Constants.Page_Size;
      else
         declare
            Fill_Str : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Content_Node,
                 Name => "pattern");
            Fill_Byte : constant String
              := (if Fill_Str = "none" then "00"
                  else Fill_Str (Fill_Str'First + 3 .. Fill_Str'First + 4));
            Fill_Quad : constant String := "16#"
              & Fill_Byte & Fill_Byte & "_" & Fill_Byte & Fill_Byte & "_"
              & Fill_Byte & Fill_Byte & "_" & Fill_Byte & Fill_Byte & "#";
         begin
            Content_Attrs (4) := (Attr  => U ("fill"),
                                  Value => U (Fill_Quad));
         end;
      end if;

      Content_Attrs (3)
        := (Attr  => U ("count"),
            Value => U (Trim (Interfaces.Unsigned_64'Image
              ((End_Addr - Base_Address) / Mutools.Constants.Page_Size))));

      CU.Append_Command
        (Stream_Doc => Stream_Doc,
         Name       => Cmd_Name,
         Attrs      => Content_Attrs);

      if Content = File then
         Content_Attrs (4) := (Attr  => U ("fill"),
                               Value => U ("16#0000_0000_0000_0000#"));

         if End_Addr < Base_Address + Size then
            Content_Attrs (2).Value := U (Mutools.Utils.To_Hex
                                          (Number => End_Addr));
            Content_Attrs (3).Value
              := U (Trim (Interfaces.Unsigned_64'Image
                    ((Base_Address + Size - End_Addr)
                         / Mutools.Constants.Page_Size)));
            CU.Append_Command
              (Stream_Doc => Stream_Doc,
               Name       => "appendPagesMRFill",
               Attrs      => Content_Attrs (1 .. 4));
         end if;
      end if;
   end Add_Content;

   -------------------------------------------------------------------------

   procedure Add_Vacuous_Pages
     (Stream_Doc   : in out CU.Stream_Document_Type;
      Region_Attr  :        CU.Attribute_Type;
      Base_Address :        Interfaces.Unsigned_64;
      Size         :        Interfaces.Unsigned_64)
   is
      use type Interfaces.Unsigned_64;

      Page_Count : constant Interfaces.Unsigned_64
        := Size / Mutools.Constants.Page_Size;
   begin
      CU.Append_Command
        (Stream_Doc => Stream_Doc,
         Name       => "appendVacuousPages",
         Attrs      => (Region_Attr,
                        (Attr  => U ("basePage"),
                         Value => U (Mutools.Utils.To_Hex
                           (Number => Base_Address))),
                        (Attr  => U ("count"),
                         Value => U (Trim (Page_Count'Img)))));
   end Add_Vacuous_Pages;

   -------------------------------------------------------------------------

   procedure Create_Memory_Regions
     (Stream_Doc  : in out CU.Stream_Document_Type;
      Phys_Memory :        DOM.Core.Node_List)
   is
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Phys_Memory) - 1 loop
         declare
            use type Interfaces.Unsigned_64;
            use type DOM.Core.Node;
            use type Mutools.Types.Memory_Kind;

            Mem_Region : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Phys_Memory,
                 Index => I);
            Mem_Type : constant Mutools.Types.Memory_Kind
              := Mutools.Types.Memory_Kind'Value
                (DOM.Core.Elements.Get_Attribute (Elem => Mem_Region,
                                                  Name => "type"));
            Caching : constant String
              := DOM.Core.Elements.Get_Attribute (Elem => Mem_Region,
                                                  Name => "caching");
            Size_Str : constant String
              := DOM.Core.Elements.Get_Attribute (Elem => Mem_Region,
                                                  Name => "size");
            Size : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value (Size_Str);
            Phys_Addr_Str : constant String
              := DOM.Core.Elements.Get_Attribute (Elem => Mem_Region,
                                                  Name => "physicalAddress");
            Phys_Addr : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value (Phys_Addr_Str);
            Content_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Mem_Region,
                 XPath => "*[self::file or self::fill]");
            Hash : constant String
              := Muxml.Utils.Get_Attribute
                (Doc   => Mem_Region,
                 XPath => "hash",
                 Name  => "value");
            Has_Content : constant Boolean := Content_Node /= null;

            Root_ID : Natural;
            Region_Attr : CU.Attribute_Type;
            Level : Natural := 1;
         begin
            case Mem_Type is
               when  Mutools.Types.System
                  | Mutools.Types.System_Pt
                  | Mutools.Types.System_Vmxon
                  | Mutools.Types.System_Iobm
                  | Mutools.Types.System_Msrbm
                  | Mutools.Types.System_Vtd_Root
                  | Mutools.Types.System_Vtd_Context
                  | Mutools.Types.System_Vtd_IR =>

                  --  Skip creation of memory region, handled either explicitly
                  --  or implicitly when creating a subject.

                  null;

               when others =>
                  Root_ID := Allocate_Root;
                  Region_Attr := (Attr  => U ("region"),
                                  Value => U (Trim (Root_ID'Img)));

                  DOM.Core.Elements.Set_Attribute
                    (Elem  => Mem_Region,
                     Name  => Constants.MR_ID_Attr_Name,
                     Value => Trim (Root_ID'Img));

                  declare
                     Cur_Map_Size : Interfaces.Unsigned_64
                       := Mutools.Constants.Page_Size;
                  begin
                     while Size > Cur_Map_Size loop
                        Level := Level + 1;
                        Cur_Map_Size := Cur_Map_Size * 512;
                     end loop;
                  end;

                  CU.Append_Command
                    (Stream_Doc => Stream_Doc,
                     Name       => "createMemoryRegion",
                     Attrs      => (Region_Attr,
                                    (Attr  => U ("level"),
                                     Value => U (Trim (Level'Img))),
                                    (Attr  => U ("caching"),
                                     Value => U (Caching)),
                                    (if Hash'Length > 0 and then Has_Content
                                     then
                                       (Attr  => U ("hash"),
                                        Value => U (Hash))
                                     else CU.Null_Attr)));

                  --  Create page tables for memory region larger than 4K.

                  if Size > Mutools.Constants.Page_Size then
                     Create_PTs (Stream_Doc   => Stream_Doc,
                                 Region_Attr  => Region_Attr,
                                 Last_Level   => Level,
                                 Base_Address => Phys_Addr,
                                 Size         => Size);
                  end if;

                  if Has_Content then
                     Add_Content (Stream_Doc   => Stream_Doc,
                                  Content_Node => Content_Node,
                                  Region_Attr  => Region_Attr,
                                  Base_Address => Phys_Addr,
                                  Size         => Size);
                  else
                     Add_Vacuous_Pages
                       (Stream_Doc   => Stream_Doc,
                        Region_Attr  => Region_Attr,
                        Base_Address => Phys_Addr,
                        Size         => Size);
                  end if;

                  CU.Append_Command
                    (Stream_Doc => Stream_Doc,
                     Name       => "lockMemoryRegion",
                     Attrs      => (1 => Region_Attr));

                  CU.Append_Command
                    (Stream_Doc => Stream_Doc,
                     Name       => "activatePagesMR",
                     Attrs      => (Region_Attr,
                                    (Attr  => U ("baseVirtualAddress"),
                                     Value => U (Mutools.Utils.To_Hex
                                       (Number => 0))),
                                    (Attr  => U ("count"),
                                     Value => U (Interfaces.Unsigned_64'Image
                                       (Size
                                          / Mutools.Constants.Page_Size)))));

                  for Lvl in 1 .. Level - 1 loop
                     declare
                        Cur_Virt_Addr : Interfaces.Unsigned_64 := 0;
                     begin
                        while Cur_Virt_Addr < Size loop
                           CU.Append_Command
                             (Stream_Doc => Stream_Doc,
                              Name       => "activatePageTableMR",
                              Attrs      => (Region_Attr,
                                             (Attr  => U ("level"),
                                              Value => U (Trim (Lvl'Img))),
                                             (Attr  => U ("virtualAddress"),
                                              Value => U (Mutools.Utils.To_Hex
                                                (Number => Cur_Virt_Addr)))));
                           Cur_Virt_Addr := Cur_Virt_Addr
                             + Mutools.Constants.Page_Size * 512 ** Lvl;
                        end loop;
                     end;
                  end loop;

                  CU.Append_Command
                    (Stream_Doc => Stream_Doc,
                     Name       => "activateMemoryRegion",
                     Attrs      => (1 => Region_Attr));
            end case;
         end;
      end loop;
   end Create_Memory_Regions;

   -------------------------------------------------------------------------

   procedure Create_PTs
     (Stream_Doc   : in out CU.Stream_Document_Type;
      Region_Attr  :        CU.Attribute_Type;
      Last_Level   :        Natural;
      Base_Address :        Interfaces.Unsigned_64;
      Size         :        Interfaces.Unsigned_64)
   is
      pragma Unreferenced (Base_Address);
      use type Interfaces.Unsigned_64;

      Cur_Map_Size : Interfaces.Unsigned_64
        := 512 ** (Last_Level - 1) * Mutools.Constants.Page_Size;
   begin

      --  MR PTs must be created top-down.

      Add_PTs :
      for Lvl in reverse Interfaces.Unsigned_64 range
        1 .. Interfaces.Unsigned_64 (Last_Level - 1)
      loop
         declare
            Lvl_Attr : constant CU.Attribute_Type
              := (Attr  => U ("level"),
                  Value => U (Trim (Lvl'Img)));
            PT_Count : constant Interfaces.Unsigned_64
              := (Size + Cur_Map_Size - 1) / Cur_Map_Size;
            Cur_Virt_Addr : Interfaces.Unsigned_64 := 0;
         begin
            for I in 1 .. PT_Count loop
               declare
                  Cur_Priv_Addr_Str : constant String
                    := Mutools.Utils.To_Hex (Number => Next_Priv_Page);
               begin
                  CU.Append_Command
                    (Stream_Doc => Stream_Doc,
                     Name       => "clearPage",
                     Attrs      => (1 => (Attr  => U ("page"),
                                          Value => U (Cur_Priv_Addr_Str))));

                  CU.Append_Command
                    (Stream_Doc => Stream_Doc,
                     Name       => "createPageTableMR",
                     Attrs      => ((Attr  => U ("page"),
                                     Value => U (Cur_Priv_Addr_Str)),
                                    Region_Attr,
                                    Lvl_Attr,
                                    (Attr  => U ("virtualAddress"),
                                     Value => U (Mutools.Utils.To_Hex
                                       (Number => Cur_Virt_Addr)))));

                  Cur_Virt_Addr
                    := Cur_Virt_Addr + Cur_Map_Size;
                  Next_Priv_Page
                    := Next_Priv_Page + Mutools.Constants.Page_Size;
               end;
            end loop;

            Cur_Map_Size := Cur_Map_Size / 512;
         end;
      end loop Add_PTs;
   end Create_PTs;

end Cmd_Stream.Roots.Memory;
