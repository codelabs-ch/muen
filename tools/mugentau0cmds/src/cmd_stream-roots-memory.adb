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

with McKae.XML.XPath.XIA;

with Mutools.Constants;
with Mutools.Utils;

with Cmd_Stream.XML_Utils;

package body Cmd_Stream.Roots.Memory
is

   --  Address of next free Tau0 private page.
   Next_Priv_Page : Interfaces.Unsigned_64 := 16#4000_0000_0000#;

   --  Generate command stream to clear memory region specified by base address
   --  and size.
   procedure Clear_Region
     (Stream_Doc   : Muxml.XML_Data_Type;
      Base_Address : Interfaces.Unsigned_64;
      Size         : Interfaces.Unsigned_64);

   --  Generate command stream for page tables of memory region specified by
   --  ID, base address and size.
   procedure Create_PTs
     (Stream_Doc   : Muxml.XML_Data_Type;
      Region_Attr  : XML_Utils.Attribute_Type;
      Last_Level   : Natural;
      Base_Address : Interfaces.Unsigned_64;
      Size         : Interfaces.Unsigned_64);

   -------------------------------------------------------------------------

   procedure Clear_Region
     (Stream_Doc   : Muxml.XML_Data_Type;
      Base_Address : Interfaces.Unsigned_64;
      Size         : Interfaces.Unsigned_64)
   is
      use type Interfaces.Unsigned_64;

      End_Addr : constant Interfaces.Unsigned_64
        := Base_Address + Size;
      Cur_Addr : Interfaces.Unsigned_64 := Base_Address;
   begin
      while Cur_Addr < End_Addr loop
         XML_Utils.Append_Command
           (Stream_Doc => Stream_Doc,
            Name       => "clearPage",
            Attrs      => (1 => (Attr  => U ("page"),
                                 Value => U (Mutools.Utils.To_Hex
                                   (Number => Cur_Addr)))));
         Cur_Addr := Cur_Addr + Mutools.Constants.Page_Size;
      end loop;
   end Clear_Region;

   -------------------------------------------------------------------------

   procedure Create_Memory_Regions
     (Policy     : in out Muxml.XML_Data_Type;
      Stream_Doc : in out Muxml.XML_Data_Type)
   is
      Phys_Memory : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Phys_Memory) - 1 loop
         declare
            use type Interfaces.Unsigned_64;

            Mem_Region : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Phys_Memory,
                 Index => I);
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
            Root_ID : constant Natural := Allocate_Root;
            Region_Attr : constant XML_Utils.Attribute_Type
              := (Attr  => U ("region"),
                  Value => U (Trim (Root_ID'Img)));

            Level : Natural := 1;
         begin
            declare
               Cur_Map_Size : Interfaces.Unsigned_64
                 := Mutools.Constants.Page_Size;
            begin
               while Size > Cur_Map_Size loop
                  Level := Level + 1;
                  Cur_Map_Size := Cur_Map_Size * 512;
               end loop;
            end;

            Clear_Region (Stream_Doc   => Stream_Doc,
                          Base_Address => Phys_Addr,
                          Size         => Size);

            XML_Utils.Append_Command
              (Stream_Doc => Stream_Doc,
               Name       => "createMemoryRegion",
               Attrs      => (Region_Attr,
                              (Attr  => U ("level"),
                               Value => U (Trim (Level'Img))),
                              (Attr  => U ("caching"),
                               Value => U (Caching))));

            --  Create page tables for memory region larger than 4K.

            if Size > Mutools.Constants.Page_Size then
               Create_PTs (Stream_Doc   => Stream_Doc,
                           Region_Attr  => Region_Attr,
                           Last_Level   => Level,
                           Base_Address => Phys_Addr,
                           Size         => Size);
            end if;
         end;
      end loop;
   end Create_Memory_Regions;

   -------------------------------------------------------------------------

   procedure Create_PTs
     (Stream_Doc   : Muxml.XML_Data_Type;
      Region_Attr  : XML_Utils.Attribute_Type;
      Last_Level   : Natural;
      Base_Address : Interfaces.Unsigned_64;
      Size         : Interfaces.Unsigned_64)
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
            Lvl_Attr : constant XML_Utils.Attribute_Type
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
                  XML_Utils.Append_Command
                    (Stream_Doc => Stream_Doc,
                     Name       => "clearPage",
                     Attrs      => (1 => (Attr  => U ("page"),
                                          Value => U (Cur_Priv_Addr_Str))));

                  XML_Utils.Append_Command
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
