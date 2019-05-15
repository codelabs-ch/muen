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

   --  Generate command stream to clear memory region specified by base address
   --  and size.
   procedure Clear_Region
     (Stream_Doc   : Muxml.XML_Data_Type;
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
            Mem_Region : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Phys_Memory,
                 Index => I);
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
            --  Root_ID : constant Natural := Allocate_Root;
         begin
            Clear_Region (Stream_Doc   => Stream_Doc,
                          Base_Address => Phys_Addr,
                          Size         => Size);
         end;
      end loop;
   end Create_Memory_Regions;

end Cmd_Stream.Roots.Memory;
