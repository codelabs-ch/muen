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

with McKae.XML.XPath.XIA;

with Mulog;

package body Validators.Platform
is

   use McKae.XML.XPath.XIA;

   --  Returns the sum of all node values.
   function Sum (Nodes : DOM.Core.Node_List) return Interfaces.Unsigned_64;

   -------------------------------------------------------------------------

   procedure Memory_Block_Overlap (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/platform/memory/memoryBlock");
   begin
      Check_Memory_Overlap (Nodes        => Nodes,
                            Region_Type  => "platform memory block",
                            Address_Attr => "physicalAddress");
   end Memory_Block_Overlap;

   -------------------------------------------------------------------------

   procedure Memory_Space (XML_Data : Muxml.XML_Data_Type)
   is
      Blocks    : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/platform/memory/memoryBlock/@size");
      Mems      : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/memory/memory/@size");
      Block_Sum : constant Interfaces.Unsigned_64 := Sum (Nodes => Blocks);
      Mem_Sum   : constant Interfaces.Unsigned_64 := Sum (Nodes => Mems);
   begin
      Mulog.Log (Msg => "Checking size of" & DOM.Core.Nodes.Length
                 (List => Mems)'Img & " physical memory region(s) against"
                 & DOM.Core.Nodes.Length (List => Blocks)'Img
                 & " memory block(s)");

      if Mem_Sum > Block_Sum then
         raise Validation_Error with "Allocated" & Mem_Sum'Img & " bytes of "
           & "physical memory but only" & Block_Sum'Img
           & " bytes available by the platform";
      end if;
   end Memory_Space;

   -------------------------------------------------------------------------

   function Sum (Nodes : DOM.Core.Node_List) return Interfaces.Unsigned_64
   is
      Node : DOM.Core.Node;
      Sum  : Interfaces.Unsigned_64 := 0;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         Node := DOM.Core.Nodes.Item
           (List  => Nodes,
            Index => I);
         Sum := Sum + Interfaces.Unsigned_64'Value
           (DOM.Core.Nodes.Node_Value (N => Node));
      end loop;

      return Sum;
   end Sum;

end Validators.Platform;
