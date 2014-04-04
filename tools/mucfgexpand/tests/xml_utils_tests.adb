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

with Ada.Directories;
with Ada.Exceptions;

with Interfaces;

with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;

with Muxml;

with Expanders.XML_Utils;

with Test_Utils;

package body XML_Utils_Tests
is

   use Ahven;
   use Expanders;

   -------------------------------------------------------------------------

   procedure Add_Memory
   is
      Filename : constant String := "obj/memory.xml";
      Policy   : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      XML_Utils.Add_Memory_Region
        (Policy    => Policy,
         Name      => "test",
         Address   => "16#9000_1000#",
         Size      => "16#3000#",
         Caching   => "UC",
         Alignment => "16#1000#");
      XML_Utils.Add_Memory_Region
        (Policy    => Policy,
         Name      => "noaddress",
         Address   => "",
         Size      => "16#8000#",
         Caching   => "WC",
         Alignment => "16#0020_0000#");

      Muxml.Write (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => Filename);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Filename,
               Filename2 => "data/memory.ref.xml"),
              Message   => "Policy mismatch");

      Ada.Directories.Delete_File (Name => Filename);
   end Add_Memory;

   -------------------------------------------------------------------------

   procedure Add_Memory_With_File
   is
      Filename : constant String := "obj/memory_with_file.xml";
      Policy   : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      XML_Utils.Add_Memory_Region
        (Policy      => Policy,
         Name        => "test",
         Address     => "16#2000#",
         Size        => "16#4000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         File_Name   => "testfile",
         File_Format => "bin_raw",
         File_Offset => "16#1000#");

      Muxml.Write (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => Filename);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Filename,
               Filename2 => "data/memory_with_file.ref.xml"),
              Message   => "Policy mismatch");

      Ada.Directories.Delete_File (Name => Filename);
   end Add_Memory_With_File;

   -------------------------------------------------------------------------

   procedure Calculate_PT_Size
   is
      use type Interfaces.Unsigned_64;

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_A,
                   File => "data/calculate_pt.xml");

      Assert
        (Condition => Expanders.XML_Utils.Calculate_PT_Size
           (Policy             => Policy,
            Dev_Virt_Mem_XPath => "/system/kernel/devices/device/memory",
            Virt_Mem_XPath     => "/system/kernel/memory/cpu[@id='0']/memory")
         = 16#6000#,
         Message   => "Size mismatch");
   end Calculate_PT_Size;

   -------------------------------------------------------------------------

   procedure Create_Virtual_Memory
   is
      Dom_Impl : DOM.Core.DOM_Implementation;
      Policy   : Muxml.XML_Data_Type;
      Node     : DOM.Core.Node;
      Logical  : constant String := "testl";
      Physical : constant String := "testp";
      Address  : constant String := "16#2000#";
   begin
      Policy.Doc := DOM.Core.Create_Document (Implementation => Dom_Impl);

      Node := XML_Utils.Create_Virtual_Memory_Node
        (Policy        => Policy,
         Logical_Name  => Logical,
         Physical_Name => Physical,
         Address       => Address,
         Writable      => True,
         Executable    => False);

      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "logical") = Logical,
              Message   => "Logical name mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "physical") = "testp",
              Message   => "Physical name mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "virtualAddress") = Address,
              Message   => "Address mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "writable") = "true",
              Message   => "Writable mismatch");
      Assert (Condition => DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => "executable") = "false",
              Message   => "Executable mismatch");
   end Create_Virtual_Memory;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "XML utility tests");
      T.Add_Test_Routine
        (Routine => Add_Memory'Access,
         Name    => "Add memory region");
      T.Add_Test_Routine
        (Routine => Add_Memory_With_File'Access,
         Name    => "Add memory region with file content");
      T.Add_Test_Routine
        (Routine => Create_Virtual_Memory'Access,
         Name    => "Create virtual memory node");
      T.Add_Test_Routine
        (Routine => Calculate_PT_Size'Access,
         Name    => "Calculate size of paging structures");
      T.Add_Test_Routine
        (Routine => Remove_Child'Access,
         Name    => "Remove XML child node");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Remove_Child
   is
      Node     : DOM.Core.Node;
      Dom_Impl : DOM.Core.DOM_Implementation;
      Doc      : constant DOM.Core.Document
        := DOM.Core.Create_Document (Implementation => Dom_Impl);
   begin
      Node := DOM.Core.Documents.Create_Element
        (Doc      => Doc,
         Tag_Name => "elem");

      XML_Utils.Append_Child (Node      => Doc,
                              New_Child => Node);

      Assert (Condition => DOM.Core.Nodes.Has_Child_Nodes (N => Doc),
              Message   => "Unable to add child to document");

      XML_Utils.Remove_Child (Node       => Doc,
                              Child_Name => "elem");

      Assert (Condition => not DOM.Core.Nodes.Has_Child_Nodes (N => Doc),
              Message   => "Error removing child node");

      begin
         XML_Utils.Remove_Child (Node       => Doc,
                                 Child_Name => "elem");
         Fail (Message => "Exception expected");

      exception
         when E : XML_Utils.XML_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Unable to remove child 'elem' from node '#document'",
                    Message   => "Exception mismatch");
      end;
   end Remove_Child;

end XML_Utils_Tests;
