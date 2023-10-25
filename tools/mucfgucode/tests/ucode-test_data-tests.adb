--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Ucode.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with Ada.Directories;
with DOM.Core.Nodes;
with DOM.Core.Elements;
with McKae.XML.XPath.XIA;
with Muxml;
with Test_Utils;
--  begin read only
--  end read only
package body Ucode.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_e5a2dd (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/e5a2dd86b12d7902/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Positive
      is
         Out_Dir : constant String := "obj/run";
      begin
         if not Ada.Directories.Exists (Name => Out_Dir) then
            Ada.Directories.Create_Directory (New_Directory => Out_Dir);
         end if;
         Ada.Directories.Copy_File
           (Source_Name => "data/test_policy.xml",
            Target_Name => "obj/test_policy.xml");
         --  Dummy file, should be replaced with real ucode.
         Ada.Directories.Copy_File
           (Source_Name => "data/test_policy.xml",
            Target_Name => "obj/run/f68");

         Run (Policy     => "obj/test_policy.xml",
              Ucode_Dir  => "data/ucode",
              Output_Dir => Out_Dir); 

         Assert (Condition => Ada.Directories.Exists 
                   (Name => Out_Dir & "/f68"),
                 Message   => "MCU not copied");

         declare
            Data  : Muxml.XML_Data_Type;
            Nodes : DOM.Core.Node_List;
            Node  : DOM.Core.Node;
         begin
            Muxml.Parse (Data => Data,
                         Kind => Muxml.Format_Src,
                         File => "obj/test_policy.xml");
            Nodes := McKae.XML.XPath.XIA.XPath_Query
              (N     => Data.Doc,
               XPath => "/system/memory/memory[@type='kernel_microcode']"); 
            Assert (Condition => DOM.Core.Nodes.Length (Nodes) = 1,
                    Message   => "One memory node expected");
            Node := DOM.Core.Nodes.Item (List => Nodes, Index => 0);
            Assert (Condition => DOM.Core.Elements.Get_Attribute 
                     (Elem => Node,
                      Name => "size") = "16#1000#",
                    Message => "Size mismatch");
            Assert (Condition => DOM.Core.Nodes.Length
                     (McKae.XML.XPath.XIA.XPath_Query
                       (N => Node, XPath => "file[@filename='f68']")) = 1, 
                    Message   => "Filename node mismatch");
            Assert (Condition => Test_Utils.Equal_Files
                    (Filename1 => "data/ucode/microcode",
                     Filename2 => Out_Dir & "/f68"),
                    Message   => "Microcode file invalid");
         end;

         Ada.Directories.Delete_Tree (Directory => Out_Dir);
      end Positive;

   begin
      Positive;
--  begin read only
   end Test_Run;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Ucode.Test_Data.Tests;
