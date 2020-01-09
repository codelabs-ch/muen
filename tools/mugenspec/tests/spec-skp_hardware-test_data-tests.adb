--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Spec.Skp_Hardware.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Spec.Skp_Hardware.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Write (Gnattest_T : in out Test);
   procedure Test_Write_23ab15 (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/23ab1562ae4604fa/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy     : Muxml.XML_Data_Type;
      Output_Dir : constant String := "obj";
      Spec       : constant String := Output_Dir & "/skp-hardware.ads";
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Write (Output_Dir => Output_Dir,
             Policy     => Policy);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Spec,
               Filename2 => "data/skp-hardware.ads"),
              Message   => "Hardware spec mismatch");
      Ada.Directories.Delete_File (Name => Spec);

      declare
         Dbg_Console : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Policy.Doc,
              XPath => "/system/kernel/devices/device"
              & "[@logical='debugconsole']");
         Mem_Node : constant DOM.Core.Node
           := Mutools.XML_Utils.Create_Virtual_Memory_Node
             (Policy        => Policy,
              Logical_Name  => "memory",
              Physical_Name => "buffer",
              Address       => "16#0030_0000#",
              Writable      => True,
              Executable    => False);
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => Dbg_Console,
            Name  => "physical",
            Value => "vga");
         Muxml.Utils.Insert_Before
           (Parent    => Dbg_Console,
            New_Child => Mem_Node,
            Ref_Names  =>
              (1 => Ada.Strings.Unbounded.To_Unbounded_String ("ioPort")));
         Muxml.Utils.Set_Attribute
           (Doc   => Dbg_Console,
            XPath => "ioPort",
            Name  => "physical",
            Value => "ports");
      end;

      Write (Output_Dir => Output_Dir,
             Policy     => Policy);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Spec,
               Filename2 => "data/skp-hardware_vga.ref"),
              Message   => "Hardware spec mismatch (vga)");

      Ada.Directories.Delete_File (Name => Spec);
--  begin read only
   end Test_Write;
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
end Spec.Skp_Hardware.Test_Data.Tests;
