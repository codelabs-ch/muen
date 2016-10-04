--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Sinfo.Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Sinfo.Utils.Test_Data.Tests is


--  begin read only
   procedure Test_To_Hash (Gnattest_T : in out Test);
   procedure Test_To_Hash_96113d (Gnattest_T : in out Test) renames Test_To_Hash;
--  id:2.2/96113d1af88939fd/To_Hash/1/0/
   procedure Test_To_Hash (Gnattest_T : in out Test) is
   --  sinfo-utils.ads:27:4:To_Hash
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Musinfo.Hash_Type;
   begin
      Assert (Condition => To_Hash
              (Hex => "16#e10e6c3b5f7e8eb8a5510b8562ed449100a856e02a31a27d30bf"
               & "5e76efd91235#") = Ref_Hash,
              Message   => "Hash mismatch");
--  begin read only
   end Test_To_Hash;
--  end read only


--  begin read only
   procedure Test_Get_Memory_Info (Gnattest_T : in out Test);
   procedure Test_Get_Memory_Info_f5dae6 (Gnattest_T : in out Test) renames Test_Get_Memory_Info;
--  id:2.2/f5dae60bb4749254/Get_Memory_Info/1/0/
   procedure Test_Get_Memory_Info (Gnattest_T : in out Test) is
   --  sinfo-utils.ads:35:4:Get_Memory_Info
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Musinfo.Memregion_Type;

      V_Path1 : constant String := "/system/subjects/subject[@id='0']/memory/"
        & "memory[@logical='acpi_rsdp']";
      P_Path1 : constant String := "/system/memory/memory"
        & "[@name='lnx|acpi_rsdp']";
      V_Path2 : constant String := "/system/subjects/subject[@id='0']/memory/"
        & "memory[@logical='keyboard']";
      P_Path2 : constant String := "/system/memory/memory"
        & "[@name='lnx_keyboard']";
      Policy  : Muxml.XML_Data_Type;
      Ref1    : constant Musinfo.Memregion_Type
        := Musinfo.Utils.Create_Memregion
          (Content    => Musinfo.Content_File,
           Address    => 16#1000_0000#,
           Size       => 16#1000#,
           Hash       => Utils.Test_Data.Ref_Hash,
           Writable   => False,
           Executable => False);
      Ref2    : constant Musinfo.Memregion_Type
        := Musinfo.Utils.Create_Memregion
          (Content    => Musinfo.Content_Fill,
           Address    => 16#ffff_e000#,
           Size       => 16#1000#,
           Hash       => Musinfo.No_Hash,
           Pattern    => 34,
           Writable   => False,
           Executable => False);
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Assert (Condition => Get_Memory_Info
              (Virt_Mem_Node => Muxml.Utils.Get_Element
               (Doc   => Policy.Doc,
                XPath => V_Path1),
               Phys_Mem_Node => Muxml.Utils.Get_Element
                 (Doc   => Policy.Doc,
                  XPath => P_Path1)) = Ref1,
              Message   => "Memregion mismatch (1)");
      Assert (Condition => Get_Memory_Info
              (Virt_Mem_Node => Muxml.Utils.Get_Element
               (Doc   => Policy.Doc,
                XPath => V_Path2),
               Phys_Mem_Node => Muxml.Utils.Get_Element
                 (Doc   => Policy.Doc,
                  XPath => P_Path2)) = Ref2,
              Message   => "Memregion mismatch (2)");
--  begin read only
   end Test_Get_Memory_Info;
--  end read only

end Sinfo.Utils.Test_Data.Tests;