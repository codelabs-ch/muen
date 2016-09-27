--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Sinfo.Generator.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Sinfo.Generator.Test_Data.Tests is


--  begin read only
   procedure Test_Write (Gnattest_T : in out Test);
   procedure Test_Write_23ab15 (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/23ab1562ae4604fa/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
   --  sinfo-generator.ads:29:4:Write
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy        : Muxml.XML_Data_Type;
      Subject_Sinfo : constant String := "obj/lnx_sinfo";
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Write (Output_Dir => "obj",
             Policy     => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/lnx_sinfo",
               Filename2 => Subject_Sinfo),
              Message   => "Subject info file mismatch");

      Ada.Directories.Delete_File (Name => Subject_Sinfo);
--  begin read only
   end Test_Write;
--  end read only


--  begin read only
   procedure Test_Get_Memory_Info (Gnattest_T : in out Test);
   procedure Test_Get_Memory_Info_f5dae6 (Gnattest_T : in out Test) renames Test_Get_Memory_Info;
--  id:2.2/f5dae60bb4749254/Get_Memory_Info/1/0/
   procedure Test_Get_Memory_Info (Gnattest_T : in out Test) is
   --  sinfo-generator.ads:37:4:Get_Memory_Info
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Musinfo.Memregion_Type;

      V_Path : constant String := "/system/subjects/subject[@id='0']/memory/"
        & "memory[@logical='acpi_rsdp']";
      P_Path : constant String := "/system/memory/memory"
        & "[@name='lnx|acpi_rsdp']";
      Policy : Muxml.XML_Data_Type;
      Ref    : constant Musinfo.Memregion_Type
        := Musinfo.Utils.Create_Memregion
          (Kind       => Musinfo.Content_Uninitialized,
           Address    => 16#1000_0000#,
           Size       => 16#1000#,
           Writable   => False,
           Executable => False);
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Assert (Condition => Get_Memory_Info
              (Virt_Mem_Node => Muxml.Utils.Get_Element
               (Doc   => Policy.Doc,
                XPath => V_Path),
               Phys_Mem_Node => Muxml.Utils.Get_Element
                 (Doc   => Policy.Doc,
                  XPath => P_Path)) = Ref,
              Message  => "Memregion mismatch");
--  begin read only
   end Test_Get_Memory_Info;
--  end read only

end Sinfo.Generator.Test_Data.Tests;
