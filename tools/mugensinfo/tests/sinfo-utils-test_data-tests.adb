--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Sinfo.Utils.Test_Data.

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
package body Sinfo.Utils.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Create_Name (Gnattest_T : in out Test);
   procedure Test_Create_Name_3d4022 (Gnattest_T : in out Test) renames Test_Create_Name;
--  id:2.2/3d402238b556bfe4/Create_Name/1/0/
   procedure Test_Create_Name (Gnattest_T : in out Test) is
   --  sinfo-utils.ads:31:4:Create_Name
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Musinfo.Name_Type;

      Ref_Str  : constant String   := "foobar";
      Ref_Name : Musinfo.Name_Type := Musinfo.Null_Name;
   begin
      Ref_Name.Length := Ref_Str'Length;
      Ref_Name.Data (1 .. Ref_Str'Length) := Musinfo.Name_Data_Type (Ref_Str);

      Assert (Condition => Create_Name (Str => Ref_Str) = Ref_Name,
              Message   => "Name mismatch");
--  begin read only
   end Test_Create_Name;
--  end read only


--  begin read only
   procedure Test_Create_Memregion (Gnattest_T : in out Test);
   procedure Test_Create_Memregion_8a0946 (Gnattest_T : in out Test) renames Test_Create_Memregion;
--  id:2.2/8a0946916a40866e/Create_Memregion/1/0/
   procedure Test_Create_Memregion (Gnattest_T : in out Test) is
   --  sinfo-utils.ads:36:4:Create_Memregion
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      Ref_Content : constant Musinfo.Content_Type   := Musinfo.Content_File;
      Ref_Addr    : constant Interfaces.Unsigned_64 := 16#8000_cafe_beef_0000#;
      Ref_Size    : constant Interfaces.Unsigned_64 := 16#0020_0000#;
      Memregion   : Musinfo.Memregion_Type;
   begin
      Memregion := Create_Memregion
        (Content    => Ref_Content,
         Address    => Ref_Addr,
         Size       => Ref_Size,
         Writable   => False,
         Executable => True);

      Assert (Condition => Memregion.Address = Ref_Addr,
              Message   => "Address mismatch");
      Assert (Condition => Memregion.Size = Ref_Size,
              Message   => "Size mismatch");
      Assert (Condition => not Memregion.Flags.Writable,
              Message   => "Writable");
      Assert (Condition => Memregion.Flags.Executable,
              Message   => "not executable");
--  begin read only
   end Test_Create_Memregion;
--  end read only


--  begin read only
   procedure Test_Create_Channel_Info (Gnattest_T : in out Test);
   procedure Test_Create_Channel_Info_825d0f (Gnattest_T : in out Test) renames Test_Create_Channel_Info;
--  id:2.2/825d0f1f0fd8a802/Create_Channel_Info/1/0/
   procedure Test_Create_Channel_Info (Gnattest_T : in out Test) is
   --  sinfo-utils.ads:47:4:Create_Channel_Info
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;
      use type Musinfo.Event_Number_Range;
      use type Musinfo.Vector_Range;

      Ref_Event    : constant Musinfo.Event_Number_Range := 234;
      Ref_Vector   : constant Musinfo.Vector_Range       := 123;
      Channel_Info : Musinfo.Channel_Info_Type;
   begin
      Channel_Info := Create_Channel_Info
        (Has_Event  => True,
         Has_Vector => False,
         Event      => Ref_Event,
         Vector     => Ref_Vector);

      Assert (Condition => Channel_Info.Flags.Has_Event,
              Message   => "Has no event");
      Assert (Condition => not Channel_Info.Flags.Has_Vector,
              Message   => "Has vector");
      Assert (Condition => Channel_Info.Event = Ref_Event,
              Message   => "Event mismatch");
      Assert (Condition => Channel_Info.Vector = Ref_Vector,
              Message   => "Vector mismatch");
--  begin read only
   end Test_Create_Channel_Info;
--  end read only


--  begin read only
   procedure Test_Create_Resource (Gnattest_T : in out Test);
   procedure Test_Create_Resource_ed1649 (Gnattest_T : in out Test) renames Test_Create_Resource;
--  id:2.2/ed1649f963c2d696/Create_Resource/1/0/
   procedure Test_Create_Resource (Gnattest_T : in out Test) is
   --  sinfo-utils.ads:55:4:Create_Resource
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Musinfo.Name_Type;

      Ref_Name     : constant Musinfo.Name_Type := Create_Name (Str => "bar");
      Ref_Mem_Idx  : constant Musinfo.Resource_Index_Type := 5;
      Ref_Chan_Idx : constant Musinfo.Resource_Index_Type := 128;
      Resource     : Musinfo.Resource_Type;
   begin
      Resource := Create_Resource
        (Name               => Ref_Name,
         Memregion_Index    => Ref_Mem_Idx,
         Channel_Info_Index => Ref_Chan_Idx);

      Assert (Condition => Resource.Name = Ref_Name,
              Message   => "Name mismatch");
      Assert (Condition => Resource.Memregion_Idx = Ref_Mem_Idx,
              Message   => "Memregion index mismatch");
      Assert (Condition => Resource.Channel_Info_Idx = Ref_Chan_Idx,
              Message   => "Channel info index mismatch");
--  begin read only
   end Test_Create_Resource;
--  end read only


--  begin read only
   procedure Test_Create_Dev_Info (Gnattest_T : in out Test);
   procedure Test_Create_Dev_Info_1ebbad (Gnattest_T : in out Test) renames Test_Create_Dev_Info;
--  id:2.2/1ebbada1bbe1fbe3/Create_Dev_Info/1/0/
   procedure Test_Create_Dev_Info (Gnattest_T : in out Test) is
   --  sinfo-utils.ads:62:4:Create_Dev_Info
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_8;
      use type Interfaces.Unsigned_16;

      Dev_Info : Musinfo.Dev_Info_Type;
   begin
      Dev_Info := Create_Dev_Info
        (SID         => 128,
         IRTE_Start  => 56,
         IRQ_Start   => 0,
         IR_Count    => 34,
         MSI_Capable => False);

      Assert (Condition => Dev_Info.SID = 128,
              Message   => "SID mismatch");
      Assert (Condition => Dev_Info.IRTE_Start = 56,
              Message   => "IRTE start mismatch");
      Assert (Condition => Dev_Info.IRQ_Start = 0,
              Message   => "IRQ start mismatch");
      Assert (Condition => Dev_Info.IR_Count = 34,
              Message   => "IR count mismatch");
      Assert (Condition => Dev_Info.Flags.MSI_Capable = False,
              Message   => "MSI capable");
--  begin read only
   end Test_Create_Dev_Info;
--  end read only


--  begin read only
   procedure Test_Append_Memregion (Gnattest_T : in out Test);
   procedure Test_Append_Memregion_089428 (Gnattest_T : in out Test) renames Test_Append_Memregion;
--  id:2.2/089428f9e0c5b3e3/Append_Memregion/1/0/
   procedure Test_Append_Memregion (Gnattest_T : in out Test) is
   --  sinfo-utils.ads:71:4:Append_Memregion
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      Ref_Name   : constant Musinfo.Name_Type := Create_Name (Str => "foo");
      Ref_Addr   : constant Interfaces.Unsigned_64 := 16#cafe_feed_cafe_0000#;
      Ref_Size   : constant Interfaces.Unsigned_64 := 16#0042_2300#;

      Info : Musinfo.Subject_Info_Type := Constants.Null_Subject_Info;
   begin
      Assert (Condition => Info.Resource_Count
              = Musinfo.Resource_Count_Type'First,
              Message   => "Resource present");
      Assert (Condition => Info.Memregion_Count
              = Musinfo.Resource_Count_Type'First,
              Message   => "Memregion present");
      Assert (Condition => Info.Channel_Info_Count
              = Musinfo. Resource_Count_Type'First,
              Message   => "Channel info present");

      Append_Memregion
        (Info   => Info,
         Name   => Ref_Name,
         Region => Create_Memregion
           (Content    => Musinfo.Content_Uninitialized,
            Address    => Ref_Addr,
            Size       => Ref_Size,
            Writable   => True,
            Executable => False));

      Assert (Condition => Info.Resource_Count = 1,
              Message   => "Resource not appended");
      Assert (Condition => Info.Memregion_Count = 1,
              Message   => "Memregion not appended");
      Assert (Condition => Info.Channel_Info_Count
              = Musinfo.Resource_Count_Type'First,
              Message   => "Channel info appended");

      declare
         use type Musinfo.Name_Type;

         Resource  : constant Musinfo.Resource_Type  := Info.Resources (1);
         Memregion : constant Musinfo.Memregion_Type := Info.Memregions (1);
      begin
         Assert (Condition => Resource.Name = Ref_Name,
                 Message   => "Name mismatch");
         Assert (Condition => Resource.Memregion_Idx = 1,
                 Message   => "Memregion index mismatch");
         Assert (Condition => Resource.Channel_Info_Idx = Musinfo.No_Resource,
                 Message   => "Channel info index mismatch");

         Assert (Condition => Memregion.Address = Ref_Addr,
                 Message   => "Address mismatch");
         Assert (Condition => Memregion.Size = Ref_Size,
                 Message   => "Size mismatch");
         Assert (Condition => Memregion.Flags.Writable,
                 Message   => "Not writable");
         Assert (Condition => not Memregion.Flags.Executable,
                 Message   => "Executable");
      end;

      Append_Memregion
        (Info   => Info,
         Name   => Ref_Name,
         Region => Create_Memregion
           (Content    => Musinfo.Content_Uninitialized,
            Address    => Ref_Addr,
            Size       => Ref_Size,
            Writable   => True,
            Executable => False));

      Assert (Condition => Info.Resource_Count = 2,
              Message   => "Resource not appended (2)");
      Assert (Condition => Info.Memregion_Count = 2,
              Message   => "Memregion not appended (2)");
      Assert (Condition => Info.Channel_Info_Count
              = Musinfo.Resource_Count_Type'First,
              Message   => "Channel info appended (2)");
--  begin read only
   end Test_Append_Memregion;
--  end read only


--  begin read only
   procedure Test_Append_Channel (Gnattest_T : in out Test);
   procedure Test_Append_Channel_f55fe6 (Gnattest_T : in out Test) renames Test_Append_Channel;
--  id:2.2/f55fe65f9d02c597/Append_Channel/1/0/
   procedure Test_Append_Channel (Gnattest_T : in out Test) is
   --  sinfo-utils.ads:81:4:Append_Channel
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      Ref_Name   : constant Musinfo.Name_Type := Create_Name (Str => "foo");
      Ref_Addr   : constant Interfaces.Unsigned_64 := 16#8000_cafe_beef_0000#;
      Ref_Size   : constant Interfaces.Unsigned_64 := 16#2000#;
      Ref_Event  : constant Musinfo.Event_Number_Range := 234;
      Ref_Vector : constant Musinfo.Vector_Range       := 123;

      Info : Musinfo.Subject_Info_Type := Constants.Null_Subject_Info;
   begin
      Assert (Condition => Info.Channel_Info_Count
              = Musinfo.Resource_Count_Type'First,
              Message   => "Channel present");

      Append_Channel (Info    => Info,
                      Name       => Ref_Name,
                      Memregion  => Utils.Create_Memregion
                        (Content    => Musinfo.Content_Uninitialized,
                         Address    => Ref_Addr,
                         Size       => Ref_Size,
                         Writable   => True,
                         Executable => False),
                      Has_Event  => False,
                      Has_Vector => True,
                      Event      => Ref_Event,
                      Vector     => Ref_Vector);

      Assert (Condition => Info.Resource_Count = 1,
              Message   => "Resource not appended");
      Assert (Condition => Info.Memregion_Count = 1,
              Message   => "Memregion not appended");
      Assert (Condition => Info.Channel_Info_Count = 1,
              Message   => "Channel not appended");

      declare
         use type Musinfo.Name_Type;
         use type Musinfo.Event_Number_Range;
         use type Musinfo.Vector_Range;

         Resource     : constant Musinfo.Resource_Type
           := Info.Resources (1);
         Channel_Info : constant Musinfo.Channel_Info_Type
           := Info.Channels_Info (1);
         Memregion    : constant Musinfo.Memregion_Type
           := Info.Memregions (1);
      begin
         Assert (Condition => Resource.Name = Ref_Name,
                 Message   => "Name mismatch");
         Assert (Condition => Resource.Memregion_Idx = 1,
                 Message   => "Memregion index mismatch");
         Assert (Condition => Resource.Channel_Info_Idx = 1,
                 Message   => "Channel info index mismatch");

         Assert (Condition => Memregion.Address = Ref_Addr,
                 Message   => "Address mismatch");
         Assert (Condition => Memregion.Size = Ref_Size,
                 Message   => "Size mismatch");
         Assert (Condition => Memregion.Flags.Writable,
                 Message   => "Not writable");
         Assert (Condition => not Memregion.Flags.Executable,
                 Message   => "Executable");

         Assert (Condition => not Channel_Info.Flags.Has_Event,
                 Message   => "Has event");
         Assert (Condition => Channel_Info.Flags.Has_Vector,
                 Message   => "Has no vector");
         Assert (Condition => Channel_Info.Event = Ref_Event,
                 Message   => "Event mismatch");
         Assert (Condition => Channel_Info.Vector = Ref_Vector,
                 Message   => "Vector mismatch");
      end;

      Append_Channel (Info    => Info,
                      Name       => Ref_Name,
                      Memregion  => Utils.Create_Memregion
                        (Content    => Musinfo.Content_Uninitialized,
                         Address    => Ref_Addr,
                         Size       => Ref_Size,
                         Writable   => True,
                         Executable => False),
                      Has_Event  => False,
                      Has_Vector => True,
                      Event      => Ref_Event,
                      Vector     => Ref_Vector);
      Assert (Condition => Info.Resource_Count = 2,
              Message   => "Resource not appended (2)");
      Assert (Condition => Info.Memregion_Count = 2,
              Message   => "Memregion not appended (2)");
      Assert (Condition => Info.Channel_Info_Count = 2,
              Message   => "Channel not appended (2)");
--  begin read only
   end Test_Append_Channel;
--  end read only


--  begin read only
   procedure Test_Append_Dev (Gnattest_T : in out Test);
   procedure Test_Append_Dev_b8cd61 (Gnattest_T : in out Test) renames Test_Append_Dev;
--  id:2.2/b8cd6115c6595659/Append_Dev/1/0/
   procedure Test_Append_Dev (Gnattest_T : in out Test) is
   --  sinfo-utils.ads:97:4:Append_Dev
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_8;
      use type Interfaces.Unsigned_16;

      Ref_SID        : constant := 245;
      Ref_IRTE_Start : constant := 12;
      Ref_IRQ_Start  : constant := 33;
      Ref_IR_Count   : constant := 245;
      Ref_MSI_Cap    : constant Boolean := True;

      Info : Musinfo.Subject_Info_Type := Constants.Null_Subject_Info;
   begin
      Assert (Condition => Info.Dev_Info_Count
              = Musinfo.Resource_Count_Type'First,
              Message   => "Dev present");

      Append_Dev (Info       => Info,
                  SID         => Ref_SID,
                  IRTE_Start  => Ref_IRTE_Start,
                  IRQ_Start   => Ref_IRQ_Start,
                  IR_Count    => Ref_IR_Count,
                  MSI_Capable => Ref_MSI_Cap);

      Assert (Condition => Info.Dev_Info_Count = 1,
              Message   => "Dev not appended");

      declare
         Dev : constant Musinfo.Dev_Info_Type := Info.Dev_Info (1);
      begin
         Assert (Condition => Dev.SID = Ref_SID,
                 Message   => "SID mismatch");
         Assert (Condition => Dev.IRTE_Start = Ref_IRTE_Start,
                 Message   => "IRTE start mismatch");
         Assert (Condition => Dev.IRQ_Start = Ref_IRQ_Start,
                 Message   => "IRQ start mismatch");
         Assert (Condition => Dev.IR_Count = Ref_IR_Count,
                 Message   => "IR count mismatch");
         Assert (Condition => Dev.Flags.MSI_Capable = Ref_MSI_Cap,
                 Message   => "MSI flag mismatch");
      end;
--  begin read only
   end Test_Append_Dev;
--  end read only


--  begin read only
   procedure Test_To_Hash (Gnattest_T : in out Test);
   procedure Test_To_Hash_96113d (Gnattest_T : in out Test) renames Test_To_Hash;
--  id:2.2/96113d1af88939fd/To_Hash/1/0/
   procedure Test_To_Hash (Gnattest_T : in out Test) is
   --  sinfo-utils.ads:109:4:To_Hash
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
   --  sinfo-utils.ads:117:4:Get_Memory_Info
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Musinfo.Memregion_Type;

      V_Path1 : constant String := "/system/subjects/subject[@globalId='0']"
        & "/memory/memory[@logical='acpi_rsdp']";
      P_Path1 : constant String := "/system/memory/memory"
        & "[@name='lnx|acpi_rsdp']";
      V_Path2 : constant String := "/system/subjects/subject[@globalId='0']"
        & "/memory/memory[@logical='keyboard']";
      P_Path2 : constant String := "/system/memory/memory"
        & "[@name='lnx_keyboard']";
      Policy  : Muxml.XML_Data_Type;
      Ref1    : constant Musinfo.Memregion_Type
        := Create_Memregion
          (Content    => Musinfo.Content_File,
           Address    => 16#1000_0000#,
           Size       => 16#1000#,
           Hash       => Utils.Test_Data.Ref_Hash,
           Writable   => False,
           Executable => False);
      Ref2    : constant Musinfo.Memregion_Type
        := Create_Memregion
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
end Sinfo.Utils.Test_Data.Tests;
