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
   --  sinfo-utils.ads:27:4:Create_Name
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
   procedure Test_Append_Resource (Gnattest_T : in out Test);
   procedure Test_Append_Resource_1d327f (Gnattest_T : in out Test) renames Test_Append_Resource;
--  id:2.2/1d327faf104b58ed/Append_Resource/1/0/
   procedure Test_Append_Resource (Gnattest_T : in out Test) is
   --  sinfo-utils.ads:33:4:Append_Resource
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_8;
      use type Interfaces.Unsigned_16;
      use type Musinfo.Resource_Kind;
      use type Musinfo.Name_Type;

      Sinfo : Musinfo.Subject_Info_Type := Constants.Null_Subject_Info;
   begin
      Assert (Condition => Sinfo.Resources (1).Kind = Musinfo.Res_None,
              Message   => "Resource already present");
      Assert (Condition => Sinfo.Resource_Count = 0,
              Message   => "Resource count not 0");

      Append_Resource (Info     => Sinfo,
                       Resource => (Kind     => Musinfo.Res_Event,
                                    Name     => Create_Name (Str => "Evt1"),
                                    Evt_Data => (Value   => 12,
                                                 Padding => (others => 0)),
                                    Padding  => (others => 0)));

      Assert (Condition => Sinfo.Resource_Count = 1,
              Message   => "Resource count not 1");

      declare
         R : constant Musinfo.Resource_Type := Sinfo.Resources (1);
      begin
         Assert (Condition => R.Kind = Musinfo.Res_Event,
                 Message   => "Kind mismatch (1)");
         Assert (Condition => R.Name = Create_Name (Str => "Evt1"),
                 Message   => "Name mismatch (1)");
         Assert (Condition => R.Evt_Data.Value = 12,
                 Message   => "Event number mismatch (1)");
      end;

      Append_Resource (Info     => Sinfo,
                       Resource => (Kind     => Musinfo.Res_Vector,
                                    Name     => Create_Name (Str => "Vec1"),
                                    Vec_Data => (Value   => 13,
                                                 Padding => (others => 0)),
                                    Padding  => (others => 0)));

      Assert (Condition => Sinfo.Resource_Count = 2,
              Message   => "Resource count not 2");

      declare
         R : Musinfo.Resource_Type := Sinfo.Resources (1);
      begin
         Assert (Condition => R.Kind = Musinfo.Res_Event,
                 Message   => "Kind mismatch (2)");
         Assert (Condition => R.Name = Create_Name (Str => "Evt1"),
                 Message   => "Name mismatch (2)");
         Assert (Condition => R.Evt_Data.Value = 12,
                 Message   => "Event number mismatch (2)");

         R := Sinfo.Resources (2);
         Assert (Condition => R.Kind = Musinfo.Res_Vector,
                 Message   => "Kind mismatch (3)");
         Assert (Condition => R.Name = Create_Name (Str => "Vec1"),
                 Message   => "Name mismatch (3)");
         Assert (Condition => R.Vec_Data.Value = 13,
                 Message   => "Vector number mismatch");
      end;

      --  Filling it up must raise an exception.

      begin
         for I in 1 .. 255 loop
            Append_Resource (Info     => Sinfo,
                             Resource =>
                               (Kind     => Musinfo.Res_Vector,
                                Name     => Create_Name (Str => "Vec1"),
                                Vec_Data => (Value   => 13,
                                             Padding => (others => 0)),
                                Padding  => (others => 0)));
         end loop;
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when Sinfo_Full => null;
      end;
--  begin read only
   end Test_Append_Resource;
--  end read only


--  begin read only
   procedure Test_To_Hash (Gnattest_T : in out Test);
   procedure Test_To_Hash_96113d (Gnattest_T : in out Test) renames Test_To_Hash;
--  id:2.2/96113d1af88939fd/To_Hash/1/0/
   procedure Test_To_Hash (Gnattest_T : in out Test) is
   --  sinfo-utils.ads:40:4:To_Hash
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
   --  sinfo-utils.ads:48:4:Get_Memory_Info
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
        := (Content => Musinfo.Content_File,
            Address => 16#1000_0000#,
            Size    => 16#1000#,
            Hash    => Utils.Test_Data.Ref_Hash,
            Flags   => (Writable   => False,
                        Executable => False,
                        Channel    => False,
                        Padding    => 0),
            Pattern => Musinfo.No_Pattern,
            Padding => 0);
      Ref2    : constant Musinfo.Memregion_Type
        := (Content => Musinfo.Content_Fill,
            Address => 16#ffff_e000#,
            Size    => 16#1000#,
            Hash    => Musinfo.No_Hash,
            Flags   => (Writable   => False,
                        Executable => False,
                        Channel    => True,
                        Padding    => 0),
            Pattern => 34,
            Padding => 0);
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
