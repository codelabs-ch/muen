--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Sinfo.Interop.Test_Data.

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
package body Sinfo.Interop.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Name_To_C (Gnattest_T : in out Test);
   procedure Test_Name_To_C_52e1d1 (Gnattest_T : in out Test) renames Test_Name_To_C;
--  id:2.2/52e1d1e1b9ecb40f/Name_To_C/1/0/
   procedure Test_Name_To_C (Gnattest_T : in out Test) is
   --  sinfo-interop.ads:24:4:Name_To_C
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Str : constant String (Musinfo.Name_Index_Type) := (others => 'a');
   begin
      Assert (Condition => C_Imports.C_Assert_Name
              (Name => Utils.Create_Name (Str => Ref_Str)'Address) = 1,
              Message   => "C name mismatch");
--  begin read only
   end Test_Name_To_C;
--  end read only


--  begin read only
   procedure Test_Memregion_To_C (Gnattest_T : in out Test);
   procedure Test_Memregion_To_C_0eef56 (Gnattest_T : in out Test) renames Test_Memregion_To_C;
--  id:2.2/0eef565aa0cc57a2/Memregion_To_C/1/0/
   procedure Test_Memregion_To_C (Gnattest_T : in out Test) is
   --  sinfo-interop.ads:27:4:Memregion_To_C
--  end read only

      pragma Unreferenced (Gnattest_T);

      M : Musinfo.Memregion_Type
        := (Content => Musinfo.Content_Fill,
            Address => 16#dead_beef_cafe_feed#,
            Size    => 16#8080_abab_cdcd_9000#,
            Hash       => (others => 253),
            Flags   => (Writable   => True,
                        Executable => True,
                        Channel    => True,
                        Padding    => 0),
            Pattern => 45,
            Padding => 0);
   begin
      Assert (Condition => C_Imports.C_Assert_Memregion
              (Memregion => M'Address) = 1,
              Message   => "C memregion mismatch");
--  begin read only
   end Test_Memregion_To_C;
--  end read only


--  begin read only
   procedure Test_Device_To_C (Gnattest_T : in out Test);
   procedure Test_Device_To_C_3846bf (Gnattest_T : in out Test) renames Test_Device_To_C;
--  id:2.2/3846bf9a817bff99/Device_To_C/1/0/
   procedure Test_Device_To_C (Gnattest_T : in out Test) is
   --  sinfo-interop.ads:30:4:Device_To_C
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => C_Imports.C_Assert_Device
              (Device => Ref_Dev.Dev_Data'Address) = 1,
              Message   => "C device mismatch");
--  begin read only
   end Test_Device_To_C;
--  end read only


--  begin read only
   procedure Test_Resource_To_C (Gnattest_T : in out Test);
   procedure Test_Resource_To_C_c62103 (Gnattest_T : in out Test) renames Test_Resource_To_C;
--  id:2.2/c6210377fb6885ae/Resource_To_C/1/0/
   procedure Test_Resource_To_C (Gnattest_T : in out Test) is
   --  sinfo-interop.ads:33:4:Resource_To_C
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => C_Imports.C_Assert_Resource
              (Resource => Ref_Dev'Address) = 1,
              Message   => "C resource mismatch");
--  begin read only
   end Test_Resource_To_C;
--  end read only


--  begin read only
   procedure Test_Subject_Info_To_C (Gnattest_T : in out Test);
   procedure Test_Subject_Info_To_C_3471da (Gnattest_T : in out Test) renames Test_Subject_Info_To_C;
--  id:2.2/3471dabca4420d92/Subject_Info_To_C/1/0/
   procedure Test_Subject_Info_To_C (Gnattest_T : in out Test) is
   --  sinfo-interop.ads:36:4:Subject_Info_To_C
--  end read only

      pragma Unreferenced (Gnattest_T);

      Info : Musinfo.Subject_Info_Type
        := Constants.Null_Subject_Info;
   begin
      Info.Name    := Ref_Name;
      Info.TSC_Khz := Musinfo.TSC_Tick_Rate_Khz_Type'Last;

      for I in Musinfo.Resource_Index_Type loop
         Utils.Append_Resource
           (Info     => Info,
            Resource => Ref_Dev);
      end loop;

      Assert (Condition => C_Imports.C_Assert_Subject_Info
              (Info => Info'Address) = 1,
              Message   => "C subject info mismatch");
--  begin read only
   end Test_Subject_Info_To_C;
--  end read only


--  begin read only
   procedure Test_Check_Name_Type (Gnattest_T : in out Test);
   procedure Test_Check_Name_Type_3e54f1 (Gnattest_T : in out Test) renames Test_Check_Name_Type;
--  id:2.2/3e54f1454c3de673/Check_Name_Type/1/0/
   procedure Test_Check_Name_Type (Gnattest_T : in out Test) is
   --  sinfo-interop.ads:39:4:Check_Name_Type
--  end read only

      pragma Unreferenced (Gnattest_T);

      Dummy : Musinfo.Name_Type;
   begin
      Assert (Condition => C_Imports.C_Assert_Name_Type
              (Size          => Musinfo.Name_Type'Size / 8,
               Alignment     => Musinfo.Name_Type'Alignment,
               Length_Offset => Dummy.Length'Bit_Position / 8,
               Data_Offset   => Dummy.Data'Bit_Position / 8) = 1,
              Message   => "C name type mismatch");
--  begin read only
   end Test_Check_Name_Type;
--  end read only


--  begin read only
   procedure Test_Check_Memregion_Type (Gnattest_T : in out Test);
   procedure Test_Check_Memregion_Type_0f8f2f (Gnattest_T : in out Test) renames Test_Check_Memregion_Type;
--  id:2.2/0f8f2f7565e4b586/Check_Memregion_Type/1/0/
   procedure Test_Check_Memregion_Type (Gnattest_T : in out Test) is
   --  sinfo-interop.ads:42:4:Check_Memregion_Type
--  end read only

      pragma Unreferenced (Gnattest_T);

      Dummy : Musinfo.Memregion_Type := Musinfo.Null_Memregion;
   begin
      Assert (Condition => C_Imports.C_Assert_Memregion_Type
              (Size           => Musinfo.Memregion_Type'Size / 8,
               Content_Offset => Dummy.Content'Bit_Position / 8,
               Address_Offset => Dummy.Address'Bit_Position / 8,
               Size_Offset    => Dummy.Size'Bit_Position / 8,
               Hash_Offset    => Dummy.Hash'Bit_Position / 8,
               Flags_Offset   => Dummy.Flags'Bit_Position / 8,
               Pattern_Offset => Dummy.Pattern'Bit_Position / 8) = 1,
              Message   => "C memregion type mismatch");
--  begin read only
   end Test_Check_Memregion_Type;
--  end read only


--  begin read only
   procedure Test_Check_Resource_Type (Gnattest_T : in out Test);
   procedure Test_Check_Resource_Type_35d4af (Gnattest_T : in out Test) renames Test_Check_Resource_Type;
--  id:2.2/35d4afdd5e3ed28f/Check_Resource_Type/1/0/
   procedure Test_Check_Resource_Type (Gnattest_T : in out Test) is
   --  sinfo-interop.ads:45:4:Check_Resource_Type
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert
        (Condition => C_Imports.C_Assert_Resource_Type
           (Size        => Musinfo.Resource_Type'Size / 8,
            Alignment   => Musinfo.Resource_Type'Alignment,
            Name_Offset => Ref_Dev.Name'Bit_Position / 8,
            Data_Offset => Ref_Dev.Dev_Data'Bit_Position / 8) = 1,
         Message   => "C resource type mismatch");
--  begin read only
   end Test_Check_Resource_Type;
--  end read only


--  begin read only
   procedure Test_Check_Device_Type (Gnattest_T : in out Test);
   procedure Test_Check_Device_Type_c9d3d2 (Gnattest_T : in out Test) renames Test_Check_Device_Type;
--  id:2.2/c9d3d2e7098a4233/Check_Device_Type/1/0/
   procedure Test_Check_Device_Type (Gnattest_T : in out Test) is
   --  sinfo-interop.ads:48:4:Check_Device_Type
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert
        (Condition => C_Imports.C_Assert_Device_Type
           (Size              => Musinfo.Device_Type'Size / 8,
            IRTE_Start_Offset => Ref_Dev.Dev_Data.IRTE_Start'Bit_Position / 8,
            IRQ_Start_Offset  => Ref_Dev.Dev_Data.IRQ_Start'Bit_Position / 8,
            IR_Count_Offset   => Ref_Dev.Dev_Data.IR_Count'Bit_Position / 8,
            Flags_Offset      => Ref_Dev.Dev_Data.Flags'Bit_Position / 8) = 1,
         Message   => "C device type mismatch");
--  begin read only
   end Test_Check_Device_Type;
--  end read only


--  begin read only
   procedure Test_Check_Subject_Info_Type (Gnattest_T : in out Test);
   procedure Test_Check_Subject_Info_Type_659906 (Gnattest_T : in out Test) renames Test_Check_Subject_Info_Type;
--  id:2.2/659906a031093bd7/Check_Subject_Info_Type/1/0/
   procedure Test_Check_Subject_Info_Type (Gnattest_T : in out Test) is
   --  sinfo-interop.ads:51:4:Check_Subject_Info_Type
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.C.int;

      Dummy : Musinfo.Subject_Info_Type;
   begin
      Assert
        (Condition => C_Imports.C_Assert_Subject_Info_Type
           (Size             => Musinfo.Subject_Info_Type'Size / 8,
            Alignment        => Musinfo.Subject_Info_Type'Alignment,
            Magic_Offset     => Dummy.Magic'Bit_Position / 8,
            TSC_Khz_Offset   => Dummy.TSC_Khz'Bit_Position / 8,
            Name_Offset      => Dummy.Name'Bit_Position / 8,
            Res_Count_Offset => Dummy.Resource_Count'Bit_Position / 8,
            Resources_Offset => Dummy.Resources'Bit_Position / 8) = 1,
         Message   => "C subject info type mismatch");
--  begin read only
   end Test_Check_Subject_Info_Type;
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
end Sinfo.Interop.Test_Data.Tests;
