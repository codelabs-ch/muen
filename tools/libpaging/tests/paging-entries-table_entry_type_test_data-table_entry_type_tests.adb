--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Paging.Entries.Table_Entry_Type_Test_Data.

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
package body Paging.Entries.Table_Entry_Type_Test_Data.Table_Entry_Type_Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Create (Gnattest_T : in out Test_Table_Entry_Type);
   procedure Test_Create_161e77 (Gnattest_T : in out Test_Table_Entry_Type) renames Test_Create;
--  id:2.2/161e77e82fda3da2/Create/1/0/
   procedure Test_Create (Gnattest_T : in out Test_Table_Entry_Type) is
   --  paging-entries.ads:30:4:Create
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      TEntry : Table_Entry_Type;
   begin
      TEntry := Create (Dst_Index   => 42,
                        Dst_Address => 16#1f_f000#,
                        Present     => False,
                        Readable    => True,
                        Writable    => False,
                        Executable  => True,
                        Maps_Page   => True,
                        Global      => True,
                        Caching     => Paging.WB);

      Assert (Condition => TEntry.Dst_Table_Index = 42,
              Message   => "Dst offset mismatch");
      Assert (Condition => TEntry.Dst_Address = 16#1f_f000#,
              Message   => "Dst address mismatch");
      Assert (Condition => not TEntry.Is_Present,
              Message   => "Present");
      Assert (Condition => TEntry.Readable,
              Message   => "Not readable");
      Assert (Condition => not TEntry.Writable,
              Message   => "Writable");
      Assert (Condition => TEntry.Executable,
              Message   => "Writable");
      Assert (Condition => TEntry.Maps_Page,
              Message   => "Not mapping page");
      Assert (Condition => TEntry.Global,
              Message   => "Non-global entry");
      Assert (Condition => TEntry.Caching = Paging.WB,
              Message   => "Caching type mismatch");
--  begin read only
   end Test_Create;
--  end read only


--  begin read only
   procedure Test_Get_Dst_Table_Index (Gnattest_T : in out Test_Table_Entry_Type);
   procedure Test_Get_Dst_Table_Index_3a8ec3 (Gnattest_T : in out Test_Table_Entry_Type) renames Test_Get_Dst_Table_Index;
--  id:2.2/3a8ec3d3f6058b3d/Get_Dst_Table_Index/1/0/
   procedure Test_Get_Dst_Table_Index (Gnattest_T : in out Test_Table_Entry_Type) is
   --  paging-entries.ads:44:4:Get_Dst_Table_Index
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Get_Dst_Table_Index (E => Test_Entry) = 42,
              Message   => "Dst table index mismatch");
--  begin read only
   end Test_Get_Dst_Table_Index;
--  end read only


--  begin read only
   procedure Test_Get_Dst_Address (Gnattest_T : in out Test_Table_Entry_Type);
   procedure Test_Get_Dst_Address_17828e (Gnattest_T : in out Test_Table_Entry_Type) renames Test_Get_Dst_Address;
--  id:2.2/17828eb746e595fc/Get_Dst_Address/1/0/
   procedure Test_Get_Dst_Address (Gnattest_T : in out Test_Table_Entry_Type) is
   --  paging-entries.ads:47:4:Get_Dst_Address
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;
   begin
      Assert (Condition => Get_Dst_Address (E => Test_Entry) = 16#1f_f000#,
              Message   => "Dst address mismatch");
--  begin read only
   end Test_Get_Dst_Address;
--  end read only


--  begin read only
   procedure Test_Set_Dst_Address (Gnattest_T : in out Test_Table_Entry_Type);
   procedure Test_Set_Dst_Address_c21cb9 (Gnattest_T : in out Test_Table_Entry_Type) renames Test_Set_Dst_Address;
--  id:2.2/c21cb91d98cddfe9/Set_Dst_Address/1/0/
   procedure Test_Set_Dst_Address (Gnattest_T : in out Test_Table_Entry_Type) is
   --  paging-entries.ads:52:4:Set_Dst_Address
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      TEntry : Table_Entry_Type;
   begin
      Set_Dst_Address (E       => TEntry,
                       Address => 16#9000#);
      Assert (Condition => TEntry.Dst_Address = 16#9000#,
              Message   => "Address mismatch");
--  begin read only
   end Test_Set_Dst_Address;
--  end read only


--  begin read only
   procedure Test_Is_Present (Gnattest_T : in out Test_Table_Entry_Type);
   procedure Test_Is_Present_b4af0c (Gnattest_T : in out Test_Table_Entry_Type) renames Test_Is_Present;
--  id:2.2/b4af0c02e50428f8/Is_Present/1/0/
   procedure Test_Is_Present (Gnattest_T : in out Test_Table_Entry_Type) is
   --  paging-entries.ads:57:4:Is_Present
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Is_Present (E => Test_Entry),
              Message   => "Not present");
      Assert (Condition => not Is_Present
              (E => Create (Dst_Index   => 0,
                            Dst_Address => 0,
                            Present     => False,
                            Readable    => False,
                            Writable    => False,
                            Executable  => False,
                            Maps_Page   => False,
                            Global      => False,
                            Caching     => WB)),
              Message   => "Present");
--  begin read only
   end Test_Is_Present;
--  end read only


--  begin read only
   procedure Test_Is_Readable (Gnattest_T : in out Test_Table_Entry_Type);
   procedure Test_Is_Readable_a3f083 (Gnattest_T : in out Test_Table_Entry_Type) renames Test_Is_Readable;
--  id:2.2/a3f083bb8d1d64f6/Is_Readable/1/0/
   procedure Test_Is_Readable (Gnattest_T : in out Test_Table_Entry_Type) is
   --  paging-entries.ads:61:4:Is_Readable
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Is_Readable (E => Test_Entry),
              Message   => "Not readable");
--  begin read only
   end Test_Is_Readable;
--  end read only


--  begin read only
   procedure Test_Is_Writable (Gnattest_T : in out Test_Table_Entry_Type);
   procedure Test_Is_Writable_4b4e3f (Gnattest_T : in out Test_Table_Entry_Type) renames Test_Is_Writable;
--  id:2.2/4b4e3f0710c4652e/Is_Writable/1/0/
   procedure Test_Is_Writable (Gnattest_T : in out Test_Table_Entry_Type) is
   --  paging-entries.ads:65:4:Is_Writable
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => not Is_Writable (E => Test_Entry),
              Message   => "Writable");
--  begin read only
   end Test_Is_Writable;
--  end read only


--  begin read only
   procedure Test_Is_Executable (Gnattest_T : in out Test_Table_Entry_Type);
   procedure Test_Is_Executable_3e2d63 (Gnattest_T : in out Test_Table_Entry_Type) renames Test_Is_Executable;
--  id:2.2/3e2d635bdaa67355/Is_Executable/1/0/
   procedure Test_Is_Executable (Gnattest_T : in out Test_Table_Entry_Type) is
   --  paging-entries.ads:69:4:Is_Executable
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Is_Executable (E => Test_Entry),
              Message   => "Writable");
--  begin read only
   end Test_Is_Executable;
--  end read only


--  begin read only
   procedure Test_Maps_Page (Gnattest_T : in out Test_Table_Entry_Type);
   procedure Test_Maps_Page_4c3f51 (Gnattest_T : in out Test_Table_Entry_Type) renames Test_Maps_Page;
--  id:2.2/4c3f511d997b690a/Maps_Page/1/0/
   procedure Test_Maps_Page (Gnattest_T : in out Test_Table_Entry_Type) is
   --  paging-entries.ads:72:4:Maps_Page
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Maps_Page (E => Test_Entry),
              Message   => "Not mapping page");
--  begin read only
   end Test_Maps_Page;
--  end read only


--  begin read only
   procedure Test_Is_Global (Gnattest_T : in out Test_Table_Entry_Type);
   procedure Test_Is_Global_aeaec3 (Gnattest_T : in out Test_Table_Entry_Type) renames Test_Is_Global;
--  id:2.2/aeaec3a5b655240a/Is_Global/1/0/
   procedure Test_Is_Global (Gnattest_T : in out Test_Table_Entry_Type) is
   --  paging-entries.ads:75:4:Is_Global
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Is_Global (E => Test_Entry),
              Message   => "Non-global entry");
--  begin read only
   end Test_Is_Global;
--  end read only


--  begin read only
   procedure Test_Get_Caching (Gnattest_T : in out Test_Table_Entry_Type);
   procedure Test_Get_Caching_b6648c (Gnattest_T : in out Test_Table_Entry_Type) renames Test_Get_Caching;
--  id:2.2/b6648c05e48c90b4/Get_Caching/1/0/
   procedure Test_Get_Caching (Gnattest_T : in out Test_Table_Entry_Type) is
   --  paging-entries.ads:78:4:Get_Caching
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Get_Caching (E => Test_Entry) = Paging.WB,
              Message   => "Caching type mismatch");
--  begin read only
   end Test_Get_Caching;
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
end Paging.Entries.Table_Entry_Type_Test_Data.Table_Entry_Type_Tests;
