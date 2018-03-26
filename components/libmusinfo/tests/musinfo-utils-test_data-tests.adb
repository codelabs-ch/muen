--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Musinfo.Utils.Test_Data.

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
package body Musinfo.Utils.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_To_Name (Gnattest_T : in out Test);
   procedure Test_To_Name_485d93 (Gnattest_T : in out Test) renames Test_To_Name;
--  id:2.2/485d9370aa398b21/To_Name/1/0/
   procedure Test_To_Name (Gnattest_T : in out Test) is
   --  musinfo-utils.ads:33:4:To_Name
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Name : constant Name_Type
        := Name_Type'
          (Length    => 2,
           Padding   => 0,
           Data      => Name_Data_Type'
             (1 => 'm', 2 => '1', others => ASCII.NUL),
           Null_Term => ASCII.NUL);
   begin
      Assert (Condition => To_Name (Str => "") = Null_Name,
              Message   => "Null name expected");
      Assert (Condition => To_Name (Str => "m1") = Ref_Name,
              Message   => "Name mismatch");
--  begin read only
   end Test_To_Name;
--  end read only


--  begin read only
   procedure Test_Concat (Gnattest_T : in out Test);
   procedure Test_Concat_d3a0af (Gnattest_T : in out Test) renames Test_Concat;
--  id:2.2/d3a0af6b2a5890a8/Concat/1/0/
   procedure Test_Concat (Gnattest_T : in out Test) is
   --  musinfo-utils.ads:39:4:Concat
--  end read only

      pragma Unreferenced (Gnattest_T);

      N1 : constant Name_Type
        := Name_Type'(Length    => 10,
                      Padding   => 0,
                      Data      => Name_Data_Type'
                        (1 .. 10 => 'a', others => ASCII.NUL),
                      Null_Term => ASCII.NUL);
      N2 : constant Name_Type
        := Name_Type'(Length    => 2,
                      Padding   => 0,
                      Data      => Name_Data_Type'
                        (1 .. 2 => 'b', others => ASCII.NUL),
                      Null_Term => ASCII.NUL);
      R  : constant Name_Type
        := Name_Type'(Length    => 12,
                      Padding   => 0,
                      Data      => Name_Data_Type'
                        ( 1 .. 10 => 'a',
                         11 .. 12 => 'b',
                         others   => ASCII.NUL),
                      Null_Term => ASCII.NUL);
   begin
      Assert (Condition => Concat (L => N1, R => N2) = R,
              Message   => "Name mismatch (1)");
      Assert (Condition => Concat (L => N1, R => Null_Name) = N1,
              Message   => "Name mismatch (2)");
      Assert (Condition => Concat (L => Null_Name, R => N1) = N1,
              Message   => "Name mismatch (3)");
      Assert (Condition => Concat (L => Null_Name, R => Null_Name) = Null_Name,
              Message   => "Name mismatch (4)");
      Assert (Condition => Concat (L => N2, R => N1) /= R,
              Message   => "Names match");
--  begin read only
   end Test_Concat;
--  end read only


--  begin read only
   procedure Test_Name_Data_Equal (Gnattest_T : in out Test);
   procedure Test_Name_Data_Equal_5adce3 (Gnattest_T : in out Test) renames Test_Name_Data_Equal;
--  id:2.2/5adce30887b56c61/Name_Data_Equal/1/0/
   procedure Test_Name_Data_Equal (Gnattest_T : in out Test) is
   --  musinfo-utils.ads:44:4:Name_Data_Equal
--  end read only

      pragma Unreferenced (Gnattest_T);

      N1 : constant Name_Data_Type := (others => 'a');
      N2 : Name_Data_Type := N1;
   begin
      Assert (Condition => Name_Data_Equal
              (Left  => N1,
               Right => N2),
              Message   => "Names not equal");

      N2 (Name_Index_Type'Last) := 'c';
      Assert (Condition => not Name_Data_Equal
              (Left  => N1,
               Right => N2),
              Message   => "Names equal");
--  begin read only
   end Test_Name_Data_Equal;
--  end read only


--  begin read only
   procedure Test_Names_Equal (Gnattest_T : in out Test);
   procedure Test_Names_Equal_7432cb (Gnattest_T : in out Test) renames Test_Names_Equal;
--  id:2.2/7432cb599ba1be7e/Names_Equal/1/0/
   procedure Test_Names_Equal (Gnattest_T : in out Test) is
   --  musinfo-utils.ads:51:4:Names_Equal
--  end read only

      pragma Unreferenced (Gnattest_T);

      N1 : constant Name_Type
        := Name_Type'(Length    => 12,
                      Padding   => 0,
                      Data      => Name_Data_Type'
                        (1 .. 12 => 'a', others => ASCII.NUL),
                      Null_Term => ASCII.NUL);
      N2 : Name_Type := N1;
   begin
      Assert (Condition => Names_Equal
              (Left  => N1,
               Right => N2),
              Message   => "Names mismatch");

      N2.Length := 13;
      Assert (Condition => not Names_Equal
              (Left  => N1,
               Right => N2),
              Message   => "Names match (1)");

      N2.Length  := 12;
      N2.Padding := 3;
      Assert (Condition => not Names_Equal
              (Left  => N1,
               Right => N2),
              Message   => "Names match (2)");
--  begin read only
   end Test_Names_Equal;
--  end read only


--  begin read only
   procedure Test_Names_Match (Gnattest_T : in out Test);
   procedure Test_Names_Match_2004f1 (Gnattest_T : in out Test) renames Test_Names_Match;
--  id:2.2/2004f134b0a87524/Names_Match/1/0/
   procedure Test_Names_Match (Gnattest_T : in out Test) is
   --  musinfo-utils.ads:61:4:Names_Match
--  end read only

      pragma Unreferenced (Gnattest_T);

      N1 : constant Name_Type
        := Name_Type'(Length    => 13,
                      Padding   => 0,
                      Data      => Name_Data_Type'
                        (1 .. 12 => 'a', 13 => 'c', others => ASCII.NUL),
                      Null_Term => ASCII.NUL);
      N2 : constant Name_Type
        := Name_Type'(Length    => 13,
                      Padding   => 0,
                      Data      => Name_Data_Type'
                        (1 .. 12 => 'a', 13 => 'b', others => ASCII.NUL),
                      Null_Term => ASCII.NUL);
   begin
      Assert (Condition => Names_Match
              (N1    => Null_Name,
               N2    => Null_Name,
               Count => 0),
              Message   => "Null name does not match");
      Assert (Condition => not Names_Match
              (N1    => Null_Name,
               N2    => N1,
               Count => 9),
              Message   => "Null name match");
      Assert (Condition => Names_Match
              (N1    => N1,
               N2    => N1,
               Count => 12),
              Message   => "Identical name does not match");
      Assert (Condition => Names_Match
              (N1    => N1,
               N2    => N2,
               Count => 12),
              Message   => "Name mismatch");
      Assert (Condition => not Names_Match
              (N1    => N1,
               N2    => N2,
               Count => 13),
              Message   => "Names match");
--  begin read only
   end Test_Names_Match;
--  end read only


--  begin read only
   procedure Test_Is_Valid (Gnattest_T : in out Test);
   procedure Test_Is_Valid_9b3e00 (Gnattest_T : in out Test) renames Test_Is_Valid;
--  id:2.2/9b3e00da4fadc58c/Is_Valid/1/0/
   procedure Test_Is_Valid (Gnattest_T : in out Test) is
   --  musinfo-utils.ads:67:4:Is_Valid
--  end read only

      pragma Unreferenced (Gnattest_T);

      SI : Subject_Info_Type;
   begin
      SI.Magic := 12;
      Assert (Condition => not Is_Valid (Sinfo => SI),
              Message   => "Sinfo valid");
      SI.Magic := Muen_Subject_Info_Magic;
      Assert (Condition => Is_Valid (Sinfo => SI),
              Message   => "Sinfo not valid");
--  begin read only
   end Test_Is_Valid;
--  end read only


--  begin read only
   procedure Test_Subject_Name (Gnattest_T : in out Test);
   procedure Test_Subject_Name_082315 (Gnattest_T : in out Test) renames Test_Subject_Name;
--  id:2.2/082315c264fa4063/Subject_Name/1/0/
   procedure Test_Subject_Name (Gnattest_T : in out Test) is
   --  musinfo-utils.ads:70:4:Subject_Name
--  end read only

      pragma Unreferenced (Gnattest_T);

      SI  : Subject_Info_Type;
      Ref : constant Name_Type
        := Name_Type'
          (Length    => 5,
           Padding   => 0,
           Data      => Name_Data_Type'
             (1 .. 5 => 'a', others => ASCII.NUL),
           Null_Term => ASCII.NUL);
   begin
      SI.Name := Ref;
      Assert (Condition => Subject_Name (Sinfo => SI) = Ref,
              Message   => "Name mismatch");
--  begin read only
   end Test_Subject_Name;
--  end read only


--  begin read only
   procedure Test_TSC_Khz (Gnattest_T : in out Test);
   procedure Test_TSC_Khz_0651a1 (Gnattest_T : in out Test) renames Test_TSC_Khz;
--  id:2.2/0651a195c755ebc0/TSC_Khz/1/0/
   procedure Test_TSC_Khz (Gnattest_T : in out Test) is
   --  musinfo-utils.ads:75:4:TSC_Khz
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      SI : Subject_Info_Type;
   begin
      SI.TSC_Khz := 2000;
      Assert (Condition => TSC_Khz (Sinfo => SI) = 2000,
              Message   => "TSC kHz value mismatch");
--  begin read only
   end Test_TSC_Khz;
--  end read only


--  begin read only
   procedure Test_Memory_By_Name (Gnattest_T : in out Test);
   procedure Test_Memory_By_Name_138c4a (Gnattest_T : in out Test) renames Test_Memory_By_Name;
--  id:2.2/138c4a5146451af0/Memory_By_Name/1/0/
   procedure Test_Memory_By_Name (Gnattest_T : in out Test) is
   --  musinfo-utils.ads:81:4:Memory_By_Name
--  end read only

      pragma Unreferenced (Gnattest_T);

      SI  : Subject_Info_Type;
      Ref : constant Memregion_Type := Memregion_Type'
        (Content => Content_Fill,
         Address => 16#2000#,
         Size    => 16#6000_0000#,
         Hash    => No_Hash,
         Flags   => Null_Memory_Flags,
         Pattern => 234,
         Padding => 0);
   begin
      SI.Magic := 12;
      Assert (Condition => Memory_By_Name
              (Sinfo => SI,
               Name  => To_Name (Str => "something")) = Null_Memregion,
              Message   => "Null_Memregion expected (1)");

      SI.Resources (1) := Resource_Type'
        (Kind     => Res_Memory,
         Name     => Name_Type'
           (Length  => 2,
            Padding   => 0,
            Data      => Name_Data_Type'
              (1 => 'm', 2 => '1', others => ASCII.NUL),
            Null_Term => ASCII.NUL),
         Padding  => (others => 0),
         Mem_Data => (Content => Content_Fill,
                      Address => 16#2000#,
                      Size    => 16#6000_0000#,
                      Hash    => No_Hash,
                      Flags   => Null_Memory_Flags,
                      Pattern => 234,
                      Padding => 0));
      SI.Resources (2) := Resource_Type'
        (Kind     => Res_Memory,
         Name     => Name_Type'
           (Length   => 2,
            Padding  => 0,
            Data     => Name_Data_Type'
              (1 => 'm', 2 => '2', others => ASCII.NUL),
           Null_Term => ASCII.NUL),
         Padding  => (others => 0),
         Mem_Data => Ref);
      Assert (Condition => Memory_By_Name
              (Sinfo => SI,
               Name  => To_Name ("m2")) = Ref,
              Message   => "Memregion mismatch");
--  begin read only
   end Test_Memory_By_Name;
--  end read only


--  begin read only
   procedure Test_Memory_By_Hash (Gnattest_T : in out Test);
   procedure Test_Memory_By_Hash_ad76ff (Gnattest_T : in out Test) renames Test_Memory_By_Hash;
--  id:2.2/ad76ff6e326b44f9/Memory_By_Hash/1/0/
   procedure Test_Memory_By_Hash (Gnattest_T : in out Test) is
   --  musinfo-utils.ads:91:4:Memory_By_Hash
--  end read only

      pragma Unreferenced (Gnattest_T);

      SI       : Subject_Info_Type;
      Ref_Hash : constant Hash_Type := (others => 128);
      Ref_Mem  : constant Memregion_Type := Memregion_Type'
        (Content => Content_Fill,
         Address => 16#2000#,
         Size    => 16#6000_0000#,
         Hash    => Ref_Hash,
         Flags   => Null_Memory_Flags,
         Pattern => 22,
         Padding => 0);
   begin
      SI.Magic := 12;
      Assert (Condition => Memory_By_Hash
              (Sinfo   => SI,
               Hash    => (others => 12),
               Content => Content_Fill) = Null_Memregion,
              Message   => "Null_Memregion expected (1)");

      SI.Resources (1) := Resource_Type'
        (Kind     => Res_Memory,
         Name     => Name_Type'
           (Length   => 2,
            Padding  => 0,
            Data     => Name_Data_Type'
              (1 => 'm', 2 => '2', others => ASCII.NUL),
            Null_Term => ASCII.NUL),
         Padding  => (others => 0),
         Mem_Data => Memregion_Type'
           (Content => Content_Fill,
            Address => 16#2000#,
            Size    => 16#6000_0000#,
            Hash    => (others => 127),
            Flags   => Null_Memory_Flags,
            Pattern => 22,
            Padding => 0));
      SI.Resources (2) := Resource_Type'
        (Kind     => Res_Memory,
         Name     => Name_Type'
           (Length   => 2,
            Padding  => 0,
            Data     => Name_Data_Type'
              (1 => 'm', 2 => '2', others => ASCII.NUL),
            Null_Term => ASCII.NUL),
         Padding  => (others => 0),
         Mem_Data => Memregion_Type'
           (Content => Content_Fill,
            Address => 16#2000#,
            Size    => 16#6000_0000#,
            Hash    => (others => 12),
            Flags   => Null_Memory_Flags,
            Pattern => 0,
            Padding => 0));
      SI.Resources (3) := Resource_Type'
        (Kind     => Res_Memory,
         Name     => Name_Type'
           (Length   => 2,
            Padding  => 0,
            Data     => Name_Data_Type'
              (1 => 'm', 2 => '2', others => ASCII.NUL),
            Null_Term => ASCII.NUL),
         Padding  => (others => 0),
         Mem_Data => Ref_Mem);

      Assert (Condition => Memory_By_Hash
              (Sinfo   => SI,
               Hash    => Ref_Hash,
               Content => Content_Fill) = Ref_Mem,
              Message   => "Memregion mismatch");

      --  Content mismatch.

      Assert (Condition => Memory_By_Hash
              (Sinfo   => SI,
               Hash    => Ref_Hash,
               Content => Content_File) = Null_Memregion,
              Message   => "Null_Memregion expected (2)");
--  begin read only
   end Test_Memory_By_Hash;
--  end read only


--  begin read only
   procedure Test_Create_Resource_Iterator (Gnattest_T : in out Test);
   procedure Test_Create_Resource_Iterator_46ac9c (Gnattest_T : in out Test) renames Test_Create_Resource_Iterator;
--  id:2.2/46ac9c9c3f25bd39/Create_Resource_Iterator/1/0/
   procedure Test_Create_Resource_Iterator (Gnattest_T : in out Test) is
   --  musinfo-utils.ads:118:4:Create_Resource_Iterator
--  end read only

      pragma Unreferenced (Gnattest_T);

      Iter : Resource_Iterator_Type;
      SI   : Subject_Info_Type;
      N    : constant Name_Type
        := Name_Type'(Length    => 12,
                      Padding   => 0,
                      Data      => Name_Data_Type'
                        (1 .. 12 => 'a', others => ASCII.NUL),
                      Null_Term => ASCII.NUL);
   begin
      Assert (Condition => Iter.Resource_Idx = Resource_Index_Type'First,
              Message   => "Default index mismatch");
      Assert (Condition => Iter.Owner = Null_Name,
              Message   => "Default not Null_Name");

      SI.Name := N;

      Iter := Create_Resource_Iterator (Container => SI);
      Assert (Condition => Iter.Resource_Idx = 1,
              Message   => "Resource index not 1");
      Assert (Condition => Iter.Owner = N,
              Message   => "Owner mismatch");
--  begin read only
   end Test_Create_Resource_Iterator;
--  end read only


--  begin read only
   procedure Test_Has_Element (Gnattest_T : in out Test);
   procedure Test_Has_Element_67f644 (Gnattest_T : in out Test) renames Test_Has_Element;
--  id:2.2/67f644ba47d8925a/Has_Element/1/0/
   procedure Test_Has_Element (Gnattest_T : in out Test) is
   --  musinfo-utils.ads:128:4:Has_Element
--  end read only

      pragma Unreferenced (Gnattest_T);

      SI   : Subject_Info_Type;
      Iter : Resource_Iterator_Type;
   begin
      SI.Magic     := 12;
      SI.Resources := (others => Null_Resource);

      Assert (Condition => not Has_Element
              (Container => SI,
               Iter      => Iter),
              Message   => "No element expected");

      SI.Resources (1) := Resource_Type'
        (Kind     => Res_Memory,
         Name     => Name_Type'
           (Length  => 2,
            Padding   => 0,
            Data      => Name_Data_Type'
              (1 => 'm', 2 => '1', others => ASCII.NUL),
            Null_Term => ASCII.NUL),
         Padding  => (others => 0),
         Mem_Data => Memregion_Type'
           (Content => Content_Fill,
            Address => 16#2000#,
            Size    => 16#6000_0000#,
            Hash    => No_Hash,
            Flags   => Null_Memory_Flags,
            Pattern => 234,
            Padding => 0));
      Assert (Condition => Has_Element
              (Container => SI,
               Iter      => Iter),
              Message   => "Element expected");
--  begin read only
   end Test_Has_Element;
--  end read only


--  begin read only
   procedure Test_Element (Gnattest_T : in out Test);
   procedure Test_Element_bebd16 (Gnattest_T : in out Test) renames Test_Element;
--  id:2.2/bebd168b38ab34c1/Element/1/0/
   procedure Test_Element (Gnattest_T : in out Test) is
   --  musinfo-utils.ads:139:4:Element
--  end read only

      pragma Unreferenced (Gnattest_T);

      Iter : Resource_Iterator_Type;
      SI   : Subject_Info_Type;
      N    : constant Name_Type
        := Name_Type'(Length    => 12,
                      Padding   => 0,
                      Data      => Name_Data_Type'
                        (1 .. 12 => 'a', others => ASCII.NUL),
                      Null_Term => ASCII.NUL);

      R     : Resource_Type;
      M_Ref : constant Memregion_Type := Memregion_Type'
        (Content => Content_Fill,
         Address => 16#2000#,
         Size    => 16#6000_0000#,
         Hash    => No_Hash,
         Flags   => Null_Memory_Flags,
         Pattern => 234,
         Padding => 0);
   begin
      SI.Magic     := 12;
      SI.Resources := (others => Null_Resource);
      Assert (Condition => Element
              (Container => SI,
               Iter      => Iter) = Null_Resource,
              Message   => "Null resource expected (1)");

      SI.Resources (10) :=
        (Kind     => Res_Memory,
         Name     => N,
         Padding  => (others => 0),
         Mem_Data => M_Ref);

      Iter.Resource_Idx := 10;
      R := Element (Container => SI,
                    Iter      => Iter);
      Assert (Condition => R.Name = N,
              Message   => "Name mismatch");
      Assert (Condition => R.Mem_Data = M_Ref,
              Message   => "Memory data mismatch");
--  begin read only
   end Test_Element;
--  end read only


--  begin read only
   procedure Test_Next (Gnattest_T : in out Test);
   procedure Test_Next_4ec7b6 (Gnattest_T : in out Test) renames Test_Next;
--  id:2.2/4ec7b6773a7eeba5/Next/1/0/
   procedure Test_Next (Gnattest_T : in out Test) is
   --  musinfo-utils.ads:149:4:Next
--  end read only

      pragma Unreferenced (Gnattest_T);

      Iter : Resource_Iterator_Type;
      SI   : Subject_Info_Type;
   begin
      SI.Magic := 12;

      Assert (Condition => Iter.Resource_Idx = Resource_Index_Type'First,
              Message   => "Unexpected start index");
      Next (Iter => Iter);
      Assert (Condition => Iter.Resource_Idx = Resource_Index_Type'First,
              Message   => "Iter moved without name");

      Iter.Owner := Name_Type'
        (Length    => 12,
         Padding   => 0,
         Data      => Name_Data_Type'
           (1 .. 12 => 'a', others => ASCII.NUL),
         Null_Term => ASCII.NUL);
      Iter.Resource_Idx := 4;
      Next (Iter => Iter);
      Assert (Condition => Iter.Resource_Idx = 5,
              Message   => "Index mismatch");

      for I in 1 .. 350 loop
         Next (Iter => Iter);
      end loop;

      Assert (Condition => Iter.Resource_Idx = Resource_Index_Type'Last,
              Message   => "Unexpected index");
--  begin read only
   end Test_Next;
--  end read only


--  begin read only
   procedure Test_Device_By_SID (Gnattest_T : in out Test);
   procedure Test_Device_By_SID_ec3395 (Gnattest_T : in out Test) renames Test_Device_By_SID;
--  id:2.2/ec33953a72eaf581/Device_By_SID/1/0/
   procedure Test_Device_By_SID (Gnattest_T : in out Test) is
   --  musinfo-utils.ads:155:4:Device_By_SID
--  end read only

      pragma Unreferenced (Gnattest_T);

      SI  : Subject_Info_Type;
      Ref : constant Device_Type := Device_Type'
        (SID        => 16#abcd#,
         IRTE_Start => 12,
         IRQ_Start  => 2,
         IR_Count   => 5,
         Flags      => (MSI_Capable => True, Padding => 1),
         Padding    => (others => 0));
   begin
      SI.Magic := 12;

      Assert (Condition => Device_By_SID
              (Sinfo => SI,
               SID   => 16#abcd#) = Null_Device,
              Message   => "Null_Dev_Info expected");

      SI.Resources (1) := Resource_Type'
        (Kind     => Res_Device,
         Name     => Name_Type'
           (Length   => 2,
            Padding  => 0,
            Data     => Name_Data_Type'
              (1 => 'm', 2 => '2', others => ASCII.NUL),
            Null_Term => ASCII.NUL),
         Padding  => (others => 0),
         Dev_Data => Device_Type'
           (SID        => 16#abcc#,
            IRTE_Start => 12,
            IRQ_Start  => 2,
            IR_Count   => 4,
            Flags      => Null_Dev_Flags,
            Padding    => (others => 0)));
      SI.Resources (2) := Resource_Type'
        (Kind     => Res_Device,
         Name     => Name_Type'
           (Length   => 2,
            Padding  => 0,
            Data     => Name_Data_Type'
              (1 => 'm', 2 => '2', others => ASCII.NUL),
            Null_Term => ASCII.NUL),
         Padding  => (others => 0),
         Dev_Data => Device_Type'
           (SID        => 16#abce#,
            IRTE_Start => 11,
            IRQ_Start  => 2,
            IR_Count   => 5,
            Flags      => Null_Dev_Flags,
            Padding    => (others => 0)));
      SI.Resources (3) := Resource_Type'
        (Kind     => Res_Device,
         Name     => Name_Type'
           (Length   => 2,
            Padding  => 0,
            Data     => Name_Data_Type'
              (1 => 'm', 2 => '2', others => ASCII.NUL),
            Null_Term => ASCII.NUL),
         Padding  => (others => 0),
         Dev_Data => Ref);

      Assert (Condition => Device_By_SID
              (Sinfo => SI,
               SID   => 16#abcd#) = Ref,
              Message   => "Device mismatch");
--  begin read only
   end Test_Device_By_SID;
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
end Musinfo.Utils.Test_Data.Tests;
