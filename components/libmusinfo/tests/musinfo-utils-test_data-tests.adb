--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Musinfo.Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Musinfo.Utils.Test_Data.Tests is


--  begin read only
   procedure Test_Name_Data_Equal (Gnattest_T : in out Test);
   procedure Test_Name_Data_Equal_5adce3 (Gnattest_T : in out Test) renames Test_Name_Data_Equal;
--  id:2.2/5adce30887b56c61/Name_Data_Equal/1/0/
   procedure Test_Name_Data_Equal (Gnattest_T : in out Test) is
   --  musinfo-utils.ads:33:4:Name_Data_Equal
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
   --  musinfo-utils.ads:40:4:Names_Equal
--  end read only

      pragma Unreferenced (Gnattest_T);

      N1 : constant Name_Type
        := Name_Type'(Length  => 12,
                      Padding => 0,
                      Data    => Name_Data_Type'
                        (1 .. 12 => 'a', others => ASCII.NUL));
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
   procedure Test_To_String (Gnattest_T : in out Test);
   procedure Test_To_String_d63ddf (Gnattest_T : in out Test) renames Test_To_String;
--  id:2.2/d63ddf11a6141363/To_String/1/0/
   procedure Test_To_String (Gnattest_T : in out Test) is
   --  musinfo-utils.ads:49:4:To_String
--  end read only

      pragma Unreferenced (Gnattest_T);

      Null_Str : String (1 .. 0) := (others => ASCII.NUL);

      N1 : constant Name_Type
        := Name_Type'(Length  => 12,
                      Padding => 0,
                      Data    => Name_Data_Type'
                        (1 .. 12 => 'a', others => ASCII.NUL));
      S1     : String (1 .. 12);
      S1_Ref : constant String (1 .. 12) := (others => 'a');
      N2 : constant Name_Type
        := Name_Type'(Length  => Name_Size_Type'Last,
                      Padding => 0,
                      Data    => Name_Data_Type'(others => 'x'));
      S2     : String (1 .. Name_Index_Type'Last);
      S2_Ref : constant String (1 .. Name_Index_Type'Last) := (others => 'x');
   begin
      To_String (Name => Null_Name,
                 Str  => Null_Str);
      Assert (Condition => Null_Str = "",
              Message   => "String mismatch (1)");
      To_String (Name => N1,
                 Str  => S1);
      Assert (Condition => S1 = S1_Ref,
              Message   => "String mismatch (2)");
      To_String (Name => N2,
                 Str  => S2);
      Assert (Condition => S2 = S2_Ref,
              Message   => "String mismatch (3)");
--  begin read only
   end Test_To_String;
--  end read only


--  begin read only
   procedure Test_Names_Match (Gnattest_T : in out Test);
   procedure Test_Names_Match_54260e (Gnattest_T : in out Test) renames Test_Names_Match;
--  id:2.2/54260ec69b8b2469/Names_Match/1/0/
   procedure Test_Names_Match (Gnattest_T : in out Test) is
   --  musinfo-utils.ads:57:4:Names_Match
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Name : constant Name_Type
        := Name_Type'(Length  => 12,
                      Padding => 0,
                      Data    => Name_Data_Type'
                        (1 .. 12 => 'a', others => ASCII.NUL));
   begin
      Assert (Condition => Names_Match
              (N1    => Null_Name,
               N2    => "",
               Count => 0),
              Message   => "Null name does not match");
      Assert (Condition => not Names_Match
              (N1    => Null_Name,
               N2    => "something",
               Count => 9),
              Message   => "Null name matches (1)");
      Assert (Condition => not Names_Match
              (N1    => Ref_Name,
               N2    => "",
               Count => 12),
              Message   => "Null name matches (2)");
      Assert (Condition => Names_Match
              (N1    => Ref_Name,
               N2    => "aaaaaaaaaaaa",
               Count => 12),
              Message   => "Name does not match (1)");

      Assert (Condition => not Names_Match
              (N1    => Ref_Name,
               N2    => "aaaaaaaaaabb",
               Count => 12),
              Message   => "Name matches (1)");
      Assert (Condition => not Names_Match
              (N1    => Ref_Name,
               N2    => "aaaaaaaaaaaaa",
               Count => 13),
              Message   => "Name matches (2)");
      Assert (Condition => Names_Match
              (N1    => Ref_Name,
               N2    => "aaaaaaaaaabb",
               Count => 10),
              Message   => "Name does not macht (2)");
--  begin read only
   end Test_Names_Match;
--  end read only


--  begin read only
   procedure Test_Is_Valid (Gnattest_T : in out Test);
   procedure Test_Is_Valid_9b3e00 (Gnattest_T : in out Test) renames Test_Is_Valid;
--  id:2.2/9b3e00da4fadc58c/Is_Valid/1/0/
   procedure Test_Is_Valid (Gnattest_T : in out Test) is
   --  musinfo-utils.ads:66:4:Is_Valid
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
   --  musinfo-utils.ads:69:4:Subject_Name
--  end read only

      pragma Unreferenced (Gnattest_T);

      SI  : Subject_Info_Type;
      Ref : constant Name_Type
        := Name_Type'
          (Length  => 5,
           Padding => 0,
           Data    => Name_Data_Type'
             (1 .. 5 => 'a', others => ASCII.NUL));
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
   --  musinfo-utils.ads:74:4:TSC_Khz
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
   procedure Test_TSC_Schedule_Start (Gnattest_T : in out Test);
   procedure Test_TSC_Schedule_Start_f96984 (Gnattest_T : in out Test) renames Test_TSC_Schedule_Start;
--  id:2.2/f969840fb024c444/TSC_Schedule_Start/1/0/
   procedure Test_TSC_Schedule_Start (Gnattest_T : in out Test) is
   --  musinfo-utils.ads:79:4:TSC_Schedule_Start
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      SI : Subject_Info_Type;
   begin
      SI.TSC_Schedule_Start := 12000;
      Assert (Condition => TSC_Schedule_Start (Sinfo => SI) = 12000,
              Message   => "TSC schedule start mismatch");
--  begin read only
   end Test_TSC_Schedule_Start;
--  end read only


--  begin read only
   procedure Test_TSC_Schedule_End (Gnattest_T : in out Test);
   procedure Test_TSC_Schedule_End_6496f0 (Gnattest_T : in out Test) renames Test_TSC_Schedule_End;
--  id:2.2/6496f057c76f4380/TSC_Schedule_End/1/0/
   procedure Test_TSC_Schedule_End (Gnattest_T : in out Test) is
   --  musinfo-utils.ads:86:4:TSC_Schedule_End
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      SI : Subject_Info_Type;
   begin
      SI.TSC_Schedule_End := 22000;
      Assert (Condition => TSC_Schedule_End (Sinfo => SI) = 22000,
              Message   => "TSC schedule end mismatch");
--  begin read only
   end Test_TSC_Schedule_End;
--  end read only


--  begin read only
   procedure Test_Memory_By_Name (Gnattest_T : in out Test);
   procedure Test_Memory_By_Name_3143a1 (Gnattest_T : in out Test) renames Test_Memory_By_Name;
--  id:2.2/3143a10f7f112a95/Memory_By_Name/1/0/
   procedure Test_Memory_By_Name (Gnattest_T : in out Test) is
   --  musinfo-utils.ads:94:4:Memory_By_Name
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
      SI.Resource_Count  := 0;
      SI.Memregion_Count := 0;
      Assert (Condition => Memory_By_Name
              (Sinfo => SI,
               Name  => "something") = Null_Memregion,
              Message   => "Null_Memregion expected (1)");

      SI.Resource_Count := 2;
      SI.Resources (1) := Resource_Type'
        (Name             => Name_Type'
           (Length  => 2,
            Padding => 0,
            Data    => Name_Data_Type'
              (1 => 'm', 2 => '1', others => ASCII.NUL)),
         Memregion_Idx    => 1,
         Channel_Info_Idx => 0,
         Padding          => 0);
      SI.Resources (2) := Resource_Type'
        (Name             => Name_Type'
           (Length  => 2,
            Padding => 0,
            Data    => Name_Data_Type'
              (1 => 'm', 2 => '2', others => ASCII.NUL)),
         Memregion_Idx    => 2,
         Channel_Info_Idx => 0,
         Padding          => 0);
      Assert (Condition => Memory_By_Name
              (Sinfo => SI,
               Name  => "m2") = Null_Memregion,
              Message   => "Null_Memregion expected (2)");

      SI.Memregion_Count := 2;
      SI.Memregions (2) := Ref;

      Assert (Condition => Memory_By_Name
              (Sinfo => SI,
               Name  => "m2") = Ref,
              Message   => "Memregion mismatch");
--  begin read only
   end Test_Memory_By_Name;
--  end read only


--  begin read only
   procedure Test_Memory_By_Hash (Gnattest_T : in out Test);
   procedure Test_Memory_By_Hash_72b070 (Gnattest_T : in out Test) renames Test_Memory_By_Hash;
--  id:2.2/72b070f50f85a698/Memory_By_Hash/1/0/
   procedure Test_Memory_By_Hash (Gnattest_T : in out Test) is
   --  musinfo-utils.ads:103:4:Memory_By_Hash
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
      SI.Memregion_Count := 3;
      Assert (Condition => Memory_By_Hash
              (Sinfo => SI,
               Hash  => (others => 12)) = Null_Memregion,
              Message   => "Null_Memregion expected");

      SI.Memregions (1) := Memregion_Type'
        (Content => Content_Fill,
         Address => 16#2000#,
         Size    => 16#6000_0000#,
         Hash    => (others => 127),
         Flags   => Null_Memory_Flags,
         Pattern => 22,
         Padding => 0);
      SI.Memregions (2) := Memregion_Type'
        (Content => Content_Fill,
         Address => 16#2000#,
         Size    => 16#6000_0000#,
         Hash    => (others => 12),
         Flags   => Null_Memory_Flags,
         Pattern => 0,
         Padding => 0);
      SI.Memregions (3) := Ref_Mem;

      Assert (Condition => Memory_By_Hash
              (Sinfo => SI,
               Hash  => Ref_Hash) = Ref_Mem,
              Message   => "Memregion mismatch");
--  begin read only
   end Test_Memory_By_Hash;
--  end read only

end Musinfo.Utils.Test_Data.Tests;
