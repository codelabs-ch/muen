--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.Intervals.Interval_List_Type_Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with Ada.Exceptions;
with Ada.Strings.Unbounded;

with Mutools.Utils;

--  begin read only
--  end read only
package body Mutools.Intervals.Interval_List_Type_Test_Data.Interval_List_Type_Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Add_Interval (Gnattest_T : in out Test_Interval_List_Type);
   procedure Test_Add_Interval_2fff12 (Gnattest_T : in out Test_Interval_List_Type) renames Test_Add_Interval;
--  id:2.2/2fff12e7fc3a1381/Add_Interval/1/0/
   procedure Test_Add_Interval (Gnattest_T : in out Test_Interval_List_Type) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Ivals_State : in out Interval_List_Type;
                      New_Ival    :        Interval_Type;
                      Result      :        String)
      is
      begin
         Add_Interval (List     => Ivals_State,
                       Interval => New_Ival);
         Assert (Condition => Interval_List_To_String_Hex (List => Ivals_State)
                   = Result,
                 Message   => "Intervals mismatch: "
                   & Interval_List_To_String_Hex (List => Ivals_State)
                   & " /= "
                   & Result);
      end Test;

      Ivals : Interval_List_Type;
   begin
      --  Add one interval to empty region
      Test (Ivals_State => Ivals,
            New_Ival    => Interval_Type'
              (First_Element => 16#0100#,
               Last_Element  => 16#0110#),
            Result      => "(16#0100#, 16#0110#)");

      --  Add overlapping interval
      Test (Ivals_State => Ivals,
            New_Ival    => Interval_Type'
              (First_Element => 16#0106#,
               Last_Element  => 16#0116#),
            Result      => "(16#0100#, 16#0116#)");

      --  Add interval covering the present one
      Test (Ivals_State => Ivals,
            New_Ival    => Interval_Type'
              (First_Element => 16#00FF#,
               Last_Element  => 16#0120#),
            Result      => "(16#00ff#, 16#0120#)");

      --  Add disjoint interval
      Test (Ivals_State => Ivals,
            New_Ival    => Interval_Type'
              (First_Element => 16#0140#,
               Last_Element  => 16#0150#),
            Result      => "(16#00ff#, 16#0120#) "
              & "(16#0140#, 16#0150#)");
      Test (Ivals_State => Ivals,
            New_Ival    => Interval_Type'
              (First_Element => 16#00E0#,
               Last_Element  => 16#00E5#),
            Result      => "(16#00e0#, 16#00e5#) "
              & "(16#00ff#, 16#0120#) "
              & "(16#0140#, 16#0150#)");

      --  Add a point-interval
      Test (Ivals_State => Ivals,
            New_Ival    => Interval_Type'
              (First_Element => 16#0130#,
               Last_Element  => 16#0130#),
            Result      => "(16#00e0#, 16#00e5#) "
              & "(16#00ff#, 16#0120#) "
              & "(16#0130#, 16#0130#) "
              & "(16#0140#, 16#0150#)");

      --  Add an interval which is already there
      Test (Ivals_State => Ivals,
            New_Ival    => Interval_Type'
              (First_Element => 16#00FF#,
               Last_Element  => 16#0120#),
            Result      => "(16#00e0#, 16#00e5#) "
              & "(16#00ff#, 16#0120#) "
              & "(16#0130#, 16#0130#) "
              & "(16#0140#, 16#0150#)");

      --  Add an interval which is contained in an existing interval
      Test (Ivals_State => Ivals,
            New_Ival    => Interval_Type'
              (First_Element => 16#0141#,
               Last_Element  => 16#0142#),
            Result      => "(16#00e0#, 16#00e5#) "
              & "(16#00ff#, 16#0120#) "
              & "(16#0130#, 16#0130#) "
              & "(16#0140#, 16#0150#)");

      --  Add interval touching 4th interval and overlapping with 2nd
      Test (Ivals_State => Ivals,
            New_Ival  => Interval_Type'
              (First_Element => 16#0110#,
               Last_Element  => 16#013F#),
            Result    => "(16#00e0#, 16#00e5#) "
              & "(16#00ff#, 16#0150#)");

      --  Touch 1st and 2nd interval (squeeze in)
      Test (Ivals_State => Ivals,
            New_Ival    => Interval_Type'
              (First_Element => 16#00e6#,
               Last_Element  => 16#00fe#),
            Result      => "(16#00e0#, 16#0150#)");

      --  Add empty interval
      Test (Ivals_State => Ivals,
            New_Ival    => Interval_Type'
              (First_Element => 16#10e6#,
               Last_Element  => 16#00fe#),
            Result      => "(16#00e0#, 16#0150#)");

      --  Add entire space
      Test (Ivals_State => Ivals,
            New_Ival    => Interval_Type'
              (First_Element => 16#0000_0000_0000_0000#,
               Last_Element  => 16#FFFF_FFFF_FFFF_FFFF#),
            Result      => "(16#0000#, 16#ffff_ffff_ffff_ffff#)");

--  begin read only
   end Test_Add_Interval;
--  end read only


--  begin read only
   procedure Test_1_Subtract_Interval (Gnattest_T : in out Test_Interval_List_Type);
   procedure Test_Subtract_Interval_e416a2 (Gnattest_T : in out Test_Interval_List_Type) renames Test_1_Subtract_Interval;
--  id:2.2/e416a28d09f5c350/Subtract_Interval/1/0/
   procedure Test_1_Subtract_Interval (Gnattest_T : in out Test_Interval_List_Type) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Ivals_State : in out Interval_List_Type;
                      Ival        :        Interval_Type;
                      Result      :        String)
      is
      begin
         Subtract_Interval (List     => Ivals_State,
                            Interval => Ival);
         Assert (Condition =>
                   Result = Interval_List_To_String_Hex (List => Ivals_State),
                 Message   => "Intervals mismatch: "
                   & Interval_List_To_String_Hex (List => Ivals_State)
                   & " /= "
                   & Result);
      end Test;

      Ivals : Interval_List_Type;
   begin
      --  Subtract from empty list
      Test (Ivals_State => Ivals,
            Ival        => Interval_Type'
              (First_Element => 16#0100#,
               Last_Element  => 16#0110#),
            Result      => "");

      --  Subtract from middle of one interval
      Add_Interval (List     => Ivals,
                    Interval => Interval_Type'
                      (First_Element => 16#1000#,
                       Last_Element  => 16#2000#));
      Test (Ivals_State => Ivals,
            Ival        => Interval_Type'
              (First_Element => 16#1100#,
               Last_Element  => 16#1200#),
            Result      => "(16#1000#, 16#10ff#) "
              & "(16#1201#, 16#2000#)");

      --  Subtract from side of one interval (some overlap)
      Test (Ivals_State => Ivals,
            Ival        => Interval_Type'
              (First_Element => 16#0000#,
               Last_Element  => 16#1021#),
            Result      => "(16#1022#, 16#10ff#) "
              & "(16#1201#, 16#2000#)");

      --  Subtraction of superset
      Test (Ivals_State => Ivals,
            Ival        => Interval_Type'
              (First_Element => 16#0000#,
               Last_Element  => 16#1111#),
            Result      => "(16#1201#, 16#2000#)");

      --  Subtract with overlap, containment and adjacent region
      Add_Interval (List     => Ivals,
                    Interval => Interval_Type'
                      (First_Element => 16#3000#,
                       Last_Element  => 16#4000#));
      Add_Interval (List     => Ivals,
                    Interval => Interval_Type'
                      (First_Element => 16#6000#,
                       Last_Element  => 16#7000#));
      --  State of Ivals is now:
      --  (16#1201#, 16#2000#) (16#3000#, 16#4000#) (16#6000#, 16#7000#)
      Test (Ivals_State => Ivals,
            Ival        => Interval_Type'
              (First_Element => 16#1500#,
               Last_Element  => 16#5fff#),
            Result      => "(16#1201#, 16#14ff#) (16#6000#, 16#7000#)");

      --  Subtract outside of list
      Test (Ivals_State => Ivals,
            Ival        => Interval_Type'
              (First_Element => 16#1500#,
               Last_Element  => 16#2000#),
            Result      => "(16#1201#, 16#14ff#) (16#6000#, 16#7000#)");

      --  Subtract empty interval
      Test (Ivals_State => Ivals,
            Ival        => Interval_Type'
              (First_Element => 16#7000#,
               Last_Element  => 16#6000#),
            Result      => "(16#1201#, 16#14ff#) (16#6000#, 16#7000#)");

--  begin read only
   end Test_1_Subtract_Interval;
--  end read only


--  begin read only
   procedure Test_2_Subtract_Interval (Gnattest_T : in out Test_Interval_List_Type);
   procedure Test_Subtract_Interval_78ea82 (Gnattest_T : in out Test_Interval_List_Type) renames Test_2_Subtract_Interval;
--  id:2.2/78ea82084b799b92/Subtract_Interval/0/0/
   procedure Test_2_Subtract_Interval (Gnattest_T : in out Test_Interval_List_Type) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Ivals_State   : in out Interval_List_Type;
                      First_Element :        Interfaces.Unsigned_64;
                      Size          :        Interfaces.Unsigned_64;
                      Result        :        String)
      is
      begin
         Subtract_Interval (List          => Ivals_State,
                            First_Element => First_Element,
                            Size          => Size);
         Assert (Condition => Result =
                   Interval_List_To_String_Hex (List => Ivals_State),
                 Message   => "Intervals mismatch: "
                   & Interval_List_To_String_Hex (List => Ivals_State)
                   & " /= "
                   & Result);
      end Test;

      Ivals : Interval_List_Type;
   begin
      --  Positive test
      Add_Interval (List     => Ivals,
                    Interval => Interval_Type'
                      (First_Element => 16#3000#,
                       Last_Element  => 16#4000#));
      Add_Interval (List     => Ivals,
                    Interval => Interval_Type'
                      (First_Element => 16#6000#,
                       Last_Element  => 16#7000#));
      Test (Ivals_State   => Ivals,
            First_Element => 16#3500#,
            Size          => 16#3000#,
            Result        => "(16#3000#, 16#34ff#) (16#6500#, 16#7000#)");

      --  Size 0
      Test (Ivals_State   => Ivals,
            First_Element => 16#3000#,
            Size          => 16#0000#,
            Result        => "(16#3000#, 16#34ff#) (16#6500#, 16#7000#)");

      --  Size too big
      begin
         Subtract_Interval (List          => Ivals,
                            First_Element => 16#0002#,
                            Size          => Interfaces.Unsigned_64'Last);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Invalid_Interval =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Unsigned_64 overflow when trying to "
                      & "subtract an interval",
                    Message   => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
      end;

--  begin read only
   end Test_2_Subtract_Interval;
--  end read only


--  begin read only
   procedure Test_Reserve_Interval (Gnattest_T : in out Test_Interval_List_Type);
   procedure Test_Reserve_Interval_14240b (Gnattest_T : in out Test_Interval_List_Type) renames Test_Reserve_Interval;
--  id:2.2/14240b71c80b27b9/Reserve_Interval/1/0/
   procedure Test_Reserve_Interval (Gnattest_T : in out Test_Interval_List_Type) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Ivals_State        : in out Interval_List_Type;
                      Size               :        Interfaces.Unsigned_64;
                      Result_Ref         :        String;
                      Ivals_State_Result :        String)
      is
         Result : Interfaces.Unsigned_64;
      begin
         Result := Reserve_Interval (List => Ivals_State,
                                     Size => Size);

         Assert (Condition => Mutools.Utils.To_Hex (Result) = Result_Ref,
                 Message   => "Address mismatch: "
                   &  Mutools.Utils.To_Hex (Result) & " /= " & Result_Ref);
         Assert (Condition => Interval_List_To_String_Hex (List => Ivals_State)
                   = Ivals_State_Result,
                 Message   => "Intervals mismatch: "
                   & Interval_List_To_String_Hex (List => Ivals_State)
                   & " /= "
                   & Ivals_State_Result);
      end Test;

      Ivals : Interval_List_Type;
   begin
      --  Positive tests

      --  Reserve from one big interval
      Add_Interval (List     => Ivals,
                    Interval => Interval_Type'
                      (First_Element => 16#2000#,
                       Last_Element  => 16#6000#));
      Test (Ivals_State        => Ivals,
            Size               => 16#1001#,
            Result_Ref         => "16#2000#",
            Ivals_State_Result => "(16#3001#, 16#6000#)");

      --  Reserve from two intervals, first has equal size
      Add_Interval (List     => Ivals,
                    Interval => Interval_Type'
                      (First_Element => 16#7000#,
                       Last_Element  => 16#8000#));
      Test (Ivals_State        => Ivals,
            Size               => 16#3000#,
            Result_Ref         => "16#3001#",
            Ivals_State_Result => "(16#7000#, 16#8000#)");

      --  Reserve from multiple intervals and first interval is big enough
      Add_Interval (List     => Ivals,
                    Interval => Interval_Type'
                      (First_Element => 16#9000#,
                       Last_Element  => 16#F500#));
      Test (Ivals_State        => Ivals,
            Size               => 16#0500#,
            Result_Ref         => "16#7000#",
            Ivals_State_Result => "(16#7500#, 16#8000#) (16#9000#, 16#f500#)");

      --  Reserve from multiple intervals and only last is big enough
      Add_Interval (List     => Ivals,
                    Interval => Interval_Type'
                      (First_Element => 16#0000#,
                       Last_Element  => 16#0500#));
      Test (Ivals_State        => Ivals,
            Size               => 16#2000#,
            Result_Ref         => "16#9000#",
            Ivals_State_Result => "(16#0000#, 16#0500#) "
              & "(16#7500#, 16#8000#) "
              & "(16#b000#, 16#f500#)");

      --  Reserve with size 1
      Test (Ivals_State        => Ivals,
            Size               => 16#0001#,
            Result_Ref         => "16#0000#",
            Ivals_State_Result => "(16#0001#, 16#0500#) "
              & "(16#7500#, 16#8000#) "
              & "(16#b000#, 16#f500#)");

      --  Reserve with maximal size
      Add_Interval (List     => Ivals,
                    Interval => Interval_Type'
                      (First_Element => 16#0000#,
                       Last_Element  => 16#FFFF_FFFF_FFFF_FFFF#));
      Test (Ivals_State        => Ivals,
            Size               => 16#FFFF_FFFF_FFFF_FFFF#,
            Result_Ref         => "16#0000#",
            Ivals_State_Result => "("
              & Mutools.Utils.To_Hex (Interfaces.Unsigned_64'Last)
              & ", "
              & Mutools.Utils.To_Hex (Interfaces.Unsigned_64'Last)
              & ")");

      --  Negative tests
      Clear (List => Ivals);

      --  List is empty
      declare
         Result : Interfaces.Unsigned_64;
      begin
         Result := Reserve_Interval (List => Ivals,
                                     Size => 16#0001#);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Out_Of_Space =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Cannot find free interval of size "
                      & "'16#0001#'",
                    Message   => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
      end;

      --  Size is 0
      Add_Interval (List             => Ivals,
                    Interval         => Interval_Type'
                      (First_Element => 16#7000#,
                       Last_Element  => 16#A000#));
      declare
         Result : Interfaces.Unsigned_64;
      begin
         Result := Reserve_Interval (List => Ivals,
                                     Size => 16#0000#);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : others =>
            null;
      end;

      --  All intervals are too small
      Add_Interval (List             => Ivals,
                    Interval         => Interval_Type'
                      (First_Element => 16#0000#,
                       Last_Element  => 16#2000#));
      declare
         Result : Interfaces.Unsigned_64;
      begin
         Result := Reserve_Interval (List => Ivals,
                                     Size => 16#3002#);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Out_Of_Space =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Cannot find free interval of size "
                      & "'16#3002#'",
                    Message   => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
      end;

--  begin read only
   end Test_Reserve_Interval;
--  end read only


--  begin read only
   procedure Test_Clear (Gnattest_T : in out Test_Interval_List_Type);
   procedure Test_Clear_beef71 (Gnattest_T : in out Test_Interval_List_Type) renames Test_Clear;
--  id:2.2/beef71e8d227f76b/Clear/1/0/
   procedure Test_Clear (Gnattest_T : in out Test_Interval_List_Type) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      Ivals : Interval_List_Type;
   begin
      Add_Interval (List             => Ivals,
                    Interval         => Interval_Type'
                      (First_Element => 16#0000#,
                       Last_Element  => 16#2000#));
      Clear (List => Ivals);
      Assert (Condition => Ivals.Data.Is_Empty,
              Message   => "List of intervals is not empty:"
                & Interval_List_To_String_Hex (List => Ivals));

--  begin read only
   end Test_Clear;
--  end read only


--  begin read only
   procedure Test_Interval_List_To_String_Hex (Gnattest_T : in out Test_Interval_List_Type);
   procedure Test_Interval_List_To_String_Hex_13fe3a (Gnattest_T : in out Test_Interval_List_Type) renames Test_Interval_List_To_String_Hex;
--  id:2.2/13fe3a7afb4fc057/Interval_List_To_String_Hex/1/0/
   procedure Test_Interval_List_To_String_Hex (Gnattest_T : in out Test_Interval_List_Type) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ivals : Interval_List_Type;
   begin
      Assert (Condition => Interval_List_To_String_Hex (List => Ivals)
                = "",
              Message   => "String mismatch (1): "
                & Interval_List_To_String_Hex (List => Ivals));
      Add_Interval (List             => Ivals,
                    Interval         => Interval_Type'
                      (First_Element => 16#1000#,
                       Last_Element  => 16#2000#));
      Add_Interval (List             => Ivals,
                    Interval         => Interval_Type'
                      (First_Element => 16#3000_0001#,
                       Last_Element  => 16#1234_5678_0000#));
      Add_Interval (List             => Ivals,
                    Interval         => Interval_Type'
                      (First_Element => 16#0000#,
                       Last_Element  => 16#0001#));
      Assert (Condition => Interval_List_To_String_Hex (List => Ivals)
                = "(16#0000#, 16#0001#) (16#1000#, 16#2000#) "
                & "(16#3000_0001#, 16#1234_5678_0000#)",
              Message   => "String mismatch (2): "
                & Interval_List_To_String_Hex (List => Ivals));

--  begin read only
   end Test_Interval_List_To_String_Hex;
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
end Mutools.Intervals.Interval_List_Type_Test_Data.Interval_List_Type_Tests;
