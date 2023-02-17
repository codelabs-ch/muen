--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Alloc.Map.VA_Regions_Type_Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Ada.Strings.Unbounded;
with Ada.Exceptions;

with DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Elements;

with Mutools.Utils;

--  begin read only
--  end read only
package body Alloc.Map.VA_Regions_Type_Test_Data.VA_Regions_Type_Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only
   function VA_Regions_To_String
     (Regions : in out VA_Regions_Type)
     return String
   is
      use Ada.Strings.Unbounded;
      use Region_List_Package;
      use type Memory_Intervals_Package.Cursor;

      Output : Unbounded_String;
      Curr   : Memory_Intervals_Package.Cursor
        := Memory_Intervals_Package.First (Container => Regions.Data);
   begin
      while Curr /= Memory_Intervals_Package.No_Element loop
         if Output /= "" then
            Output := Output & " ";
         end if;
         Output := Output & "("
           & Mutools.Utils.To_Hex
           (Memory_Intervals_Package.Element (Curr).First_Address)
           & ", "
           & Mutools.Utils.To_Hex
           (Memory_Intervals_Package.Element (Curr).Last_Address)
           & ")";
         Memory_Intervals_Package.Next (Curr);
      end loop;
      return To_String (Output);
   end VA_Regions_To_String;

--  begin read only
--  end read only

--  begin read only
   procedure Test_Add_Memory_Interval (Gnattest_T : in out Test_VA_Regions_Type);
   procedure Test_Add_Memory_Interval_725fef (Gnattest_T : in out Test_VA_Regions_Type) renames Test_Add_Memory_Interval;
--  id:2.2/725fef0e5facde20/Add_Memory_Interval/1/0/
   procedure Test_Add_Memory_Interval (Gnattest_T : in out Test_VA_Regions_Type) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Mem_State : in out VA_Regions_Type;
                      New_Ival  :        Memory_Interval_Type;
                      Result    :        String)
      is
      begin
         Add_Memory_Interval (List     => Mem_State,
                              Interval => New_Ival);
         Assert (Condition => VA_Regions_To_String (Regions => Mem_State)
                   = Result,
                 Message   => "Memory regions mismatch: "
                   & VA_Regions_To_String (Regions => Mem_State)
                   & " /= "
                   & Result);
      end Test;

      Mem : VA_Regions_Type;
   begin
      --  Add one interval to empty region
      Test (Mem_State => Mem,
            New_Ival  => Memory_Interval_Type'
              (First_Address => 16#0100#,
               Last_Address  => 16#0110#),
            Result    => "(16#0100#, 16#0110#)");

      --  Add overlapping interval
      Test (Mem_State => Mem,
            New_Ival  => Memory_Interval_Type'
              (First_Address => 16#0106#,
               Last_Address  => 16#0116#),
            Result    => "(16#0100#, 16#0116#)");

      --  Add interval covering the present one
      Test (Mem_State => Mem,
            New_Ival  => Memory_Interval_Type'
              (First_Address => 16#00FF#,
               Last_Address  => 16#0120#),
            Result    => "(16#00ff#, 16#0120#)");

      --  Add disjoint interval
      Test (Mem_State => Mem,
            New_Ival  => Memory_Interval_Type'
              (First_Address => 16#0140#,
               Last_Address  => 16#0150#),
            Result    => "(16#00ff#, 16#0120#) "
              & "(16#0140#, 16#0150#)");
      Test (Mem_State => Mem,
            New_Ival  => Memory_Interval_Type'
              (First_Address => 16#00E0#,
               Last_Address  => 16#00E5#),
            Result    => "(16#00e0#, 16#00e5#) "
              & "(16#00ff#, 16#0120#) "
              & "(16#0140#, 16#0150#)");

      --  Add a point-interall
      Test (Mem_State => Mem,
            New_Ival  => Memory_Interval_Type'
              (First_Address => 16#0130#,
               Last_Address  => 16#0130#),
            Result    => "(16#00e0#, 16#00e5#) "
              & "(16#00ff#, 16#0120#) "
              & "(16#0130#, 16#0130#) "
              & "(16#0140#, 16#0150#)");

      --  Add an interval which is already there
      Test (Mem_State => Mem,
            New_Ival  => Memory_Interval_Type'
              (First_Address => 16#00FF#,
               Last_Address  => 16#0120#),
            Result    => "(16#00e0#, 16#00e5#) "
              & "(16#00ff#, 16#0120#) "
              & "(16#0130#, 16#0130#) "
              & "(16#0140#, 16#0150#)");

      --  Add an interval which is contained in an existing interval
      Test (Mem_State => Mem,
            New_Ival  => Memory_Interval_Type'
              (First_Address => 16#0141#,
               Last_Address  => 16#0142#),
            Result    => "(16#00e0#, 16#00e5#) "
              & "(16#00ff#, 16#0120#) "
              & "(16#0130#, 16#0130#) "
              & "(16#0140#, 16#0150#)");

      --  Add interval touching 4th interval and overlapping with 2nd
      Test (Mem_State => Mem,
            New_Ival  => Memory_Interval_Type'
              (First_Address => 16#0110#,
               Last_Address  => 16#013F#),
            Result    => "(16#00e0#, 16#00e5#) "
              & "(16#00ff#, 16#0150#)");

      --  Touch 1st and 2nd interval (squeeze in)
      Test (Mem_State => Mem,
            New_Ival  => Memory_Interval_Type'
              (First_Address => 16#00e6#,
               Last_Address  => 16#00fe#),
            Result    => "(16#00e0#, 16#0150#)");

      --  Add entire space
      Test (Mem_State => Mem,
            New_Ival  => Memory_Interval_Type'
              (First_Address => 16#0000_0000_0000_0000#,
               Last_Address  => 16#FFFF_FFFF_FFFF_FFFF#),
            Result    => "(16#0000#, 16#ffff_ffff_ffff_ffff#)");

      --  Negative test: add ivalid region
      begin
         Add_Memory_Interval (List     => Mem,
                              Interval => Memory_Interval_Type'
                                (First_Address => 16#0900#,
                                 Last_Address  => 16#0899#));
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Invalid_Region =>
           Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Found invalid interval from '16#0900#' to '16#0899#'",
                   Message   => "Exception mismatch: "
                     & Ada.Exceptions.Exception_Message (X => E));
      end;

--  begin read only
   end Test_Add_Memory_Interval;
--  end read only


--  begin read only
   procedure Test_1_Subtract_Memory_Interval (Gnattest_T : in out Test_VA_Regions_Type);
   procedure Test_Subtract_Memory_Interval_cff167 (Gnattest_T : in out Test_VA_Regions_Type) renames Test_1_Subtract_Memory_Interval;
--  id:2.2/cff167258b49fabd/Subtract_Memory_Interval/1/0/
   procedure Test_1_Subtract_Memory_Interval (Gnattest_T : in out Test_VA_Regions_Type) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Mem_State : in out VA_Regions_Type;
                      Ival      :        Memory_Interval_Type;
                      Result    :        String)
      is
      begin
         Subtract_Memory_Interval (List     => Mem_State,
                                   Interval => Ival);
         Assert (Condition =>
                   Result = VA_Regions_To_String (Regions => Mem_State),
                 Message   => "Memory regions mismatch: "
                   & VA_Regions_To_String (Regions => Mem_State)
                   & " /= "
                   & Result);
      end Test;

      Mem : VA_Regions_Type;
   begin
      --  Subtract from empty list
      Test (Mem_State => Mem,
            Ival  => Memory_Interval_Type'
              (First_Address => 16#0100#,
               Last_Address  => 16#0110#),
            Result    => "");

      --  Subtract from middle of one interval
      Add_Memory_Interval (List     => Mem,
                           Interval => Memory_Interval_Type'
                             (First_Address => 16#1000#,
                              Last_Address  => 16#2000#));
      Test (Mem_State => Mem,
            Ival      => Memory_Interval_Type'
              (First_Address => 16#1100#,
               Last_Address  => 16#1200#),
            Result    => "(16#1000#, 16#10ff#) "
              & "(16#1201#, 16#2000#)");

      --  Subtract from side of one interval (some overlap)
      Test (Mem_State => Mem,
            Ival      => Memory_Interval_Type'
              (First_Address => 16#0000#,
               Last_Address  => 16#1021#),
            Result    => "(16#1022#, 16#10ff#) "
              & "(16#1201#, 16#2000#)");

      --  Subtraction of superset
      Test (Mem_State => Mem,
            Ival      => Memory_Interval_Type'
              (First_Address => 16#0000#,
               Last_Address  => 16#1111#),
            Result    => "(16#1201#, 16#2000#)");

      --  Subtract with overlap, containment and adjacent region
      Add_Memory_Interval (List     => Mem,
                           Interval => Memory_Interval_Type'
                             (First_Address => 16#3000#,
                              Last_Address  => 16#4000#));
      Add_Memory_Interval (List     => Mem,
                           Interval => Memory_Interval_Type'
                             (First_Address => 16#6000#,
                              Last_Address  => 16#7000#));
      --  State of Mem is now:
      --  (16#1201#, 16#2000#) (16#3000#, 16#4000#) (16#6000#, 16#7000#)
      Test (Mem_State => Mem,
            Ival      => Memory_Interval_Type'
              (First_Address => 16#1500#,
               Last_Address  => 16#5fff#),
            Result    => "(16#1201#, 16#14ff#) (16#6000#, 16#7000#)");

      --  Subtract outside of list
      Test (Mem_State => Mem,
            Ival      => Memory_Interval_Type'
              (First_Address => 16#1500#,
               Last_Address  => 16#2000#),
            Result    => "(16#1201#, 16#14ff#) (16#6000#, 16#7000#)");

      --  Negative test: invalid region
      begin
         Subtract_Memory_Interval (List     => Mem,
                                   Interval => Memory_Interval_Type'
                                     (First_Address => 16#4000#,
                                      Last_Address  => 16#2000#));
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Invalid_Region =>
           Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Found invalid interval from '16#4000#' to '16#2000#'",
                   Message   => "Exception mismatch: "
                     & Ada.Exceptions.Exception_Message (X => E));
      end;

--  begin read only
   end Test_1_Subtract_Memory_Interval;
--  end read only


--  begin read only
   procedure Test_2_Subtract_Memory_Interval (Gnattest_T : in out Test_VA_Regions_Type);
   procedure Test_Subtract_Memory_Interval_0780bb (Gnattest_T : in out Test_VA_Regions_Type) renames Test_2_Subtract_Memory_Interval;
--  id:2.2/0780bb487b15c41b/Subtract_Memory_Interval/0/0/
   procedure Test_2_Subtract_Memory_Interval (Gnattest_T : in out Test_VA_Regions_Type) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Mem_State     : in out VA_Regions_Type;
                      First_Address :        Interfaces.Unsigned_64;
                      Size          :        Interfaces.Unsigned_64;
                      Result        :        String)
      is
      begin
         Subtract_Memory_Interval (List          => Mem_State,
                                   First_Address =>  First_Address,
                                   Size          => Size);
         Assert (Condition =>
                   VA_Regions_To_String (Regions => Mem_State) = Result,
                 Message   => "Memory regions mismatch: "
                   & VA_Regions_To_String (Regions => Mem_State)
                   & " /= "
                   & Result);
      end Test;

      Mem : VA_Regions_Type;
   begin
      --  Positive test
      Add_Memory_Interval (List     => Mem,
                           Interval => Memory_Interval_Type'
                             (First_Address => 16#3000#,
                              Last_Address  => 16#4000#));
      Add_Memory_Interval (List     => Mem,
                           Interval => Memory_Interval_Type'
                             (First_Address => 16#6000#,
                              Last_Address  => 16#7000#));
      Test (Mem_State     => Mem,
            First_Address => 16#3500#,
            Size          => 16#3000#,
            Result        => "(16#3000#, 16#34ff#) (16#6500#, 16#7000#)");

      --  Size 0
      Test (Mem_State     => Mem,
            First_Address => 16#3000#,
            Size          => 16#0000#,
            Result        => "(16#3000#, 16#34ff#) (16#6500#, 16#7000#)");

      --  Size too big
      begin
         Subtract_Memory_Interval (List          => Mem,
                                   First_Address => 16#0002#,
                                   Size          => Interfaces.Unsigned_64'Last);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Invalid_Region =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Unsigned_64 overflow when trying to allocate a "
                      & "fixed interval",
                    Message   => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
      end;
--  begin read only
   end Test_2_Subtract_Memory_Interval;
--  end read only


--  begin read only
   procedure Test_Reserve_Memory (Gnattest_T : in out Test_VA_Regions_Type);
   procedure Test_Reserve_Memory_779a4a (Gnattest_T : in out Test_VA_Regions_Type) renames Test_Reserve_Memory;
--  id:2.2/779a4aa2c67734fe/Reserve_Memory/1/0/
   procedure Test_Reserve_Memory (Gnattest_T : in out Test_VA_Regions_Type) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test (Mem_State        : in out VA_Regions_Type;
                      Size             :        Interfaces.Unsigned_64;
                      Result_Ref       :        String;
                      Mem_State_Result :        String)
      is
         Result : Interfaces.Unsigned_64;
      begin
         Result := Reserve_Memory (List => Mem_State,
                                   Size => Size);

         Assert (Condition => Mutools.Utils.To_Hex (Result) = Result_Ref,
                 Message   => "Address mismatch: "
                   &  Mutools.Utils.To_Hex (Result) & " /= " & Result_Ref);
         Assert (Condition => VA_Regions_To_String (Regions => Mem_State)
                   = Mem_State_Result,
                 Message   => "Memory regions mismatch: "
                   & VA_Regions_To_String (Regions => Mem_State)
                   & " /= "
                   & Mem_State_Result);
      end Test;

      Mem : VA_Regions_Type;
   begin
      --  Positive tests

      --  Reserve from one big interval
      Add_Memory_Interval (List     => Mem,
                           Interval => Memory_Interval_Type'
                             (First_Address => 16#2000#,
                              Last_Address  => 16#6000#));
      Test (Mem_State        => Mem,
            Size             => 16#1001#,
            Result_Ref       => "16#2000#",
            Mem_State_Result => "(16#3001#, 16#6000#)");

      --  Reserve from two intervals, first has equal size
      Add_Memory_Interval (List     => Mem,
                           Interval => Memory_Interval_Type'
                             (First_Address => 16#7000#,
                              Last_Address  => 16#8000#));
      Test (Mem_State        => Mem,
            Size             => 16#3000#,
            Result_Ref       => "16#3001#",
            Mem_State_Result => "(16#7000#, 16#8000#)");

      --  Reserve from multiple intervals and first interval is big enough
      Add_Memory_Interval (List     => Mem,
                           Interval => Memory_Interval_Type'
                             (First_Address => 16#9000#,
                              Last_Address  => 16#F500#));
      Test (Mem_State        => Mem,
            Size             => 16#0500#,
            Result_Ref       => "16#7000#",
            Mem_State_Result => "(16#7500#, 16#8000#) (16#9000#, 16#f500#)");

      --  Reserve from multiple intervals and only last is big enough
      Add_Memory_Interval (List     => Mem,
                           Interval => Memory_Interval_Type'
                             (First_Address => 16#0000#,
                              Last_Address  => 16#0500#));
      Test (Mem_State        => Mem,
            Size             => 16#2000#,
            Result_Ref       => "16#9000#",
            Mem_State_Result => "(16#0000#, 16#0500#) "
              & "(16#7500#, 16#8000#) "
              & "(16#b000#, 16#f500#)");

      --  Reserve with size 1
      Test (Mem_State        => Mem,
            Size             => 16#0001#,
            Result_Ref       => "16#0000#",
            Mem_State_Result => "(16#0001#, 16#0500#) "
              & "(16#7500#, 16#8000#) "
              & "(16#b000#, 16#f500#)");

      --  Reserve with maximal size
      Add_Memory_Interval (List     => Mem,
                           Interval => Memory_Interval_Type'
                             (First_Address => 16#0000#,
                              Last_Address  => 16#FFFF_FFFF_FFFF_FFFF#));
      Test (Mem_State        => Mem,
            Size             => 16#FFFF_FFFF_FFFF_FFFF#,
            Result_Ref       => "16#0000#",
            Mem_State_Result => "("
              & Mutools.Utils.To_Hex (Interfaces.Unsigned_64'Last)
              & ", "
              & Mutools.Utils.To_Hex (Interfaces.Unsigned_64'Last)
              & ")");

      --  Negative tests
      Clear (List => Mem);

      --  List is empty
      declare
         Result : Interfaces.Unsigned_64;
      begin
         Result := Reserve_Memory (List => Mem,
                                   Size => 16#0001#);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Out_Of_Memory =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Could not find free virtual memory region of size "
                      & "'16#0001#'",
                    Message   => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
      end;

      --  Size is 0
      Add_Memory_Interval (List             => Mem,
                           Interval         => Memory_Interval_Type'
                             (First_Address => 16#7000#,
                              Last_Address  => 16#A000#));
      declare
         Result : Interfaces.Unsigned_64;
      begin
         Result := Reserve_Memory (List => Mem,
                                   Size => 16#0000#);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Invalid_Region =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Cannot reserve region of size 0",
                    Message   => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
      end;

      --  All intervals are too small
      Add_Memory_Interval (List             => Mem,
                           Interval         => Memory_Interval_Type'
                             (First_Address => 16#0000#,
                              Last_Address  => 16#2000#));
      declare
         Result : Interfaces.Unsigned_64;
      begin
         Result := Reserve_Memory (List => Mem,
                                   Size => 16#3002#);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Out_Of_Memory =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Could not find free virtual memory region of size "
                      & "'16#3002#'",
                    Message   => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
      end;
--  begin read only
   end Test_Reserve_Memory;
--  end read only


--  begin read only
   procedure Test_1_Clear (Gnattest_T : in out Test_VA_Regions_Type);
   procedure Test_Clear_78e083 (Gnattest_T : in out Test_VA_Regions_Type) renames Test_1_Clear;
--  id:2.2/78e083865938ab3c/Clear/1/0/
   procedure Test_1_Clear (Gnattest_T : in out Test_VA_Regions_Type) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      Mem : VA_Regions_Type;
   begin
      Add_Memory_Interval (List             => Mem,
                           Interval         => Memory_Interval_Type'
                             (First_Address => 16#0000#,
                              Last_Address  => 16#2000#));
      Clear (List => Mem);
      Assert (Condition => Mem.Data.Is_Empty,
              Message   => "List of memory intervals is not empty:"
                & VA_Regions_To_String (Regions => Mem));

--  begin read only
   end Test_1_Clear;
--  end read only


--  begin read only
   procedure Test_Allocate_And_Set_Single_Resource (Gnattest_T : in out Test_VA_Regions_Type);
   procedure Test_Allocate_And_Set_Single_Resource_fc4cc0 (Gnattest_T : in out Test_VA_Regions_Type) renames Test_Allocate_And_Set_Single_Resource;
--  id:2.2/fc4cc0ba14be3f92/Allocate_And_Set_Single_Resource/1/0/
   procedure Test_Allocate_And_Set_Single_Resource (Gnattest_T : in out Test_VA_Regions_Type) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      Mem            : VA_Regions_Type;
      Implementation : DOM.Core.DOM_Implementation;
      Doc            : DOM.Core.Node
        := DOM.Core.Create_Document (Implementation);
      Node           : DOM.Core.Node;

      procedure Test_Event
        (Run_Type : Run_Type_Type;
         Attr_Name : String)
      is
      begin
         Node :=  DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "node");
         Clear (List => Mem);
         Add_Memory_Interval (List             => Mem,
                              Interval         => Memory_Interval_Type'
                                (First_Address => 10,
                                 Last_Address  => 15));
         Allocate_And_Set_Single_Resource
           (Av_Mem => Mem,
            Node   => Node,
            Run_Type => Run_Type);
         Assert (Condition => "10"
                   = DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => Attr_Name),
                 Message   => Attr_Name  & " mismatch: "
                   & DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => Attr_Name));
         Assert (Condition => VA_Regions_To_String (Regions => Mem)
                   = "(16#000b#, 16#000f#)",
                 Message   => "Memory intervals mismatch:"
                   & VA_Regions_To_String (Regions => Mem)
                   & " /= "
                   & "(16#100b#, 16#000f#)");
      end Test_Event;

   begin
      --  Positive test for VIRTUAL_ADDRESSES
      begin

         Node :=  DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "node");
         Add_Memory_Interval (List             => Mem,
                              Interval         => Memory_Interval_Type'
                                (First_Address => 16#0000#,
                                 Last_Address  => 16#2000#));
         Allocate_And_Set_Single_Resource
           (Av_Mem => Mem,
            Node   => Node,
            Run_Type => VIRTUAL_ADDRESSES,
            Size   => "16#1000#");
         Assert (Condition => "16#0000#"
                   = DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "virtualAddress"),
                 Message   => "virtualAddress mismatch: "
                   & DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "virtualAddress"));
         Assert (Condition => VA_Regions_To_String (Regions => Mem)
                   = "(16#1000#, 16#2000#)",
                 Message   => "Memory intervals mismatch:"
                   & VA_Regions_To_String (Regions => Mem)
                   & " /= "
                   & "(16#1000#, 16#2000#)");

         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "size",
            Value => "16#0004#");
         Allocate_And_Set_Single_Resource
           (Av_Mem => Mem,
            Node   => Node,
            Run_Type => VIRTUAL_ADDRESSES);
         Assert (Condition => "16#1000#"
                   = DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "virtualAddress"),
                 Message   => "virtualAddress mismatch: "
                   & DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "virtualAddress"));
         Assert (Condition => VA_Regions_To_String (Regions => Mem)
                   = "(16#1004#, 16#2000#)",
                 Message   => "Memory intervals mismatch:"
                   & VA_Regions_To_String (Regions => Mem)
                   & " /= "
                   & "(16#1004#, 16#2000#)");
      end;

      --  Positive test for READER_EVENTS and WRITER_EVENTS
      Test_Event (Run_Type => READER_EVENTS, Attr_Name => "vector");
      Test_Event (Run_Type => WRITER_EVENTS, Attr_Name => "event");

--  begin read only
   end Test_Allocate_And_Set_Single_Resource;
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
end Alloc.Map.VA_Regions_Type_Test_Data.VA_Regions_Type_Tests;
