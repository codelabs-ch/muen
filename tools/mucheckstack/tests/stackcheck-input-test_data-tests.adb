--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Stackcheck.Input.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Stackcheck.Input.Test_Data.Tests is


--  begin read only
   procedure Test_Parse_Node (Gnattest_T : in out Test);
   procedure Test_Parse_Node_1ffc37 (Gnattest_T : in out Test) renames Test_Parse_Node;
--  id:2.2/1ffc37158ef08213/Parse_Node/1/0/
   procedure Test_Parse_Node (Gnattest_T : in out Test) is
   --  stackcheck-input.ads:28:4:Parse_Node
--  end read only

      pragma Unreferenced (Gnattest_T);

      Str_1 : constant String
        := "node: { title: ""exit_handlers__cpuid__process"" label: ""Process"
        & "\nsm/src/exit_handlers-cpuid.adb:30:4\n32 bytes (static)"" }";
      Str_2 : constant String
        := "node: { title: ""/muen/components/libdebuglog/src/debuglog-sink.a"
        & "db:debuglog__sink__rdtsc"" label: ""Rdtsc\n/muen/components/libdeb"
        & "uglog/src/debuglog-sink.adb:63:4\n16 bytes (static)""";
      No_Size : constant String
        := "node: { title: ""foo"" label: ""Process\n bytes (static)"" }";
      No_Name : constant String
        := "node: { title: """" label: ""Process\n64 bytes (static)"" }";

      Sub   : Types.Subprogram_Type;
      Valid : Boolean;
   begin
      Parse_Node (Data       => "",
                  Valid      => Valid,
                  Subprogram => Sub);
      Assert (Condition => not Valid,
              Message   => "Empty string valid");

      Parse_Node (Data       => "foobar string",
                  Valid      => Valid,
                  Subprogram => Sub);
      Assert (Condition => not Valid,
              Message   => "Invalid string valid");

      Parse_Node (Data       => No_Size,
                  Valid      => Valid,
                  Subprogram => Sub);
      Assert (Condition => not Valid,
              Message   => "String without stack size valid");

      Parse_Node (Data       => No_Name,
                  Valid      => Valid,
                  Subprogram => Sub);
      Assert (Condition => not Valid,
              Message   => "String without name valid");

      Parse_Node (Data       => Str_1,
                  Valid      => Valid,
                  Subprogram => Sub);
      Assert (Condition => Valid,
              Message   => "String not valid (1)");
      Assert (Condition => Types.Get_Stack_Usage (Subprogram => Sub) = 32,
              Message   => "Stack usage mismatch (1)");
      Assert (Condition => Types.Get_Name (Subprogram => Sub)
              = "exit_handlers__cpuid__process",
              Message   => "Name mismatch (1)");

      Parse_Node (Data       => Str_2,
                  Valid      => Valid,
                  Subprogram => Sub);
      Assert (Condition => Valid,
              Message   => "String not valid (2)");
      Assert (Condition => Types.Get_Stack_Usage (Subprogram => Sub) = 16,
              Message   => "Stack usage mismatch (2)");
      Assert (Condition => Types.Get_Name (Subprogram => Sub)
              = "debuglog__sink__rdtsc",
              Message   => "Name mismatch (2)");
--  begin read only
   end Test_Parse_Node;
--  end read only


--  begin read only
   procedure Test_Parse_Edge (Gnattest_T : in out Test);
   procedure Test_Parse_Edge_7217b5 (Gnattest_T : in out Test) renames Test_Parse_Edge;
--  id:2.2/7217b5a7fcf74232/Parse_Edge/1/0/
   procedure Test_Parse_Edge (Gnattest_T : in out Test) is
   --  stackcheck-input.ads:35:4:Parse_Edge
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Str_1  : constant String
        := "edge: { sourcename: ""devices__i8042__emulate"" targetname: ""debu"
        & "g_ops__put_line"" label : ""/muen/components/sm/src/devices-i8042.a"
        & "db:47:22"" }";
      Str_2  : constant String
        := "edge: { sourcename: ""debuglog__sink__flush"" targetname: ""/muen/"
        & "components/libdebuglog/src/debuglog-sink.adb:debuglog__sink__rdtsc"
        & """ label: ""/muen/components/libdebuglog/src/debuglog-sink.adb:85:3"
        & "5"" }";
      No_Src : constant String :=
        "edge: { sourcename: """" targetname: ""foobar"" }";
      No_Tgt : constant String :=
        "edge: { sourcename: ""foobar"" targetname: """" }";

      Src, Tgt : Unbounded_String;
      Valid    : Boolean;
   begin
      Parse_Edge (Data   => "",
                  Valid  => Valid,
                  Source => Src,
                  Target => Tgt);
      Assert (Condition => not Valid,
              Message   => "Empty string valid");

      Parse_Edge (Data   => "foobar string",
                  Valid  => Valid,
                  Source => Src,
                  Target => Tgt);
      Assert (Condition => not Valid,
              Message   => "Invalid string valid");

      Parse_Edge (Data   => No_Src,
                  Valid  => Valid,
                  Source => Src,
                  Target => Tgt);
      Assert (Condition => not Valid,
              Message   => "String without source valid");

      Parse_Edge (Data   => No_Tgt,
                  Valid  => Valid,
                  Source => Src,
                  Target => Tgt);
      Assert (Condition => not Valid,
              Message   => "String without target valid");

      Parse_Edge (Data   => Str_1,
                  Valid  => Valid,
                  Source => Src,
                  Target => Tgt);
      Assert (Condition => Valid,
              Message   => "String not valid (1)");
      Assert (Condition => To_String (Src) = "devices__i8042__emulate",
              Message   => "Source name mismatch (1)");
      Assert (Condition => To_String (Tgt) = "debug_ops__put_line",
              Message   => "Target name mismatch (1)");

      Parse_Edge (Data   => Str_2,
                  Valid  => Valid,
                  Source => Src,
                  Target => Tgt);
      Assert (Condition => Valid,
              Message   => "String not valid (2)");
      Assert (Condition => To_String (Src) = "debuglog__sink__flush",
              Message   => "Source name mismatch (2)");
      Assert (Condition => To_String (Tgt) = "debuglog__sink__rdtsc",
              Message   => "Target name mismatch (2)");
--  begin read only
   end Test_Parse_Edge;
--  end read only

end Stackcheck.Input.Test_Data.Tests;
