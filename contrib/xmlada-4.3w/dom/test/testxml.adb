------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_05;

with Ada.Direct_IO;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;
with Ada.Unchecked_Deallocation;
with DOM.Core.Documents; use DOM.Core, DOM.Core.Documents;
with DOM.Core.Nodes;     use DOM.Core.Nodes;
with DOM.Readers;        use DOM.Readers;
with GNAT.Command_Line;  use GNAT.Command_Line;
with GNAT.Expect;        use GNAT.Expect;
with GNAT.OS_Lib;        use GNAT.OS_Lib;
with Input_Sources.File; use Input_Sources.File;
with Input_Sources.Http; use Input_Sources.Http;
with Input_Sources;      use Input_Sources;
with Sax.Encodings;      use Sax.Encodings;
with Sax.Readers;        use Sax.Readers;
with Sax.Symbols;        use Sax.Symbols;
with Sax.Utils;          use Sax.Utils;
with Testxml_Support;    use Testxml_Support;
with Unicode.CES;        use Unicode.CES;
with Unicode.Encodings;  use Unicode.Encodings;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

--  Try also
--     ./testxml http://java.sun.com/j2ee/1.4/docs/tutorial/examples/jaxp
--     /dom/samples/slideSample01.xml

procedure Testxml is
   Show_Invalid_Encoding   : constant Boolean := False;
   --  If True, an unsupported encoding reported by the XML parser is
   --  considered as a fatal error for the testsuite. If False, the test is
   --  simply ignored

   Run_XML_1_1_Tests       : Boolean := False;
   --  Whether we should run XML 1.1 tests. If False, only XML 1.0 tests are
   --  run

   Run_Disabled_Tests     : constant Boolean := False;
   --  If True, tests disabled in the "disable" file are run.

   Cst_Tmp_File1_Name : aliased String := "testxml_tmp1";
   Tmp_File1_Name     : constant GNAT.OS_Lib.String_Access :=
     Cst_Tmp_File1_Name'Unchecked_Access;
   Cst_Tmp_File2_Name : aliased String := "testxml_tmp2";
   Tmp_File2_Name     : constant GNAT.OS_Lib.String_Access :=
     Cst_Tmp_File2_Name'Unchecked_Access;
   --  Do not use temporary file names created by Create, since otherwise
   --  valgrind will report a memory leak in the GNAT runtime (which is not
   --  really a leak, just unfreed memory on exit).

   package String_Hash is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Boolean,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");
   use String_Hash;

   Disabled : String_Hash.Map;

   procedure Parse_Disabled;
   --  Parse the "disable" file, and set the [Disabled] variable

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Input_Source'Class, Input_Source_Access);

   package Character_IO is new Ada.Direct_IO (Character);

   Silent : Boolean := False;
   --  If True, do not print the resulting DOM tree when testing a single XML
   --  file.

   With_URI : Boolean := False;
   Dump : Boolean := False;
   Name_Start : Natural;
   Validate : Boolean := False;
   Valid_Chars : Boolean := True;
   Must_Normalize : Boolean := False;
   Support_Namespaces : Boolean := True;
   Encoding_Out : Unicode.Encodings.Unicode_Encoding := Get_By_Name ("utf-8");
   EOL : Byte_Sequence_Access := new Byte_Sequence'(Sax.Encodings.Lf_Sequence);
   Print_Comments : Boolean := False;
   Print_XML_PI   : Boolean := False;
   Collapse_Empty_Nodes : Boolean := False;
   Auto_Run : Boolean := False;
   Verbose : Boolean := False;
   Pretty_Print : Boolean := False;

   Symbols : Symbol_Table;

   type Testcase_Type is (Type_WF,      --  XML OK, Validation=No
                          Type_Not_WF,
                          Type_Valid,   --  XML OK, Validation=Yes
                          Type_Invalid,
                          Type_Error
                         );
   type Result_Type is (Result_Success, --  Matches expected result
                        Result_Failure, --  Doesn't match expected result
                        Result_Ignore,  --  Explicitly marked as "unsupported"
                        Result_XML_1_1, --  Test for XML 1.1
                        Result_Encoding, --  Invalid encoding
                        Result_IE);     --  Unexpected exception
   type Testcases_Results is array (Testcase_Type, Result_Type) of Natural;

   type Test_Description is record
      Base        : Unbounded_String;
      ID          : Unbounded_String;
      Description : Unbounded_String;
      URI         : Unbounded_String;
      Section     : Unbounded_String;
      Output      : Unbounded_String;
      Version     : Unbounded_String;
      Test_Type   : Testcase_Type;
      Edition     : XML_Versions;
      Namespace   : Boolean;
   end record;

   function Open_Input (XML_File : String) return Input_Source_Access;
   --  Open a given input_source. According to the file prefix, various types
   --  of sources can be open

   procedure Run_Single_Test (XML_File : String; Edition : XML_Versions);
   --  Parse XML_File, and print the output according to the command-line
   --  parameters

   procedure Run_Testsuite;
   --  Parse the W3C's testsuite description file, and run every test in it.
   --  Return True if all tests succeeded

   procedure Run_Testcases
     (N : Node; Base : String; Results : in out Testcases_Results);
   --  Parse a <TESTCASES> node from tests/xmlconf.xml to drive the automatic
   --  testsuite.

   procedure Run_Test
     (Entities       : String;
      Descr          : Test_Description;
      Results        : in out Testcases_Results);
   --  Run a single test from the W3C testsuite

   function Get_Attribute (N : Node; Attribute : String) return String;
   --  Query an attribute from N. The empty string is returned if the attribute
   --  does not exists

   function Diff_Output
     (Reader   : My_Tree_Reader'Class;
      Descr    : Test_Description) return String;
   --  Compare the output of a test with the expected output.

   function Image (Num : Integer; Width : Natural) return String;
   --  Return the image of [Num], on [Width] characters.
   --  This includes the leading whitespace

   procedure Print_Test_Result
     (Reader  : My_Tree_Reader'Class;
      Descr   : Test_Description;
      Result  : Result_Type;
      Msg     : String;
      Results : in out Testcases_Results);
   --  Print the result for the test.
   --  Checks the XML output of the test if needed

   function Test_Prefix (Typ : Testcase_Type) return String;
   --  Return a short description of the test type

   procedure Run_Error_Test
     (Reader    : in out My_Tree_Reader'Class;
      Input     : in out Input_Source'Class;
      Descr     : Test_Description;
      Results   : out Testcases_Results);
   procedure Run_Not_WF_Test
     (Reader    : in out My_Tree_Reader'Class;
      Input     : in out Input_Source'Class;
      Descr     : Test_Description;
      Results   : out Testcases_Results);
   procedure Run_Valid_Test
     (Reader    : in out My_Tree_Reader'Class;
      Input     : in out Input_Source'Class;
      Descr     : Test_Description;
      Results   : out Testcases_Results);
   procedure Run_Invalid_Test
     (Reader    : in out My_Tree_Reader'Class;
      Input     : in out Input_Source'Class;
      Descr     : Test_Description;
      Results   : out Testcases_Results);
   --  Run a single test, for each of the possible test category

   function Trim (Str : String) return String;
   --  Remove all leading white space characters in Str

   function Test_URI
     (Descr : Test_Description; URI : Unbounded_String) return String;
   --  Compute the URI for the test

   ----------
   -- Trim --
   ----------

   function Trim (Str : String) return String is
      S : Integer := Str'First;
   begin
      while S <= Str'Last
        and then (Str (S) = ' ' or else Str (S) = ASCII.LF)
      loop
         S := S + 1;
      end loop;

      if S <= Str'Last then
         return Str (S .. Str'Last);
      else
         return "";
      end if;
   end Trim;

   ----------------
   -- Open_Input --
   ----------------

   function Open_Input (XML_File : String) return Input_Source_Access is
      Read : Input_Source_Access;
   begin
      if XML_File'Length > 0 then
         if XML_File'Length > 6
           and then XML_File (XML_File'First .. XML_File'First + 6) = "http://"
         then
            Read := new Http_Input;
            Open (XML_File, Http_Input (Read.all));
         else
            Read := new File_Input;
            Open (XML_File, File_Input (Read.all));
         end if;

         --  Base file name should be used as the public Id
         Name_Start := XML_File'Last;
         while Name_Start >= XML_File'First
           and then XML_File (Name_Start) /= '/'
           and then XML_File (Name_Start) /= '\'
         loop
            Name_Start := Name_Start - 1;
         end loop;

         Set_Public_Id (Read.all, XML_File (Name_Start + 1 .. XML_File'Last));

         --  Full name is used as the system id
         Set_System_Id (Read.all, XML_File);
      else
         Read := new File_Input;
         Open ("test.xml", File_Input (Read.all));
      end if;
      return Read;

   exception
      when Ada.Text_IO.Name_Error =>
         return null;
   end Open_Input;

   ---------------------
   -- Run_Single_Test --
   ---------------------

   procedure Run_Single_Test (XML_File : String; Edition : XML_Versions) is
      Read : Input_Source_Access;
      Reader : My_Tree_Reader;
   begin
      Read := Open_Input (XML_File);
      if Read = null then
         return;
      end if;

      Set_XML_Version (Reader, Edition);
      Set_Feature (Reader, Namespace_Feature, Support_Namespaces);
      Set_Feature (Reader, Namespace_Prefixes_Feature, Support_Namespaces);
      Set_Feature (Reader, Validation_Feature, Validate);
      Set_Feature (Reader, Test_Valid_Chars_Feature, Valid_Chars);

      Set_Symbol_Table (Reader, Symbols);  --  optional, for efficiency

      Parse (Reader, Read.all);

      if Reader.Had_Error then
         Put_Line (Reader.Error_Msg.all);
      end if;

      Close (Read.all);
      Free (Reader.Error_Msg);
      Unchecked_Free (Read);

      if Must_Normalize then
         Normalize (Get_Tree (Reader));
      end if;

      if not Silent then
         if Dump then
            DOM.Core.Nodes.Dump (Get_Tree (Reader), With_URI => With_URI);
         else
            Write
              (Stream               => Stream (Current_Output),
               N                    => Get_Tree (Reader),
               Print_Comments       => Print_Comments,
               Print_XML_Declaration => Print_XML_PI,
               With_URI             => With_URI,
               EOL_Sequence         => EOL.all,
               Pretty_Print         => Pretty_Print,
               Encoding             => Encoding_Out,
               Collapse_Empty_Nodes => Collapse_Empty_Nodes);
         end if;
      end if;

      Free (Reader);

   exception
      when E : XML_Fatal_Error =>
         if Reader.Had_Error then
            Put_Line (Reader.Error_Msg.all);
         end if;
         Put_Line (Exception_Message (E));
   end Run_Single_Test;

   -----------
   -- Image --
   -----------

   function Image (Num : Integer; Width : Natural) return String is
      Str : constant String := Integer'Image (Num);
   begin
      if Str'Length < Width then
         return (1 .. Width - Str'Length => ' ') & Str;
      else
         return Str;
      end if;
   end Image;

   --------------------
   -- Parse_Disabled --
   --------------------

   procedure Parse_Disabled is
      File : File_Type;
      Line : String (1 .. 1024);
      Last : Natural;
   begin
      Open (File, Mode => In_File, Name => "disable");

      while not End_Of_File (File) loop
         Get_Line (File, Line, Last);
         if Line (1) /= '-' and then Line (1) /= ' ' then
            Disabled.Include
              (Key => Line (1 .. Last),
               New_Item => True);
         end if;
      end loop;

      Close (File);

   exception
      when Name_Error =>
         null;
   end Parse_Disabled;

   -------------------
   -- Run_Testsuite --
   -------------------

   procedure Run_Testsuite is
      Input : File_Input;
      Tests : Tree_Reader;
      N, Top : Node;
      Count : Testcases_Results := (others => (others => 0));
      Total : Natural;

   begin
      Parse_Disabled;
      Set_Symbol_Table (Tests, Symbols);  --  optional, for efficiency

      Open ("tests/xmlconf.xml", Input);
      Parse (Tests, Input);
      Close (Input);

      Top := Get_Element (Get_Tree (Tests));

      N := First_Child (Top);
      while N /= null loop
         if Node_Name (N) = "TESTCASES" then
            Run_Testcases (N, Get_Attribute (N, "xml:base"), Count);

         elsif Node_Type (N) = Element_Node then
            Put_Line ("Unknown node in xmlconf.xml: " & Node_Name (N));
            raise Program_Error;
         end if;

         N := Next_Sibling (N);
      end loop;

      New_Line;
      New_Line;
      Put_Line ("Release: " & Get_Attribute (Top, "PROFILE"));

      if not Run_XML_1_1_Tests then
         Put_Line ("N/A: XML 1.1 tests");
      end if;

      if not Show_Invalid_Encoding then
         Put_Line ("N/A: tests with encoding unknown to XML/Ada");
      end if;

      Put_Line
        ("+-----------+--------+---------+---------+"
         & "---------+---------+--------+----+");
      Put_Line
        ("|           | Total  | Success | Failure |"
         & " N/A     | XML 1.1 | Encod. | IE |");
      Put_Line
        ("+-----------+--------+---------+---------+"
         & "---------+---------+--------+----+");

      for T in Count'Range (1) loop
         declare
            Pref : constant String := Test_Prefix (T);
         begin
            Put ("| " & Pref & (1 .. 9 - Pref'Length => ' ') & " |");
         end;

         Total := 0;
         for T2 in Count'Range (2) loop
            Total := Total + Count (T, T2);
         end loop;

         Put_Line
           (Image (Total, 7)
            & " |" & Image (Count (T, Result_Success), 8)
            & " |" & Image (Count (T, Result_Failure), 8)
            & " |" & Image (Count (T, Result_Ignore),  8)
            & " |" & Image (Count (T, Result_XML_1_1),  8)
            & " |" & Image (Count (T, Result_Encoding),  7)
            & " |" & Image (Count (T, Result_IE), 3) & " |");
      end loop;

      Put_Line
        ("+-----------+--------+---------+---------+"
         & "---------+---------+--------+----+");
      Free (Tests);
   end Run_Testsuite;

   -------------------
   -- Get_Attribute --
   -------------------

   function Get_Attribute (N : Node; Attribute : String) return String is
      Attr : constant Node := Get_Named_Item (Attributes (N), Attribute);
   begin
      if Attr = null then
         return "";
      else
         return Node_Value (Attr);
      end if;
   end Get_Attribute;

   -------------------
   -- Run_Testcases --
   -------------------

   procedure Run_Testcases
     (N : Node; Base : String; Results : in out Testcases_Results)
   is
      Test  : Node := First_Child (N);
      Descr : Test_Description;
   begin
      Put_Line ("Profile: " & Get_Attribute (N, "PROFILE"));

      Descr.Base := To_Unbounded_String (Base);

      while Test /= null loop
         if Node_Name (Test) = "TEST" then
            if Get_Attribute (Test, "TYPE") = "valid" then
               Descr.Test_Type := Type_Valid;
            elsif Get_Attribute (Test, "TYPE") = "invalid" then
               Descr.Test_Type := Type_Invalid;
            elsif Get_Attribute (Test, "TYPE") = "not-wf" then
               Descr.Test_Type := Type_Not_WF;
            elsif Get_Attribute (Test, "TYPE") = "wf" then
               Descr.Test_Type := Type_WF;
            elsif Get_Attribute (Test, "TYPE") = "error" then
               Descr.Test_Type := Type_Error;
            else
               Put_Line ("Invalid test type: " & Get_Attribute (Test, "TYPE"));
               raise Program_Error;
            end if;

            Descr.ID := To_Unbounded_String (Get_Attribute (Test, "ID"));
            Descr.Description := To_Unbounded_String
              (Node_Value (First_Child (Test)));
            Descr.URI := To_Unbounded_String (Get_Attribute (Test, "URI"));
            Descr.Section := To_Unbounded_String
              (Get_Attribute (Test, "SECTIONS"));
            Descr.Output := To_Unbounded_String
              (Get_Attribute (Test, "OUTPUT"));
            Descr.Version := To_Unbounded_String
              (Get_Attribute (Test, "VERSION"));
            Descr.Namespace := Get_Attribute (Test, "NAMESPACE") /= "no";

            if Get_Attribute (Test, "EDITION") = "1 2 3 4" then
               Descr.Edition := XML_1_0_Fourth_Edition;
            else
               Descr.Edition := XML_1_0_Fifth_Edition;
            end if;

            Run_Test
              (Descr      => Descr,
               Entities   => Get_Attribute (Test, "ENTITIES"),
               Results    => Results);

         elsif Node_Name (Test) = "TESTCASES" then
            Run_Testcases (Test, Base, Results);

         elsif Node_Type (Test) = Element_Node then
            Put_Line
              (Standard_Error, "Unknown child of TEST: " & Node_Name (Test));
            raise Program_Error;
         end if;

         Test := Next_Sibling (Test);
      end loop;
   end Run_Testcases;

   --------------
   -- Test_URI --
   --------------

   function Test_URI
     (Descr : Test_Description; URI : Unbounded_String) return String
   is
      ID    : constant String := To_String (Descr.ID);
      S_URI : constant String := To_String (URI);
      Base  : constant String := To_String (Descr.Base);
   begin
      if Base'Length = 0 then
         if ID'Length > 8
           and then ID (ID'First .. ID'First + 7) = "rmt-ns11"
         then
            return Normalize_Pathname
              (Name          => S_URI,
               Directory     => "tests/eduni/namespaces/1.1",
               Resolve_Links => False);
         elsif ID'Length > 8
           and then ID (ID'First .. ID'First + 7) = "rmt-ns10"
         then
            return Normalize_Pathname
              (Name          => S_URI,
               Directory     => "tests/eduni/namespaces/1.0",
               Resolve_Links => False);
         elsif ID'Length > 6
           and then ID (ID'First .. ID'First + 6) = "rmt-e2e"
         then
            return Normalize_Pathname
              (Name          => S_URI,
               Directory     => "tests/eduni/errata-2e",
               Resolve_Links => False);
         elsif ID'Length > 3
           and then ID (ID'First .. ID'First + 3) = "rmt-"
         then
            return Normalize_Pathname
              (Name          => S_URI,
               Directory     => "tests/eduni/xml-1.1",
               Resolve_Links => False);
         else
            return "tests/" & S_URI;
         end if;

      elsif Base (Base'Last) = '/' then
         return Normalize_Pathname
           (Name          => S_URI,
            Directory     => "tests/" & To_String (Descr.Base),
            Resolve_Links => False);

      else
         return Normalize_Pathname
           (Name          => S_URI,
            Directory     => "tests/" & To_String (Descr.Base) & '/',
            Resolve_Links => False);
      end if;
   end Test_URI;

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test
     (Entities       : String;
      Descr          : Test_Description;
      Results        : in out Testcases_Results)
   is
      Path     : constant String := Test_URI (Descr, Descr.URI);
      Input    : Input_Source_Access;
      Reader   : My_Tree_Reader;

      procedure Cleanup;
      --  Free locally allocated variables

      procedure Cleanup is
      begin
         if Input /= null then
            Close (Input.all);
            Unchecked_Free (Input);
         end if;
         Free (Reader.Error_Msg);
         Free (Reader);
      end Cleanup;
   begin
      if not Run_XML_1_1_Tests and then Descr.Version = "1.1" then
         Print_Test_Result
           (Reader, Descr, Result_XML_1_1, "For XML 1.1", Results);
         return;
      end if;

      if not Run_Disabled_Tests
        and then Contains (Disabled, To_String (Descr.ID))
      then
         Print_Test_Result
           (Reader, Descr, Result_Ignore, "Disabled in XML/Ada", Results);
         return;
      end if;

      Input := Open_Input (Path);
      if Input = null then
         Print_Test_Result
           (Reader, Descr, Result_Ignore, "File not found: " & Path,
            Results);
         return;
      end if;

      Set_Symbol_Table (Reader, Symbols);  --  Optional, for efficiency

      Set_XML_Version (Reader, Descr.Edition);

      if not Descr.Namespace then
         Set_Feature (Reader, Namespace_Feature, False);
         Set_Feature (Reader, Namespace_Prefixes_Feature, False);
      end if;

      Set_Feature (Reader, Test_Valid_Chars_Feature, True);

      case Descr.Test_Type is
         when Type_Valid =>
            Set_Feature (Reader, Validation_Feature, True);
            Run_Valid_Test (Reader, Input.all, Descr, Results);

         when Type_WF =>
            Set_Feature (Reader, Validation_Feature, False);
            Run_Valid_Test (Reader, Input.all, Descr, Results);

         when Type_Not_WF =>
            Set_Feature (Reader, Validation_Feature, False);
            Run_Not_WF_Test (Reader, Input.all, Descr, Results);

         when Type_Invalid =>
            --  Run the test twice (once with validation, once without). Even
            --  if the test is unsupported, we still check that XML/Ada find
            --  the document as well-formed
            declare
               Descr2 : Test_Description := Descr;
            begin
               Descr2.Test_Type := Type_WF;
               Run_Test (Entities, Descr2, Results => Results);
            end;

            Set_Feature (Reader, Validation_Feature, True);
            Run_Invalid_Test (Reader, Input.all, Descr, Results);

         when Type_Error =>
            Set_Feature (Reader, Validation_Feature, False);
            Run_Error_Test (Reader, Input.all, Descr, Results);
      end case;

      Cleanup;

   exception
      when E : Invalid_Encoding =>
         Cleanup;
         if Show_Invalid_Encoding then
            Print_Test_Result
              (Reader, Descr, Result_Failure, "Invalid encoding: "
               & Exception_Message (E), Results);
         else
            Print_Test_Result
              (Reader, Descr, Result_Encoding, "Invalid encoding", Results);
         end if;

      when E : others =>
         Cleanup;
         Print_Test_Result
           (Reader, Descr,
            Result_IE, "Unexpected error: " & Exception_Message (E), Results);
   end Run_Test;

   -----------------
   -- Test_Prefix --
   -----------------

   function Test_Prefix (Typ : Testcase_Type) return String is
   begin
      case Typ is
         when Type_Valid   => return "XMLv";
         when Type_Not_WF  => return "XMLnot-wf";
         when Type_Invalid => return "XMLi";
         when Type_Error   => return "XMLerror ";
         when Type_WF      => return "XMLwf";
      end case;
   end Test_Prefix;

   -----------------------
   -- Print_Test_Result --
   -----------------------

   procedure Print_Test_Result
     (Reader  : My_Tree_Reader'Class;
      Descr   : Test_Description;
      Result  : Result_Type;
      Msg     : String;
      Results : in out Testcases_Results)
   is
      R : Result_Type := Result;
      M : Unbounded_String := To_Unbounded_String (Msg);
   begin
      if Result = Result_Success then
         declare
            D : constant String := Diff_Output (Reader, Descr);
         begin
            if D /= "" then
               R := Result_Failure;
               Append (M, ASCII.LF & D);
            end if;
         end;
      end if;

      case R is
         when Result_Success   => Put (" OK ");
         when Result_Failure   => Put (" NOK ");
         when Result_Ignore    => Put (" NA ");
         when Result_IE        => Put (" IE ");
         when Result_XML_1_1   => Put (" 1.1 ");
         when Result_Encoding  => Put (" ENC ");
      end case;

      Results (Descr.Test_Type, R) := Results (Descr.Test_Type, R) + 1;

      Put (Test_Prefix (Descr.Test_Type) & " ");
      Put (Descr.Edition'Img & " ");
      Put_Line ('[' & To_String (Descr.ID) & "] ");
      if Verbose then
         Put_Line ("  " & Test_URI (Descr, Descr.URI));
         Put_Line ("  Description: [" & To_String (Descr.Section) & "] "
                   & Trim (To_String (Descr.Description)));
      end if;

      if M /= "" then
         Put_Line ("   " & To_String (M));
      end if;
   end Print_Test_Result;

   -----------------
   -- Diff_Output --
   -----------------

   function Diff_Output
     (Reader   : My_Tree_Reader'Class;
      Descr    : Test_Description) return String
   is
      Expected : constant String := Test_URI (Descr, Descr.Output);
      use Character_IO;

      File  : Ada.Text_IO.File_Type;
      File2 : Character_IO.File_Type;
      File3 : Character_IO.File_Type;
      C, Previous, Previous2 : Character := ASCII.NUL;
      Last_Written : Character := ASCII.LF;
      In_Doctype : Boolean := False;
   begin
      if Descr.Output = "" then
         return "";
      end if;

      Create (File, Out_File, Cst_Tmp_File1_Name);
      Write (Stream              => Ada.Text_IO.Text_Streams.Stream (File),
             N                     => Get_Tree (Reader),
             Print_Comments        => Print_Comments,
             Print_XML_Declaration => Print_XML_PI,
             With_URI              => With_URI,
             EOL_Sequence          => EOL.all,
             Encoding              => Encoding_Out,
             Collapse_Empty_Nodes  => Collapse_Empty_Nodes);
      Close (File); --  Automatically adds a newline character at the end

      --  Process the expected output by removing the DTD, which
      --  is not stored in the DOM tree, and thus cannot be output
      Create (File2, Out_File, Cst_Tmp_File2_Name);
      Open (File3, In_File, Expected);
      while not End_Of_File (File3) loop
         Read (File3, C);

         if C = 'D'
           and then Previous2 = '<'
           and then Previous = '!'
         then
            In_Doctype := True;
            Previous := ASCII.NUL;
            Previous2 := ASCII.NUL;

         elsif In_Doctype
           and then C = ASCII.LF
           and then Previous = '>'
           and then Previous2 = ']'
         then
            In_Doctype := False;
            Previous := ASCII.NUL;
            Previous2 := ASCII.NUL;
            C := ASCII.NUL; --  Do not print
         end if;

         if not In_Doctype and then Previous2 /= ASCII.NUL then
            Write (File2, Previous2);
            Last_Written := Previous2;
         end if;

         Previous2 := Previous;
         Previous  := C;
      end loop;

      if not In_Doctype and then Previous2 /= ASCII.NUL then
         Write (File2, Previous2);
         Last_Written := Previous2;
      end if;
      if not In_Doctype and then Previous /= ASCII.NUL then
         Write (File2, Previous);
         Last_Written := Previous;
      end if;

      --  Ensure we end up with a newline, since otherwise some diffs will
      --  complain on some systems
      if Last_Written /= ASCII.LF then
         if Directory_Separator = '\' then
            Write (File2, ASCII.CR);
         end if;
         Write (File2, ASCII.LF);
      end if;

      Close (File3);
      Close (File2);

      declare
         Status : aliased Integer;
         D : constant String := Get_Command_Output
           (Command     => "diff",
            Arguments   => (1 => Tmp_File2_Name, 2 => Tmp_File1_Name),
            Input       => "",
            Status      => Status'Access,
            Err_To_Out  => True);
      begin
         if Status /= 0 then
            return D;
         end if;
      end;

      --  Can't delete, since they have been closed. Anyway, it is more
      --  convenient to analyze the output.
      --  Delete (File2);
      --  Delete (File);

      return "";
   end Diff_Output;

   --------------------
   -- Run_Error_Test --
   --------------------

   procedure Run_Error_Test
     (Reader    : in out My_Tree_Reader'Class;
      Input     : in out Input_Source'Class;
      Descr     : Test_Description;
      Results   : out Testcases_Results) is
   begin
      Parse (Reader, Input);

      if Reader.Had_Error then
         Print_Test_Result
           (Reader, Descr, Result_Success, Reader.Error_Msg.all, Results);
      else
         Print_Test_Result (Reader, Descr, Result_Failure, "", Results);
      end if;

   exception
      when E : XML_Fatal_Error =>
         Print_Test_Result
           (Reader, Descr, Result_Failure,
            "Unexpected Fatal_Error, must have Error" & ASCII.LF
            & Exception_Message (E), Results);
   end Run_Error_Test;

   ---------------------
   -- Run_Not_WF_Test --
   ---------------------

   procedure Run_Not_WF_Test
     (Reader    : in out My_Tree_Reader'Class;
      Input     : in out Input_Source'Class;
      Descr     : Test_Description;
      Results   : out Testcases_Results) is
   begin
      Parse (Reader, Input);
      Print_Test_Result (Reader, Descr, Result_Failure, "", Results);
   exception
      when E : XML_Fatal_Error =>
         Print_Test_Result
           (Reader, Descr, Result_Success, Exception_Message (E), Results);
   end Run_Not_WF_Test;

   --------------------
   -- Run_Valid_Test --
   --------------------

   procedure Run_Valid_Test
     (Reader    : in out My_Tree_Reader'Class;
      Input     : in out Input_Source'Class;
      Descr     : Test_Description;
      Results   : out Testcases_Results) is
   begin
      Parse (Reader, Input);
      Print_Test_Result (Reader, Descr, Result_Success, "", Results);

   exception
      when E : XML_Fatal_Error =>
         Print_Test_Result
           (Reader, Descr, Result_Failure, Exception_Message (E), Results);
   end Run_Valid_Test;

   ----------------------
   -- Run_Invalid_Test --
   ----------------------

   procedure Run_Invalid_Test
     (Reader    : in out My_Tree_Reader'Class;
      Input     : in out Input_Source'Class;
      Descr     : Test_Description;
      Results   : out Testcases_Results) is
   begin
      Parse (Reader, Input);

      if Reader.Had_Error then
         Print_Test_Result
           (Reader, Descr, Result_Success, Reader.Error_Msg.all, Results);
      else
         Print_Test_Result
           (Reader, Descr, Result_Failure, "", Results);
      end if;

   exception
      when E : XML_Fatal_Error =>
         Print_Test_Result
           (Reader, Descr, Result_Failure,
            "Unexpected Fatal_Error, must have Error" & ASCII.LF
            & Exception_Message (E), Results);
   end Run_Invalid_Test;

   Edition : XML_Versions := XML_1_0;
begin
   --  Since we are going to create multiple parsers, we will share the symbol
   --  table, which saves on the number of calls to malloc().
   --  This is however optional, since a parser would create its own symbol
   --  table when appropriate

   declare
      S : constant Symbol_Table_Access := new Symbol_Table_Record;
   begin
      Symbols := Symbol_Table_Pointers.Allocate (S);
   end;

   --  Parse the command line
   loop
      case Getopt
        ("silent uri normalize validate dump valid_chars encoding-out: eol:"
         & " comments xmlpi collapse nonamespaces auto verbose pretty xml11"
         & " edition:")
      is
         when ASCII.NUL => exit;
         when 'e' =>
            if Full_Switch = "eol" then
               Free (EOL);
               if Parameter = "\n" then
                  EOL := new String'("" & ASCII.LF);
               else
                  EOL := new String'(Parameter);
               end if;
            elsif Full_Switch = "encoding-out" then
               Encoding_Out := Get_By_Name (Parameter);

            elsif Full_Switch = "edition" then
               Edition := XML_Versions'Value (Parameter);
            end if;
         when 'x' =>
            if Full_Switch = "xmlpi" then
               Print_XML_PI := True;
            else
               Run_XML_1_1_Tests := True;
            end if;
         when 'c' =>
            if Full_Switch = "comments" then
               Print_Comments := True;
            else
               Collapse_Empty_Nodes := True;
            end if;
         when 's' => Silent := True;
         when 'u' => With_URI := True;
         when 'v' =>
            if Full_Switch = "validate" then
               Validate := True;
            elsif Full_Switch = "valid_chars" then
               Valid_Chars := False;
            elsif Full_Switch = "verbose" then
               Verbose := True;
            end if;
         when 'd' =>
            Dump := True;
         when 'p' =>
            Print_XML_PI := True;
            Pretty_Print := True;
            Collapse_Empty_Nodes := True;
            Print_Comments := True;
            Free (EOL);
            EOL := new String'("" & ASCII.LF);
         when 'n' =>
            if Full_Switch = "normalize" then
               Must_Normalize := True;
            else
               Support_Namespaces := False;
            end if;
         when 'a' => Auto_Run := True;

         when others => null;
      end case;
   end loop;

   if Auto_Run then
      Run_Testsuite;
   else
      Run_Single_Test (Get_Argument, Edition);
   end if;

   Free (EOL);
end Testxml;
