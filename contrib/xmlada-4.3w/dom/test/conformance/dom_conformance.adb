
--  Generates Ada test files from the XML files describing the standard
--  DOM conformance testsuite.
--  See http://dev.w3.org/cvsweb/2001/DOM-Test-Suite/

with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.Fixed;     use Ada.Strings, Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with DOM.Readers;           use DOM.Readers;
with DOM.Core.Attrs;        use DOM.Core.Attrs;
with DOM.Core.Documents;    use DOM.Core.Documents;
with DOM.Core.Nodes;        use DOM.Core, DOM.Core.Nodes;
with GNAT.Case_Util;        use GNAT.Case_Util;
with GNAT.OS_Lib;           use GNAT.OS_Lib;
with Input_Sources.File;    use Input_Sources.File;

procedure DOM_Conformance is

   function System (Str : String) return Integer;
   pragma Import (C, System, "system");

   procedure Generate_Test (Dir : String; Test : String);
   --  Generate the output for a specific test

   procedure Parse_Metadata (Metadata : Node);
   --  Parse the <metadata> node

   procedure Put_Begin (Has_Begin : in out Boolean);
   --  Insert the "begin" keyword if needed

   procedure Format_String (Str : String; Indent : String := "");
   --  Format Str in a format suitable for Ada sources (split on multiple
   --  lines, ...)

   function Type_To_Ada (Str : String) return String;
   --  Convert a type description as found in the XML files into a proper
   --  XML/Ada type

   function As_Boolean (Str : DOM_String) return DOM_String;
   --  Return Str as a boolean (proper casing for Ada code)

   procedure Run_Testsuite (Dir : String; Test_To_Run : String := "");
   --  Run a whole testsuite (or a specific test within the testsuite)

   -----------------
   -- Type_To_Ada --
   -----------------

   function Type_To_Ada (Str : String) return String is
   begin
      if Str = "DOMString" then
         return "Unbounded_String";
      elsif Str = "NodeList" then
         return "Node_List";
      elsif Str = "NamedNodeMap" then
         return "Named_Node_Map";
      elsif Str = "DocumentFragment" then
         return "Document_Fragment";
      elsif Str = "DocumentType" then
         return "Document_Type";
      elsif Str = "EntityReference" then
         return "Entity_Reference";
      elsif Str = "CdataSection" then
         return "Cdata_Section";
      elsif Str = "ProcessingInstruction" then
         return "Processing_Instruction";
      elsif Str = "Element" then
         return "Dom.Core.Element";
      else
         return Str;
      end if;
   end Type_To_Ada;

   -------------------
   -- Format_String --
   -------------------

   procedure Format_String (Str : String; Indent : String := "") is
      Index : Natural := Str'First;
      Is_First_Line : Boolean := True;
      Line_Start : Natural;
   begin
      --  Skip first empty line
      while Index <= Str'Last
        and then (Str (Index) = ' ' or Str (Index) = ASCII.LF)
      loop
         Index := Index + 1;
      end loop;

      Line_Start := Index;
      while Index <= Str'Last loop
         if Str (Index) = ASCII.LF then
            declare
               Line : constant String :=
                 Trim (Str (Line_Start .. Index - 1), Both);
            begin
               New_Line;
               Put (Indent);
               if not Is_First_Line then
                  Put ("& ");
               end if;

               if Line'Length = 0 then
                  Put ("ASCII.LF");
               else
                  Put ("""");
                  for L in Line'Range loop
                     if Line (L) = '"' then
                        Put ("""""");
                     else
                        Put (Line (L));
                     end if;
                  end loop;
                  Put ("""");
               end if;

               Is_First_Line := False;
               Line_Start := Index + 1;
            end;
         end if;
         Index := Index + 1;
      end loop;
   end Format_String;

   --------------------
   -- Parse_Metadata --
   --------------------

   procedure Parse_Metadata (Metadata : Node) is
      Description : Unbounded_String;
      N : Node := First_Child (Metadata);
   begin
      while N /= null loop
         if Node_Name (N) = "description" then
            Description := To_Unbounded_String (Node_Value (First_Child (N)));
            Put ("   Description : constant String :=");
            Format_String (To_String (Description), Indent => "      ");
            Put_Line (";");
            Put_Line ("   pragma Unreferenced (Description);");
         end if;
         N := Next_Sibling (N);
      end loop;
   end Parse_Metadata;

   ---------------
   -- Put_Begin --
   ---------------

   procedure Put_Begin (Has_Begin : in out Boolean) is
   begin
      if not Has_Begin then
         Put_Line ("begin");
         Has_Begin := True;
      end if;
   end Put_Begin;

   ----------------
   -- As_Boolean --
   ----------------

   function As_Boolean (Str : DOM_String) return DOM_String is
      S : DOM_String := Str;
   begin
      To_Mixed (S);
      return S;
   end As_Boolean;

   -------------------
   -- Generate_Test --
   -------------------

   procedure Generate_Test (Dir : String; Test : String) is
      File   : File_Input;
      Reader : Tree_Reader;
      Doc    : Document;
      N, Child : Node;
      Attrs  : Named_Node_Map;
      Has_Begin : Boolean := False;
      Is_Validating : Boolean := False;

      function Att (N : Node; Name : String) return DOM_String;
      --  Return a specific attribute of N

      function Has_Att (N : Node; Name : String) return Boolean;
      --  True if N has such an attribute

      procedure Single_Function (Name : String);
      --  Calls a function with a single argument ("obj"), and store its
      --  value in "var".

      procedure Two_Function (Name : String; Arg : String);
      --  Calls a function with two arguments (the name of the attribute for
      --  the second argument is given), and store result in "var".

      procedure Assert_Single (Name : String);
      --  Calls one of the assert function that takes a single argument

      ---------
      -- Att --
      ---------

      function Att (N : Node; Name : String) return DOM_String is
      begin
         return Value (Attr (Get_Named_Item (Attributes (N), Name)));
      end Att;

      -------------
      -- Has_Att --
      -------------

      function Has_Att (N : Node; Name : String) return Boolean is
      begin
         return Get_Named_Item (Attributes (N), Name) /= null;
      end Has_Att;

      ---------------------
      -- Single_Function --
      ---------------------

      procedure Single_Function (Name : String) is
      begin
         Put_Line
           ("   " & Att (N, "var")
            & " := " & Name & " (" & Att (N, "obj") & ");");
      end Single_Function;

      ------------------
      -- Two_Function --
      ------------------

      procedure Two_Function (Name : String; Arg : String) is
      begin
         Put_Line
           ("   " & Att (N, "var")
            & " := " & Name & " (" & Att (N, "obj") & ", "
            & Att (N, Arg) & ");");
      end Two_Function;

      -------------------
      -- Assert_Single --
      -------------------

      procedure Assert_Single (Name : String) is
      begin
         Put_Line ("   " & Name & " (" & Att (N, "actual") & ",");
         Put_Line
           ("     Id => """ & Att (N, "id")
            & """, File => Test_Name);");
      end Assert_Single;

   begin
      Open (Dir & Test, File);
      Parse (Reader, File);
      Close (File);

      Put_Line ("with Ada.Command_Line;      use Ada.Command_Line;");
      Put_Line ("with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;");
      Put_Line ("with Ada.Text_IO;           use Ada.Text_IO;");
      Put_Line ("with DOM.Core.Nodes;        use DOM.Core, DOM.Core.Nodes;");
      Put_Line ("with DOM.Core.Documents;    use DOM.Core.Documents;");
      Put_Line ("with DOM.Core.Attrs;        use DOM.Core.Attrs;");
      Put_Line ("with DOM.Core.Elements;     use DOM.Core.Elements;");
      Put_Line ("with DOM.Readers;           use DOM.Readers;");
      Put_Line ("with Input_Sources.File;    use Input_Sources.File;");
      Put_Line ("with Driver;                use Driver;");
      Put_Line ("with SAX.Readers;           use SAX.Readers;");
      Put_Line ("procedure DOMTest is");
      Put_Line ("   Reader : Tree_Reader;");
      Put_Line ("   File   : File_Input;");

      Put_Line ("   Test_Name : constant String := """ & Test & """;");

      Doc := Get_Tree (Reader);
      N   := First_Child (Get_Element (Doc));
      while N /= null loop
         if Node_Type (N) = Comment_Node
           or else Node_Type (N) = Text_Node
         then
            null;

         elsif Node_Name (N) = "metadata" then
            Parse_Metadata (N);

         elsif Node_Name (N) = "implementationAttribute" then
            if Att (N, "name") = "validating" then
               Is_Validating := Boolean'Value (Att (N, "value"));
            elsif Att (N, "name") = "expandEntityReferences" then
               null;
            else
               Put_Line
                 (Standard_Error, "Unknown implementation attribute: "
                  & Att (N, "name"));
               raise Program_Error;
            end if;

         elsif Node_Name (N) = "var" then
            Put_Line
              ("   " & Att (N, "name")
               & " : " & Type_To_Ada (Att (N, "type")) & ";");

         elsif Node_Name (N) = "load" then
            Put_Begin (Has_Begin);

            if Is_Validating then
               Put_Line ("   Set_Feature (Reader, Validation_Feature, True);");
            end if;

            Put_Line ("   Open (""" & Dir & "files/"
                      & Att (N, "href") & ".xml"", File);");
            Put_Line ("   Parse (Reader, File);");
            Put_Line ("   Close (File);");
            Put_Line ("   " & Att (N, "var") & " := Get_Tree (Reader);");

         elsif Node_Name (N) = "item" then
            Two_Function ("Item", "index");
         elsif Node_Name (N) = "createElement" then
            Two_Function ("Create_Element", "tagName");
         elsif Node_Name (N) = "appendChild" then
            Two_Function ("Append_Child", "newChild");
         elsif Node_Name (N) = "getNamedItem" then
            Two_Function ("Get_Named_Item", "name");
         elsif Node_Name (N) = "createEntityReference" then
            Two_Function ("Create_Entity_Reference", "name");

         elsif Node_Name (N) = "firstChild" then
            Single_Function ("First_Child");
         elsif Node_Name (N) = "parentNode" then
            Single_Function ("Parent_Node");
         elsif Node_Name (N) = "createDocumentFragment" then
            Single_Function ("Create_Document_Fragment");
         elsif Node_Name (N) = "nextSibling" then
            Single_Function ("Next_Sibling");
         elsif Node_Name (N) = "childNodes" then
            Single_Function ("Child_Nodes");
         elsif Node_Name (N) = "previousSibling" then
            Single_Function ("Previous_Sibling");
         elsif Node_Name (N) = "attributes" then
            Single_Function ("DOM.Core.Nodes.Attributes");
         elsif Node_Name (N) = "specified" then
            Single_Function ("Specified");

         elsif Node_Name (N) = "assertNull" then
            Assert_Single ("Assert_Null");
         elsif Node_Name (N) = "assertNotNull" then
            Assert_Single ("Assert_Not_Null");
         elsif Node_Name (N) = "assertFalse" then
            Assert_Single ("Assert_False");
         elsif Node_Name (N) = "assertTrue" then
            Assert_Single ("Assert_True");

         elsif Node_Name (N) = "removeAttribute" then
            Put_Line ("   Remove_Attribute ("
                      & Att (N, "obj") & ", " & Att (N, "name") & ");");

         elsif Node_Name (N) = "getElementsByTagName" then
            Put_Begin (Has_Begin);
            Put_Line ("   "
                      & Att (N, "var")
                      & " := Dom.Core." & Att (N, "interface")
                      & "s.Get_Elements_By_Tag_Name ("
                      & Att (N, "obj") & ", "
                      & Att (N, "tagname") & ");");

         elsif Node_Name (N) = "nodeName" then
            Put_Line
              ("   " & Att (N, "var")
               & " := To_Unbounded_String (DOM.Core.Nodes.Node_Name ("
               & Att (N, "obj") & "));");

         elsif Node_Name (N) = "nodeValue" then
            if Has_Att (N, "value") then
               Put_Line
                 ("   Set_Node_Value (" & Att (N, "obj") & ", "
                  & Att (N, "value") & ");");
            else
               Put_Line
                 ("   " & Att (N, "var")
                  & " := To_Unbounded_String (Node_Value ("
                  & Att (N, "obj") & "));");
            end if;

         elsif Node_Name (N) = "setAttribute" then
            Put_Line
              ("   Set_Attribute (" & Att (N, "obj")
               & ", " & Att (N, "name") & ", " & Att (N, "value") & ");");

         elsif Node_Name (N) = "name" then
            Put_Line
              ("   " & Att (N, "var")
               & " := To_Unbounded_String (DOM.Core.Attrs.Name ("
               & Att (N, "obj") & "));");

         elsif Node_Name (N) = "value" then
            if Has_Att (N, "value") then
               Put_Line
                 ("   Set_Value (" & Att (N, "obj") & ", "
                  & Att (N, "value") & ");");
            else
               Put_Line
                 ("   " & Att (N, "var")
                  & " := To_Unbounded_String (DOM.Core.Attrs.Value ("
                  & Att (N, "obj") & "));");
            end if;

         elsif Node_Name (N) = "assertEquals" then
            Put_Line ("   Assert_Equals (" & Att (N, "expected") & ",");
            Put_Line ("    " & Att (N, "actual") & ",");
            Put_Line
              ("    Ignore_Case => " & As_Boolean (Att (N, "ignoreCase"))
               & ", Id => """ & Att (N, "id")
               & """, File => Test_Name);");

         elsif Node_Name (N) = "assertDOMException" then
            Child := First_Child (N);
            while Child /= null and Node_Type (Child) /= Element_Node loop
               Child := Next_Sibling (Child);
            end loop;

            Put_Line ("   begin");
            Put_Line ("     null;");
            Put_Line ("     Put_Line (""Should have raised exception"");");
            Put_Line ("     Set_Exit_Status (1);");
            Put_Line ("   exception");
            Put_Line ("     when " & Node_Name (Child) & " => null;");
            Put_Line ("     when others =>");
            Put_Line ("        Put_Line (""Unexpected exception"");");
            Put_Line ("        Set_Exit_Status (1);");
            Put_Line ("        return;");
            Put_Line ("   end;");

         else
            Put_Line (Standard_Error, "Unknown node: " & Node_Name (N));
            Put_Line (Standard_Error, "in " & Dir & Test);
            raise Program_Error;
         end if;

         N := Next_Sibling (N);
      end loop;

      Put_Line ("end DOMTest;");
   end Generate_Test;

   -------------------
   -- Run_Testsuite --
   -------------------

   procedure Run_Testsuite (Dir : String; Test_To_Run : String := "") is
      File   : File_Input;
      Reader : Tree_Reader;
      N      : Node;
      Attrs  : Named_Node_Map;
      Output : File_Type;
      Success : Boolean;
   begin
      Open (Dir & "alltests.xml", File);
      Parse (Reader, File);
      Close (File);

      N := First_Child (Get_Element (Get_Tree (Reader)));
      while N /= null loop
         if Node_Name (N) = "suite.member" then
            Attrs := Attributes (N);

            declare
               Test : constant String :=
                 Value (Attr (Get_Named_Item (Attrs, "href")));
            begin
               if Test_To_Run = ""
                 or else Test = Test_To_Run
               then
                  Put_Line ("==== Running " & Test);
                  Create (Output, Out_File, "domtest.adb");
                  Set_Output (Output);
                  Generate_Test (Dir => Dir, Test => Test);
                  Set_Output (Standard_Output);
                  Close (Output);

                  --  Will force a compilation, otherwise timestamps might
                  --  prevent this
                  Delete_File ("domtest.o", Success);

                  if System
                    ("gnatmake -q -Pconformance domtest.adb" & ASCII.NUL) /= 0
                  then
                     Put_Line
                       (Standard_Error, "Could not compile domtest.adb");
                     Set_Exit_Status (Failure);
                     return;
                  end if;

                  if System ("./domtest" & ASCII.NUL) /= 0 then
                     Put_Line ("Failure: " & Dir & Test);
                  end if;
               end if;
            end;
         end if;
         N := Next_Sibling (N);
      end loop;

   exception
      when Program_Error =>
         Close (Output);
         raise;
   end Run_Testsuite;

begin
   if Argument_Count >= 2 then
      Run_Testsuite (Dir => Argument (1), Test_To_Run => Argument (2));
   else
      Run_Testsuite (Dir => Argument (1));
   end if;
end DOM_Conformance;
