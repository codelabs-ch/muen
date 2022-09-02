--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.Templates.Test_Data.

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
package body Mutools.Templates.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_1_Create (Gnattest_T : in out Test);
   procedure Test_Create_4652d7 (Gnattest_T : in out Test) renames Test_1_Create;
--  id:2.2/4652d7f723cf959d/Create/1/0/
   procedure Test_1_Create (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      T : Template_Type;
   begin
      T := Create (Content => Tmpl);
      Assert (Condition => Ada.Strings.Unbounded.Length
              (Source => T.Data) = 55,
              Message   => "Template size mismatch");
--  begin read only
   end Test_1_Create;
--  end read only


--  begin read only
   procedure Test_Replace (Gnattest_T : in out Test);
   procedure Test_Replace_a74ecb (Gnattest_T : in out Test) renames Test_Replace;
--  id:2.2/a74ecbbbacf1d1be/Replace/1/0/
   procedure Test_Replace (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Out_File : constant String := "obj/template";
      T        : Template_Type;
   begin
      T.Data := Ada.Strings.Unbounded.To_Unbounded_String (Source => Tmpl);

      begin
         Replace (Template => T,
                  Pattern  => "NONEXISTENT_PATTERN",
                  Content  => "foobar");
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when Pattern_Not_Found => null;
      end;

      Replace (Template => T,
               Pattern  => "PATTERN1",
               Content  => "processed");
      Replace (Template => T,
               Pattern  => "PATTERN2",
               Content  => "file");
      Write (Template => T,
             Filename => Out_File);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/template.ref",
               Filename2 => Out_File),
              Message   => "Template mismatch");

      Ada.Directories.Delete_File (Name => Out_File);
--  begin read only
   end Test_Replace;
--  end read only


--  begin read only
   procedure Test_1_Write (Gnattest_T : in out Test);
   procedure Test_Write_53091d (Gnattest_T : in out Test) renames Test_1_Write;
--  id:2.2/53091d21d5c910db/Write/1/0/
   procedure Test_1_Write (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Out_File : constant String := "obj";
      T        : Template_Type;
   begin
      begin
         Write (Template => T,
                Filename => Out_File);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : IO_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Unable to write template to 'obj' - Unable to open file"
                    & " 'obj' - obj: Is a directory",
                    Message   => "Exception message mismatch");
      end;
--  begin read only
   end Test_1_Write;
--  end read only


--  begin read only
   procedure Test_2_Create (Gnattest_T : in out Test);
   procedure Test_Create_755d54 (Gnattest_T : in out Test) renames Test_2_Create;
--  id:2.2/755d54b6c5dce8b4/Create/0/0/
   procedure Test_2_Create (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Containers.Count_Type;

      T : Stream_Template_Type
        := Create (Content  => "C1__t1__C2__t2__C3__t3__C4",
                   Filename => "obj/tmpl-create");
   begin
      Assert (Condition => T.Tokens.Length = 4,
              Message   => "Length mismatch:" & T.Tokens.Length'Img);
      Assert (Condition => Ada.Directories.Exists (Name => "obj/tmpl-create"),
              Message   => "File not created");
      Close (Template => T);
      Ada.Directories.Delete_File (Name => "obj/tmpl-create");
--  begin read only
   end Test_2_Create;
--  end read only


--  begin read only
   procedure Test_Stream (Gnattest_T : in out Test);
   procedure Test_Stream_4d2af0 (Gnattest_T : in out Test) renames Test_Stream;
--  id:2.2/4d2af00a423c98dc/Stream/1/0/
   procedure Test_Stream (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
     
      Out_File : constant String := "obj/tmpl-stream";
      Tmpl     : constant String := "A1__t1__A2" & ASCII.LF & "A3__t2__B1__t3__B2";
      T        : Stream_Template_Type
        := Create (Content  => Tmpl,
                   Filename => Out_File);
   begin
      for I in 1 .. 4 loop
         Stream (Template => T);
      end loop;
      Close (Template => T);
      
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/stream_template.ref",
               Filename2 => Out_File),
              Message   => "Template mismatch");

      begin
         Stream (Template => T);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : IO_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E) 
                    = "Unable to stream: no content left",
                    Message   => "Exception mismatch");
      end;

      Ada.Directories.Delete_File (Name => Out_File);
--  begin read only
   end Test_Stream;
--  end read only


--  begin read only
   procedure Test_2_Write (Gnattest_T : in out Test);
   procedure Test_Write_cb9d07 (Gnattest_T : in out Test) renames Test_2_Write;
--  id:2.2/cb9d07d65e5e8392/Write/0/0/
   procedure Test_2_Write (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Tested in Stream() test");
--  begin read only
   end Test_2_Write;
--  end read only


--  begin read only
   procedure Test_Close (Gnattest_T : in out Test);
   procedure Test_Close_c2d1dc (Gnattest_T : in out Test) renames Test_Close;
--  id:2.2/c2d1dc7c20263422/Close/1/0/
   procedure Test_Close (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Closes fd only");
--  begin read only
   end Test_Close;
--  end read only


--  begin read only
   procedure Test_Tokenize (Gnattest_T : in out Test);
   procedure Test_Tokenize_6096f3 (Gnattest_T : in out Test) renames Test_Tokenize;
--  id:2.2/6096f3a80bc73e43/Tokenize/1/0/
   procedure Test_Tokenize (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Containers.Count_Type;

      T1 : constant String := "C1__t1_a__C2";
      T2 : constant String := "B1";
      T3 : constant String := ASCII.LF & "A1" & ASCII.LF & "  __t1__something"
      & ASCII.LF & "A2__t2__";
      T4 : constant String := "";
      L  : USLP.List;
   begin
      Tokenize 
        (Data => T1,
         List => L);
      Assert 
        (Condition => L.Length = 2,
         Message   => "Two tokens expected:" & L.Length'Img);
      Assert 
        (Condition => L.First_Element = To_Unbounded_String ("C1"),
         Message   => "First element mismatch: " & To_String (L.First_Element));
      Assert 
        (Condition => L.Last_Element = To_Unbounded_String ("C2"),
         Message   => "Last element mismatch: " & To_String (L.Last_Element));
      L.Clear;

      Tokenize 
        (Data => T2,
         List => L);
      Assert 
        (Condition => L.Length = 1,
         Message   => "Only one token expected:" & L.Length'Img);
      Assert 
        (Condition => L.First_Element = To_Unbounded_String ("B1"),
         Message   => "First element mismatch (2): " & To_String (L.First_Element));
      L.Clear;

      Tokenize 
        (Data => T3,
         List => L);
      Assert 
        (Condition => L.Length = 2,
         Message   => "Two tokens expected (2):" & L.Length'Img);
      Assert 
        (Condition => L.First_Element = To_Unbounded_String
          (ASCII.LF & "A1" & ASCII.LF & "  "),
         Message   => "First element mismatch (3): " & To_String (L.First_Element));
      Assert 
        (Condition => L.Last_Element = To_Unbounded_String
          ("something" & ASCII.LF & "A2"),
         Message   => "Last element mismatch (2): " & To_String (L.Last_Element));
      L.Clear;

      Tokenize 
        (Data => T4,
         List => L);
      Assert 
        (Condition => L.Length = 0,
         Message   => "Zero tokens expected:" & L.Length'Img);
--  begin read only
   end Test_Tokenize;
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
end Mutools.Templates.Test_Data.Tests;
