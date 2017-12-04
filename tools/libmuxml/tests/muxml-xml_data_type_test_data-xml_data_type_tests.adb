--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Muxml.XML_Data_Type_Test_Data.

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
package body Muxml.XML_Data_Type_Test_Data.XML_Data_Type_Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Parse (Gnattest_T : in out Test_XML_Data_Type);
   procedure Test_Parse_8fecef (Gnattest_T : in out Test_XML_Data_Type) renames Test_Parse;
--  id:2.2/8fecefff65fd7cbb/Parse/1/0/
   procedure Test_Parse (Gnattest_T : in out Test_XML_Data_Type) is
   --  muxml.ads:47:4:Parse
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      for K in Schema_Kind loop
         declare
            Data     : XML_Data_Type;
            K_Str    : constant String := Ada.Characters.Handling.To_Lower
              (Item => K'Img);
            Src_File : constant String := "data/" & K_Str & ".xml";
            Dst_File : constant String := "obj/" & K_Str & ".xml";
         begin
            Parse (Data => Data,
                   Kind => K,
                   File => Src_File);
            Write (Data => Data,
                   Kind => K,
                   File => Dst_File);
            Assert
              (Condition => Test_Utils.Equal_Files
                 (Filename1 => Src_File,
                  Filename2 => Dst_File),
               Message => "Stored " & K_Str & " XML differs from loaded one");
            Ada.Directories.Delete_File (Name => Dst_File);
         end;
      end loop;

      Load_Invalid_Format :
      declare
         Data : XML_Data_Type;
         pragma Unreferenced (Data);
      begin
         Parse (Data => Data,
                Kind => Format_B,
                File => "data/format_a.xml");
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when Validation_Error => null;
      end Load_Invalid_Format;

      Load_Invalid_Xml:
      declare
         Data : XML_Data_Type;
         pragma Unreferenced (Data);
      begin
         Parse (Data => Data,
                Kind => Format_B,
                File => "data/invalid.xml");
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when Validation_Error => null;
      end Load_Invalid_Xml;

      Load_Non_Xml_File:
      declare
         Ref_Msg : constant String
           := "Error validating XML data - data/invalid:1:1: Non-white space "
           & "found at top level";
         Data    : XML_Data_Type;
         pragma Unreferenced (Data);
      begin
         Parse (Data => Data,
                Kind => Format_B,
                File => "data/invalid");

         Assert (Condition => False,
                 Message   => "Exception expected (2)");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message
                    (X => E) = Ref_Msg,
                    Message   => "Exception message mismatch");
      end Load_Non_Xml_File;

      Load_Nonexistent_Xml:
      declare
         Ref_Msg : constant String := "Error reading XML file 'nonexistent' - "
           & "Could not open nonexistent";
         Data    : XML_Data_Type;
         pragma Unreferenced (Data);
      begin
         Parse (Data => Data,
                Kind => Format_B,
                File => "nonexistent");
         Assert (Condition => False,
                 Message   => "Exception expected (3)");

      exception
         when E : XML_Input_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message
                    (X => E) = Ref_Msg,
                    Message   => "Exception message mismatch");
      end Load_Nonexistent_Xml;
--  begin read only
   end Test_Parse;
--  end read only


--  begin read only
   procedure Test_Parse_String (Gnattest_T : in out Test_XML_Data_Type);
   procedure Test_Parse_String_75212d (Gnattest_T : in out Test_XML_Data_Type) renames Test_Parse_String;
--  id:2.2/75212d3c1652da1d/Parse_String/1/0/
   procedure Test_Parse_String (Gnattest_T : in out Test_XML_Data_Type) is
   --  muxml.ads:54:4:Parse_String
--  end read only

      pragma Unreferenced (Gnattest_T);

      XML_Data : constant String := Test_Utils.Read_File
        (Filename => "data/vcpu_profile.xml");
      Data : XML_Data_Type;
   begin
      Parse_String (Data => Data,
                    Kind => VCPU_Profile,
                    XML  => XML_Data);
      Write (Data => Data,
             Kind => VCPU_Profile,
             File => "obj/vcpu_profile_from_string.xml");

      Assert
        (Condition => Test_Utils.Equal_Files
           (Filename1 => "obj/vcpu_profile_from_string.xml",
            Filename2 => "data/vcpu_profile.xml"),
         Message   => "Error parsing XML string");
      Ada.Directories.Delete_File (Name => "obj/vcpu_profile_from_string.xml");

--  begin read only
   end Test_Parse_String;
--  end read only


--  begin read only
   procedure Test_Write (Gnattest_T : in out Test_XML_Data_Type);
   procedure Test_Write_d72a62 (Gnattest_T : in out Test_XML_Data_Type) renames Test_Write;
--  id:2.2/d72a62f5169b254f/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test_XML_Data_Type) is
   --  muxml.ads:60:4:Write
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : XML_Data_Type;
   begin
      Parse (Data => Data,
             Kind => Format_A,
             File => "data/format_a.xml");

      begin
         Write (Data => Data,
                Kind => Format_B,
                File => "obj/test_policy_b.xml");
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when Validation_Error => null;
      end;
--  begin read only
   end Test_Write;
--  end read only


--  begin read only
   procedure Test_Finalize (Gnattest_T : in out Test_XML_Data_Type);
   procedure Test_Finalize_1d29f1 (Gnattest_T : in out Test_XML_Data_Type) renames Test_Finalize;
--  id:2.2/1d29f15228a8f8f4/Finalize/1/0/
   procedure Test_Finalize (Gnattest_T : in out Test_XML_Data_Type) is
   --  muxml.ads:71:4:Finalize
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type DOM.Core.Node;

      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);
      Assert (Condition => DOM.Core.Nodes.Node_Name
              (N => Data.Doc) = "#document",
              Message   => "Node name mismatch");

      Finalize (Object => Data);

      Assert (Condition => Data.Doc = null,
              Message   => "XML doc not freed");
--  begin read only
   end Test_Finalize;
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
end Muxml.XML_Data_Type_Test_Data.XML_Data_Type_Tests;
