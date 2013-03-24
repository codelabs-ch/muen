------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

--  This is a small example showing how to parse one or more XML schemas
--  and then validate one XML document. To use this tool, do a
--  "make test" at the root of the XML/Ada distribution, then run
--      ./testschema -xsd schema1.xsd -xsd schema2.xsd file1.xml file2.xml
--  where schema1.xsd, schema2.xsd, schema3.xsd,... are our schema files
--  to parse, and file.xml the XML document to validate

with Ada.Text_IO;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;
with Schema.Readers;        use Schema.Readers;
with Schema.Dom_Readers;    use Schema.Dom_Readers;
with Schema.Schema_Readers; use Schema.Schema_Readers;
with Schema.Validators;     use Schema.Validators;
with DOM.Core.Nodes;        use DOM.Core.Nodes;
with DOM.Core;              use DOM.Core;
with DOM.Core.Documents;    use DOM.Core.Documents;
with Input_Sources.File;    use Input_Sources.File;
with Ada.Exceptions;        use Ada.Exceptions;
with GNAT.IO;               use GNAT.IO;
with Sax.Readers;           use Sax.Readers;
with Sax.Utils;             use Sax.Utils;
with GNAT.Command_Line;     use GNAT.Command_Line;

procedure TestSchema is
   Read         : File_Input;
   My_Reader    : Validating_Reader_Access;
   Schema       : Schema_Reader;
   Grammar      : XML_Grammar := No_Grammar;
   Explicit_XSD : Boolean := False;
   Switches     : constant String := "xsd: debug base dom h";
   DOM          : Boolean := False;
   Base_Names   : Boolean := False;
   Tree         : Document;

begin
   --  Special case: check if we want debug output, before doing anything else
   loop
      case Getopt (Switches) is
         when 'h' =>
            Put_Line ("-xsd file     Specifies location of XSD file");
            Put_Line ("-debug        Print extra debugging info");
            Put_Line ("-base         Use basenames in error messages");
            Put_Line ("-dom          Dump the DOM tree after parsing");
            Put_Line ("               Uses a DOM-based parser, instead of");
            Put_Line ("               the default sax-based parser");
            return;

         when 'd' =>
            if Full_Switch = "debug" then
               Standard.Schema.Set_Debug_Output (True);
            elsif Full_Switch = "dom" then
               DOM := True;
            end if;
         when ASCII.NUL =>
            exit;
         when others =>
            null;  --  Handled later
      end case;
   end loop;

   --  We want to validate with possibly several schemas to parse first. This
   --  is slightly more complex than a single grammar, since some checks can
   --  only be done at the end, and we need to let XML/Ada know about that.

   Set_XSD_Version (Grammar, XSD_1_0);
   Set_XML_Version (Schema, XML_1_0_Third_Edition);
   Set_Grammar (Schema, Grammar);
   Initialize_Option_Scan;

   loop
      case Getopt (Switches) is
         when 'x' =>
            Open (Parameter, Read);
            begin
               Parse (Schema, Read);
               Close (Read);
            exception
               when others =>
                  Close (Read);
                  raise;
            end;

            Explicit_XSD := True;

         when 'b' =>
            Base_Names := True;
            Use_Basename_In_Error_Messages (Schema, Base_Names);

         when 'd' =>
            null; --  Already handled

         when others =>
            exit;
      end case;
   end loop;

   --  Create the parser

   if DOM then
      My_Reader := new Standard.Schema.Dom_Readers.Tree_Reader;
   else
      My_Reader := new Standard.Schema.Readers.Validating_Reader;
   end if;

   Set_XML_Version (My_Reader.all, XML_1_0_Third_Edition);
   Use_Basename_In_Error_Messages (My_Reader.all, Base_Names);

   --  If we have at least one schema, we need to perform the final checks
   --  to make sure they are correct and leave no undefined entity.

   if Explicit_XSD then
      --  Validate the documents with the schemas we have just parsed.
      Set_Grammar (My_Reader.all, Get_Grammar (Schema));
   end if;

   Free (Schema);  --  No longer needed

   --  Activate validation. Even though we have a validating reader, we can
   --  still choose to disable validation if we know the document is correct.
   --  This makes loading the document faster

   Set_Feature (My_Reader.all, Schema_Validation_Feature, True);

   --  Now valid all XML files given as input

   loop
      declare
         Xml_File : constant String := Get_Argument;
         List : Node_List;
      begin
         exit when Xml_File'Length = 0;

         Open (Xml_File, Read);
         Parse (My_Reader.all, Read);
         Close (Read);

         if DOM then
            Write
              (Stream => Stream (Ada.Text_IO.Current_Output),
               N      => Get_Tree (Tree_Reader (My_Reader.all)),
               Print_XML_Declaration => False,
               EOL_Sequence => "");

            List := Get_Elements_By_Tag_Name
              (Get_Tree (Tree_Reader (My_Reader.all)),
               Tag_Name => "Corrective_Action");
            if Item (List, 0) /= null then
               Put_Line
                 ("Found " & Length (List)'Img & " Corrective_Action nodes");
               Put_Line
                 ("Value=" & Node_Value (Item (List, 0)));
            end if;
         end if;
      end;
   end loop;

   if DOM then
      Tree := Get_Tree (Tree_Reader (My_Reader.all));
   end if;

   Free (My_Reader);

   --  You can keep using the tree here, it is still valid.

   Standard.DOM.Core.Nodes.Free (Tree);

exception
   when XML_Validation_Error =>
      if My_Reader /= null then
         Put_Line (Get_Error_Message (My_Reader.all));
      else
         Put_Line (Get_Error_Message (Schema));
      end if;

      Close (Read);
      Free (My_Reader);

   when Standard.Schema.XML_Not_Implemented =>
      if My_Reader = null then
         Put_Line ("NOT IMPLEMENTED: " & Get_Error_Message (Schema));
      else
         Put_Line ("NOT IMPLEMENTED: " & Get_Error_Message (My_Reader.all));
      end if;

      Close (Read);
      Free (My_Reader);

   when E : XML_Fatal_Error =>
      Put_Line (Exception_Message (E));
      Close (Read);
      Free (My_Reader);
end TestSchema;
