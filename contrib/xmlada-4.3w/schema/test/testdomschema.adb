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

with Schema.Schema_Readers; use Schema.Schema_Readers;
with Schema.Validators;     use Schema.Validators;
with Schema.Dom_Readers;    use Schema.Dom_Readers;
with Input_Sources.File;    use Input_Sources.File;
with Ada.Exceptions;        use Ada.Exceptions;
with GNAT.IO;               use GNAT.IO;
with Sax.Readers;           use Sax.Readers;
with GNAT.Command_Line;     use GNAT.Command_Line;
with GNAT.OS_Lib;           use GNAT.OS_Lib;
with DOM.Core;              use DOM.Core;
with DOM.Core.Nodes;        use DOM.Core.Nodes;

procedure TestDomSchema is
   Read      : File_Input;
   My_Reader : Schema.Dom_Readers.Tree_Reader;
   Schema    : Schema_Reader;
   Grammar   : XML_Grammar := No_Grammar;
   Xsd_File  : String_Access := null;
   Xml_File  : String_Access := null;
   Doc       : Document;
   Silent    : Boolean := False;

begin
   loop
      case Getopt ("xsd: debug silent") is
         when 'x' =>
            Free (Xsd_File);
            Xsd_File := new String'(Parameter);
         when 'd' =>
            Standard.Schema.Set_Debug_Output (True);
         when 's' =>
            Silent := True;
         when others =>
            exit;
      end case;
   end loop;

   Xml_File := new String'(Get_Argument);

   if Xsd_File /= null then
      Open (Xsd_File.all, Read);
      Parse (Schema, Read);
      Close (Read);
      Grammar := Get_Grammar (Schema);
   end if;

   if Xml_File.all /= "" then
      Set_Grammar (My_Reader, Grammar);
      Set_Feature (My_Reader, Schema_Validation_Feature, True);
      Open (Xml_File.all, Read);
      Parse (My_Reader, Read);
      Doc := Get_Tree (My_Reader);

      if not Silent then
         Print (Doc);
      end if;

      Close (Read);
   end if;

exception
   when E : XML_Validation_Error | XML_Fatal_Error =>
      Put_Line (Exception_Message (E));
      Close (Read);
end TestDomSchema;
