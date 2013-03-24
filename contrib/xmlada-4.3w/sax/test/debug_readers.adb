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

with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;
with Sax.Readers;    use Sax.Readers;
with Sax.Exceptions; use Sax.Exceptions;
with Sax.Locators;   use Sax.Locators;
with Sax.Attributes; use Sax.Attributes;
with Sax.Models;     use Sax.Models;
with Unicode.CES;    use Unicode.CES;
with Unicode;        use Unicode;
with Sax.Encodings;  use Sax.Encodings;
with Input_Sources.Strings; use Input_Sources.Strings;

package body Debug_Readers is

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (String, String_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (String_List, String_List_Access);

   procedure Free (List : in out String_List_Access);
   --  Free the array and all the stored strings

   function Location
     (Handler : Debug_Reader'Class; Skip : Natural := 0) return String;
   --  Return the current location.
   --  Skip indicates the number of frames to skip in the saved locations
   --  stack

   ----------
   -- Free --
   ----------

   procedure Free (List : in out String_List_Access) is
   begin
      if List /= null then
         for L in List'Range loop
            Unchecked_Free (List (L));
         end loop;
         Unchecked_Free (List);
      end if;
   end Free;

   --------------
   -- Location --
   --------------

   function Location
     (Handler : Debug_Reader'Class; Skip : Natural := 0) return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for C in Handler.Saved_Locs'First + Skip .. Handler.Saved_Locs'Last loop
         Result := Result
           & "[entity expansion at " & Handler.Saved_Locs (C).all;
      end loop;

      for C in Handler.Saved_Locs'First + Skip .. Handler.Saved_Locs'Last loop
         Result := Result & ']';
      end loop;

      if Handler.Color then
         return ASCII.ESC & "[33m"
           & To_String (Handler.Locator) & To_String (Result)
           & ASCII.ESC & "[39m";
      else
         return To_String (Handler.Locator) & To_String (Result);
      end if;
   end Location;

   ----------------
   -- Set_Silent --
   ----------------

   procedure Set_Silent
     (Handler : in out Debug_Reader; Silent : Boolean) is
   begin
      Handler.Silent := Silent;
   end Set_Silent;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color
     (Handler : in out Debug_Reader; Color : Boolean) is
   begin
      Handler.Color := Color;
   end Set_Color;

   -------------
   -- Warning --
   -------------

   procedure Warning
     (Handler : in out Debug_Reader;
      Except : Sax.Exceptions.Sax_Parse_Exception'Class)
   is
      pragma Warnings (Off, Handler);
   begin
      Put_Line ("Sax.Warning ("
                & Get_Message (Except) & ", at "
                & To_String (Get_Location (Except)) & ')');
   end Warning;

   -----------
   -- Error --
   -----------

   procedure Error
     (Handler : in out Debug_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class)
   is
      pragma Warnings (Off, Handler);
   begin
      Put_Line ("Sax.Error ("
                & Get_Message (Except) & ", at "
                & To_String (Get_Location (Except)) & ')');
   end Error;

   -----------------
   -- Fatal_Error --
   -----------------

   procedure Fatal_Error
     (Handler : in out Debug_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class) is
   begin
      Put_Line ("Sax.Fatal_Error (" & Get_Message (Except) & ") at "
                & Location (Handler));
      Free (Handler.Saved_Locs);
      Handler.Saved_Locs := new String_List'(1 .. 0 => null);
      Fatal_Error (Reader (Handler), Except);
   end Fatal_Error;

   --------------------------
   -- Set_Document_Locator --
   --------------------------

   procedure Set_Document_Locator
     (Handler : in out Debug_Reader;
      Loc     : in out Sax.Locators.Locator) is
   begin
      if not Handler.Silent then
         Put_Line ("Sax.Set_Document_Locator ()");
      end if;
      Handler.Locator := Loc;
   end Set_Document_Locator;

   --------------------
   -- Start_Document --
   --------------------

   procedure Start_Document (Handler : in out Debug_Reader) is
   begin
      Handler.Saved_Locs := new String_List'(1 .. 0 => null);

      if not Handler.Silent then
         Put_Line ("Sax.Start_Document () at " & Location (Handler));
      end if;
   end Start_Document;

   ------------------
   -- End_Document --
   ------------------

   procedure End_Document (Handler : in out Debug_Reader) is
   begin
      if not Handler.Silent then
         Put_Line ("Sax.End_Document () at " & Location  (Handler));

         Free (Handler.Saved_Locs);
      end if;
   end End_Document;

   --------------------------
   -- Start_Prefix_Mapping --
   --------------------------

   procedure Start_Prefix_Mapping
     (Handler : in out Debug_Reader;
      Prefix  : Unicode.CES.Byte_Sequence;
      URI     : Unicode.CES.Byte_Sequence)
   is
      pragma Unmodified (Handler);
   begin
      if not Handler.Silent then
         Put_Line ("Sax.Start_Prefix_Mapping (" & Prefix & ", " & URI
                   & ") at " & Location (Handler));
      end if;
   end Start_Prefix_Mapping;

   ------------------------
   -- End_Prefix_Mapping --
   ------------------------

   procedure End_Prefix_Mapping
     (Handler : in out Debug_Reader; Prefix : Unicode.CES.Byte_Sequence)
   is
      pragma Unmodified (Handler);
   begin
      if not Handler.Silent then
         Put_Line ("Sax.End_Prefix_Mapping (" & Prefix & ") at "
                   & Location (Handler));
      end if;
   end End_Prefix_Mapping;

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element
     (Handler       : in out Debug_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class)
   is
      pragma Unmodified (Handler);
   begin
      if not Handler.Silent then
         Put ("Sax.Start_Element ("
              & Namespace_URI & ", " & Local_Name & ", " & Qname);
         for J in 0 .. Get_Length (Atts) - 1 loop
            Put (", " & Get_Qname (Atts, J) & "='"
                 & Get_Value (Atts, J) & ''');
         end loop;
         Put_Line (") at " & Location (Handler));
      end if;
   end Start_Element;

   -----------------
   -- End_Element --
   -----------------

   procedure End_Element
     (Handler : in out Debug_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "")
   is
      pragma Unmodified (Handler);
   begin
      if not Handler.Silent then
         Put_Line ("Sax.End_Element (" & Namespace_URI & ", "
                   & Local_Name & ", " & Qname & ") at " & Location (Handler));
      end if;
   end End_Element;

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Handler : in out Debug_Reader; Ch : Unicode.CES.Byte_Sequence)
   is
      pragma Unmodified (Handler);
   begin
      if not Handler.Silent then
         Put_Line ("Sax.Characters (" & Ch & ','
                   & Integer'Image (Ch'Length) & ") at " & Location (Handler));
      end if;
   end Characters;

   --------------------------
   -- Ignorable_Whitespace --
   --------------------------

   procedure Ignorable_Whitespace
     (Handler : in out Debug_Reader; Ch : Unicode.CES.Byte_Sequence)
   is
      pragma Unmodified (Handler);
      Index : Natural := Ch'First;
      C     : Unicode_Char;
   begin
      if not Handler.Silent then
         Put ("Sax.Ignorable_Whitespace (");
         while Index <= Ch'Last loop
            Encoding.Read (Ch, Index, C);
            Put (Unicode_Char'Image (C));
         end loop;
         Put_Line (','
                   & Integer'Image (Ch'Length) & ") at "
                   & Location (Handler));
      end if;
   end Ignorable_Whitespace;

   ----------------------------
   -- Processing_Instruction --
   ----------------------------

   procedure Processing_Instruction
     (Handler : in out Debug_Reader;
      Target  : Unicode.CES.Byte_Sequence;
      Data    : Unicode.CES.Byte_Sequence)
   is
      pragma Unmodified (Handler);
   begin
      if not Handler.Silent then
         Put_Line ("Sax.Processing instruction (" & Target & ", '" & Data
                   & "') at " & Location (Handler));
      end if;
   end Processing_Instruction;

   --------------------
   -- Skipped_Entity --
   --------------------

   procedure Skipped_Entity
     (Handler : in out Debug_Reader; Name : Unicode.CES.Byte_Sequence)
   is
      pragma Unmodified (Handler);
   begin
      if not Handler.Silent then
         Put_Line ("Sax.Skipped_Entity (" & Name & ") at "
                   & Location (Handler));
      end if;
   end Skipped_Entity;

   -------------
   -- Comment --
   -------------

   procedure Comment
     (Handler : in out Debug_Reader; Ch : Unicode.CES.Byte_Sequence)
   is
      pragma Unmodified (Handler);
   begin
      if not Handler.Silent then
         Put_Line ("Sax.Comment (" & Ch & ") at "
                   & Location (Handler));
      end if;
   end Comment;

   -----------------
   -- Start_Cdata --
   -----------------

   procedure Start_Cdata (Handler : in out Debug_Reader) is
      pragma Unmodified (Handler);
   begin
      if not Handler.Silent then
         Put_Line ("Sax.Start_Cdata () at " & Location (Handler));
      end if;
   end Start_Cdata;

   ---------------
   -- End_Cdata --
   ---------------

   procedure End_Cdata (Handler : in out Debug_Reader) is
      pragma Unmodified (Handler);
   begin
      if not Handler.Silent then
         Put_Line ("Sax.End_Cdata () at " & Location (Handler));
      end if;
   end End_Cdata;

   ------------------
   -- Start_Entity --
   ------------------

   procedure Start_Entity
     (Handler : in out Debug_Reader; Name : Unicode.CES.Byte_Sequence)
   is
      Tmp : String_List_Access;
   begin
      if not Handler.Silent then
         Put_Line ("Sax.Start_Entity (" & Name & ") at "
                   & Location (Handler));

         Tmp := new String_List'
           (new String'(To_String (Handler.Locator))
            & Handler.Saved_Locs.all);
         Unchecked_Free (Handler.Saved_Locs);
         Handler.Saved_Locs := Tmp;
      end if;
   end Start_Entity;

   ----------------
   -- End_Entity --
   ----------------

   procedure End_Entity
     (Handler : in out Debug_Reader; Name : Unicode.CES.Byte_Sequence)
   is
      Tmp : String_List_Access;
   begin
      if not Handler.Silent then
         Tmp := new String_List (1 .. Handler.Saved_Locs'Length - 1);
         Tmp.all := Handler.Saved_Locs
           (Handler.Saved_Locs'First + 1 .. Handler.Saved_Locs'Last);
         Unchecked_Free (Handler.Saved_Locs);
         Handler.Saved_Locs := Tmp;

         Put_Line ("Sax.End_Entity (" & Name & ") at " & Location (Handler));
      end if;
   end End_Entity;

   ---------------
   -- Start_DTD --
   ---------------

   procedure Start_DTD
     (Handler   : in out Debug_Reader;
      Name      : Unicode.CES.Byte_Sequence;
      Public_Id : Unicode.CES.Byte_Sequence := "";
      System_Id : Unicode.CES.Byte_Sequence := "")
   is
      pragma Unmodified (Handler);
   begin
      if not Handler.Silent then
         Put_Line ("Sax.Start_DTD (" & Name
                   & ", " & Public_Id
                   & ", " & System_Id & ") at "
                   & Location (Handler));
      end if;
   end Start_DTD;

   -------------
   -- End_DTD --
   -------------

   procedure End_DTD (Handler : in out Debug_Reader) is
      pragma Unmodified (Handler);
   begin
      if not Handler.Silent then
         Put_Line ("Sax.End_DTD () at " & Location (Handler));
      end if;
   end End_DTD;

   --------------------------
   -- Internal_Entity_Decl --
   --------------------------

   procedure Internal_Entity_Decl
     (Handler : in out Debug_Reader;
      Name    : Unicode.CES.Byte_Sequence;
      Value   : Unicode.CES.Byte_Sequence)
   is
      pragma Unmodified (Handler);
   begin
      if not Handler.Silent then
         Put_Line ("Sax.Internal_Entity_Decl ("
                   & Name & ", " & Value
                   & ") at " & Location (Handler));
      end if;
   end Internal_Entity_Decl;

   --------------------------
   -- External_Entity_Decl --
   --------------------------

   procedure External_Entity_Decl
     (Handler   : in out Debug_Reader;
      Name      : Unicode.CES.Byte_Sequence;
      Public_Id : Unicode.CES.Byte_Sequence;
      System_Id : Unicode.CES.Byte_Sequence)
   is
      pragma Unmodified (Handler);
   begin
      if not Handler.Silent then
         Put_Line ("Sax.External_Entity_Decl ("
                   & Name & ", " & Public_Id
                   & ", " & System_Id
                   & ") at " & Location (Handler));
      end if;
   end External_Entity_Decl;

   --------------------------
   -- Unparsed_Entity_Decl --
   --------------------------

   procedure Unparsed_Entity_Decl
     (Handler       : in out Debug_Reader;
      Name          : Unicode.CES.Byte_Sequence;
      System_Id     : Unicode.CES.Byte_Sequence;
      Notation_Name : Unicode.CES.Byte_Sequence)
   is
      pragma Unmodified (Handler);
   begin
      if not Handler.Silent then
         Put_Line ("Sax.Unparsed_Entity_Decl ("
                   & Name & ", " & System_Id
                   & ", " & Notation_Name
                   & ") at " & Location (Handler));
      end if;
   end Unparsed_Entity_Decl;

   ------------------
   -- Element_Decl --
   ------------------

   procedure Element_Decl
     (Handler : in out Debug_Reader;
      Name    : Unicode.CES.Byte_Sequence;
      Model   : Content_Model)
   is
      pragma Unmodified (Handler);
   begin
      if not Handler.Silent then
         Put_Line ("Sax.Element_Decl ("
                   & Name & ", " & To_String (Model)
                   & ") at " & Location (Handler));
      end if;
   end Element_Decl;

   -------------------
   -- Notation_Decl --
   -------------------

   procedure Notation_Decl
     (Handler   : in out Debug_Reader;
      Name      : Unicode.CES.Byte_Sequence;
      Public_Id : Unicode.CES.Byte_Sequence;
      System_Id : Unicode.CES.Byte_Sequence)
   is
      pragma Unmodified (Handler);
   begin
      if not Handler.Silent then
         Put_Line ("Sax.Notation_Decl ("
                   & Name & ", " & Public_Id
                   & ", " & System_Id & ") at "
                   & Location (Handler));
      end if;
   end Notation_Decl;

   --------------------
   -- Attribute_Decl --
   --------------------

   procedure Attribute_Decl
     (Handler       : in out Debug_Reader;
      Ename         : Unicode.CES.Byte_Sequence;
      Aname         : Unicode.CES.Byte_Sequence;
      Typ           : Attribute_Type;
      Content       : Content_Model;
      Value_Default : Sax.Attributes.Default_Declaration;
      Value         : Unicode.CES.Byte_Sequence)
   is
      pragma Unmodified (Handler);
   begin
      if not Handler.Silent then
         if Content /= Unknown_Model then
            Put_Line ("Sax.Attribute_Decl ("
                      & Ename & ", " & Aname
                      & ", " & Attribute_Type'Image (Typ) & ", "
                      & To_String (Content)
                      & ", " & Default_Declaration'Image (Value_Default)
                      & ", " & Value & ") at " & Location (Handler));
         else
            Put_Line ("Sax.Attribute_Decl ("
                      & Ename & ", " & Aname
                      & ", " & Attribute_Type'Image (Typ) & ", "
                      & Default_Declaration'Image (Value_Default)
                      & ", " & Value & ") at " & Location (Handler));
         end if;
      end if;
   end Attribute_Decl;

   --------------------
   -- Resolve_Entity --
   --------------------

   function Resolve_Entity
     (Handler   : Debug_Reader;
      Public_Id : Unicode.CES.Byte_Sequence;
      System_Id : Unicode.CES.Byte_Sequence)
      return Input_Sources.Input_Source_Access is
   begin
      if not Handler.Silent then
         Put_Line ("Sax.Resolve_Entity ("
                   & Public_Id & ", " & System_Id
                   & ") at " & Location (Handler, Skip => 1));
      end if;

      if System_Id = "t3.xml" then
         declare
            S : constant String_Input_Access := new String_Input;
         begin
            Open (new Byte_Sequence'("Virtual_t3.xml_file"),
                  Sax.Encodings.Encoding, S.all);
            return Input_Sources.Input_Source_Access (S);
         end;
      end if;

      return null;
   end Resolve_Entity;

end Debug_Readers;
