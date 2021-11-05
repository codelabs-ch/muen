--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Strings;
with DOM.Core.Documents;
with DOM.Core.Elements;

with Muxml.Utils;

package body Mutools.System_Config
is

   use type DOM.Core.Node;

   function U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   --  Returns True if a config option of given type with specified name
   --  exists.
   function Has_Option
     (Data     : Muxml.XML_Data_Type;
      Opt_Type : String;
      Name     : String)
      return Boolean;

   -------------------------------------------------------------------------

   function Get_Raw_Value
     (Data : Muxml.XML_Data_Type;
      Name : String)
      return String
   is
      Val_Str : constant String
        := Muxml.Utils.Get_Attribute
          (Doc   => Data.Doc,
           XPath => "/*/config/*[@name='" & Name & "']",
           Name  => "value");
   begin
      if Val_Str'Length = 0 then
         raise Not_Found with "No config option '" & Name & "' found";
      end if;
      return Val_Str;
   end Get_Raw_Value;

   -------------------------------------------------------------------------

   function Get_Value
     (Data : Muxml.XML_Data_Type;
      Name : String)
      return Boolean
   is
      Val_Str : constant String
        := Muxml.Utils.Get_Attribute
          (Doc   => Data.Doc,
           XPath => "/*/config/boolean[@name='" & Name & "']",
           Name  => "value");
   begin
      if Val_Str'Length = 0 then
         raise Not_Found with "No boolean config option '" & Name & "' found";
      end if;

      return Boolean'Value (Val_Str);
   end Get_Value;

   -------------------------------------------------------------------------

   function Get_Value
     (Data : Muxml.XML_Data_Type;
      Name : String)
      return Integer
   is
      Val_Str : constant String
        := Muxml.Utils.Get_Attribute
          (Doc   => Data.Doc,
           XPath => "/*/config/integer[@name='" & Name & "']",
           Name  => "value");
   begin
      if Val_Str'Length = 0 then
         raise Not_Found with "No integer config option '" & Name & "' found";
      end if;

      return Integer'Value (Val_Str);
   end Get_Value;

   -------------------------------------------------------------------------

   function Get_Value
     (Data : Muxml.XML_Data_Type;
      Name : String)
      return String
   is
      Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/*/config/string[@name='" & Name & "']");
   begin
      if Node = null then
         raise Not_Found with "No string config option '" & Name & "' found";
      end if;

      return DOM.Core.Elements.Get_Attribute
        (Elem => Node,
         Name => "value");
   end Get_Value;

   -------------------------------------------------------------------------

   function Has_Boolean
     (Data : Muxml.XML_Data_Type;
      Name : String)
      return Boolean
   is
   begin
      return Has_Option (Data     => Data,
                         Opt_Type => "boolean",
                         Name     => Name);
   end Has_Boolean;

   -------------------------------------------------------------------------

   function Has_Integer
     (Data : Muxml.XML_Data_Type;
      Name : String)
      return Boolean
   is
   begin
      return Has_Option (Data     => Data,
                         Opt_Type => "integer",
                         Name     => Name);
   end Has_Integer;

   -------------------------------------------------------------------------

   function Has_Option
     (Data     : Muxml.XML_Data_Type;
      Opt_Type : String;
      Name     : String)
      return Boolean
   is
   begin
      return null /= Muxml.Utils.Get_Element
        (Doc   => Data.Doc,
         XPath => "/*/config/" & Opt_Type & "[@name='" & Name & "']");
   end Has_Option;

   -------------------------------------------------------------------------

   function Has_String
     (Data : Muxml.XML_Data_Type;
      Name : String)
      return Boolean
   is
   begin
      return Has_Option (Data     => Data,
                         Opt_Type => "string",
                         Name     => Name);
   end Has_String;

   -------------------------------------------------------------------------

   function Has_Value
     (Data : Muxml.XML_Data_Type;
      Name : String)
      return Boolean
   is
   begin
      return Has_Option (Data     => Data,
                         Opt_Type => "*",
                         Name     => Name);
   end Has_Value;

   -------------------------------------------------------------------------

   procedure Set_Value
     (Data  : Muxml.XML_Data_Type;
      Name  : String;
      Value : Boolean)
   is
      Cfg_Node : DOM.Core.Node := Muxml.Utils.Get_Element
        (Doc   => Data.Doc,
         XPath => "/*/config/boolean[@name='" & Name & "']");
   begin
      if Cfg_Node = null then
         Cfg_Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "boolean");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Cfg_Node,
            Name  => "name",
            Value => Name);
         Muxml.Utils.Insert_Before
           (Parent    => Muxml.Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => "/*/config"),
            New_Child => Cfg_Node,
            Ref_Names => (1 => U ("integer"),
                          2 => U ("string")));
      end if;

      DOM.Core.Elements.Set_Attribute
        (Elem  => Cfg_Node,
         Name  => "value",
         Value => Ada.Characters.Handling.To_Lower (Item => Value'Img));
   end Set_Value;

   -------------------------------------------------------------------------

   procedure Set_Value
     (Data  : Muxml.XML_Data_Type;
      Name  : String;
      Value : Integer)
   is
      Cfg_Node : DOM.Core.Node := Muxml.Utils.Get_Element
        (Doc   => Data.Doc,
         XPath => "/*/config/integer[@name='" & Name & "']");
   begin
      if Cfg_Node = null then
         Cfg_Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "integer");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Cfg_Node,
            Name  => "name",
            Value => Name);
         Muxml.Utils.Insert_Before
           (Parent    => Muxml.Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => "/*/config"),
            New_Child => Cfg_Node,
            Ref_Names => (1 => U ("string")));
      end if;

      DOM.Core.Elements.Set_Attribute
        (Elem  => Cfg_Node,
         Name  => "value",
         Value => Ada.Strings.Fixed.Trim (Integer'Image (Value), Ada.Strings.Both));
   end Set_Value;

   -------------------------------------------------------------------------

   procedure Set_Value
     (Data  : Muxml.XML_Data_Type;
      Name  : String;
      Value : String)
   is
      Cfg_Node : DOM.Core.Node := Muxml.Utils.Get_Element
        (Doc   => Data.Doc,
         XPath => "/*/config/string[@name='" & Name & "']");
   begin
      if Cfg_Node = null then
         Cfg_Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "string");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Cfg_Node,
            Name  => "name",
            Value => Name);
         Muxml.Utils.Insert_Before
           (Parent    => Muxml.Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => "/*/config"),
            New_Child => Cfg_Node,
            Ref_Names => Muxml.Utils.No_Tags);
      end if;

      DOM.Core.Elements.Set_Attribute
        (Elem  => Cfg_Node,
         Name  => "value",
         Value => Value);
   end Set_Value;

end Mutools.System_Config;
