------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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

--  This package implements the content models as described in the DTDs.
--  They are not strictly part of the SAX 2.0 standard, however they are
--  used to simply the handling in users' applications.

with Sax.Symbols;
with Unicode.CES;

package Sax.Models is

   type Content_Spec is
     (Character_Data,   --  Characters, but no child node  (#PCDATA)
      Element_Ref,      --  A specific child
      Any_Of,           --  child is one of many choices
      Sequence,         --  a sequence of elements (order is imposed)
      Repeat,           --  A repeated element
      Empty,            --  Element must be empty  (EMPTY)
      Anything          --  Content is not described, and can be anything (ANY)
     );

   type Element_Model;
   type Element_Model_Ptr is access Element_Model;
   type Element_Model_Array is array (Natural range <>) of Element_Model_Ptr;
   type Element_Model_Array_Ptr is access Element_Model_Array;

   type Element_Model (Content : Content_Spec) is record
      case Content is
         when Character_Data | Empty | Anything => null;

         when Element_Ref =>
            Name : Sax.Symbols.Symbol; --  Name of the element

         when Any_Of | Sequence =>
            List : Element_Model_Array_Ptr; --  all the possible choices

         when Repeat =>
            Min : Natural;
            Max : Positive;
            Elem : Element_Model_Ptr;
      end case;
   end record;
   --  Type used to describe the model used for an element, as described in
   --  the DTD (see 3.2.* in XML specifications). For instance, the following
   --  model "(#PCDATA|emph)*" is translated to:
   --     (Content => Repeat,
   --      Min     => 0,
   --      Max     => Positive'Last,
   --      Elem    => (Content => Any_Of,
   --                  Choices => (0 => (Content => Character_Data),
   --                              1 => (Content => Element,
   --                                    Name    => "emp"))))

   type Content_Model is private;
   Unknown_Model : constant Content_Model;
   --  This is a reference counted type that represents a content model defined
   --  in an XML document's DTD.

   procedure Ref   (Model : Content_Model);
   procedure Unref (Model : in out Content_Model);
   --  Increment or decrement the reference counting for Model.
   --  When the reference counting reaches 0, the model is freed. You need to
   --  call these methods automatically if you intend to keep a copy of the
   --  model in your own structures.

   function Create_Model (Element : Element_Model_Ptr) return Content_Model;
   --  Create a content model based on a description.
   --  No copy of Element is done, and the returned content model becomes
   --  responsible for freeing that data structure when no longer needed

   procedure Free (Model : in out Element_Model_Ptr);
   --  Free the memory allocated for the model.

   function Get_Element_Model
     (Model : Content_Model) return Element_Model_Ptr;
   --  Return a description of the content model. Do not free the resulting
   --  pointer, since this points directly into the Content_Model structure

   function To_String
     (Model : Content_Model) return Unicode.CES.Byte_Sequence;
   --  Return the string to put in an XML file to describe Model
   --  Invalid_Content_Model is raised if Model can not be described in a
   --  DTD.

   function Is_Mixed (M : Element_Model_Ptr) return Boolean;
   --  Return True if M represents a Mixed content model (3.2.2 in XML
   --  specifications).

   Invalid_Content_Model : exception;
   --  Raised by To_String, when the model is invalid

private

   type Model_Item;
   type Model_List is access Model_Item;
   type Model_Item is record
      State : Element_Model_Ptr;
      Next  : Model_List;
   end record;

   type Natural_Access is access Natural;
   type Content_Model is record
      Ref_Count : Natural_Access;
      Model     : Element_Model_Ptr;
   end record;

   Unknown_Model : constant Content_Model := (null, null);

end Sax.Models;
