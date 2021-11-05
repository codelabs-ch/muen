--
--  Copyright (C) 2022 secunet Security Networks AG
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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;

package Mutools.Amend.Ordering
is
   type Insert_Index is range -2 .. Integer'Last;
   package String_Vector is new Ada.Containers.Indefinite_Vectors
      (Index_Type   => Natural,
       Element_Type => String);

   function "=" (L, R : String_Vector.Vector) return Boolean;

   use all type String_Vector.Vector;

   -- amend can "query" the ordering information unsing this function
   -- Returns an index before which New_Child can be inserted such that
   --     the schema is satisfied (e.g. "0" means "before the first child")
   -- Anchestors is a list always containing the name of the parent P of
   --    New_Child. More names of anchestors may follow in order of distance
   --    (starting with the parent of P)
   -- Siblings: names of children of P, in order of appearence in P.
   --     It is legal to substitute consecutive subequences of equal names to
   --     one name. E.g.:
   --        ([...]^1, "channel", "channel", "channel", [...]^2)
   --     -> ([...]^1, "channel", [...]^2)
   -- "After the last sibling" is expressed by index Length(Siblings)
   -- -1 is returned if a correct index cannot be determined (missing info)
   -- -2 is returned if no legal insertion exists (error)
   function Get_Insert_Index
      (Anchestors : String_Vector.Vector;
       New_Child  : String;
       Siblings   : String_Vector.Vector)
      return Insert_Index;

   -- raised if the schema uses a construction which is not supported
   Not_Implemented : exception;

   -- raised if the schema does is not complient
   -- with https://www.w3.org/TR/xmlschema-0/
   Validation_Error : exception;

private
   -- raised only internally to signal that a certain query cannot be decided
   Insufficient_Information : exception;

   type Vector_Tuple is
   record
      Node_Names : String_Vector.Vector;
      Type_Names : String_Vector.Vector;
   end record;

   function "=" (L, R : Vector_Tuple) return Boolean;

   package String_To_VectorTuple is new Ada.Containers.Indefinite_Ordered_Maps
      (Key_Type     => String,
       Element_Type => Vector_Tuple);

   package String_To_StringVector is new  Ada.Containers.Indefinite_Ordered_Maps
      (Key_Type     => String,
       Element_Type => String_Vector.Vector);

   type Order_Information is
   record
      Type_To_Children : String_To_VectorTuple.Map;
      Tag_To_Type      : String_To_StringVector.Map;
   end record;

   -- read the schema definition of the policy_src format and return a
   -- (1) a mapping of the form
   --     "typename -> (tag1, tag2, ...), (type tag1, type tag2, ...)"  and
   -- (2) a mapping of the form
   --     "tagname  -> (unique typename of that tag)"
   -- Schema_XML_Data is a parameter for testing purposes only
   procedure Init_Order_Information (Schema_XML_Data : String);

   -- return a string representation of Order_Information
   function To_String (OI : Order_Information) return String;

   -- return a string representation of a Vector_Tuple as tuples if they
   -- have the same length
   function To_String (VT : Vector_Tuple) return String;

   -- return a string representation of a String_Vector
   function To_String (SV : String_Vector.Vector) return String;

end Mutools.Amend.Ordering;
