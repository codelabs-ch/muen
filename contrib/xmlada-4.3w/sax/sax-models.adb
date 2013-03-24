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

with Unicode;                   use Unicode;
with Unicode.CES;               use Unicode.CES;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Sax.Encodings;             use Sax.Encodings;

package body Sax.Models is

   function To_String
     (Model   : Element_Model) return Unicode.CES.Byte_Sequence;
   --  Same as To_String, applies to an Element_Model_Ptr

   ---------
   -- Ref --
   ---------

   procedure Ref (Model : Content_Model) is
   begin
      if Model.Ref_Count /= null then
         Model.Ref_Count.all := Model.Ref_Count.all + 1;
      end if;
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Model : in out Content_Model) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Natural, Natural_Access);
   begin
      if Model.Ref_Count /= null
        and then Model.Ref_Count.all > 0
      then
         Model.Ref_Count.all := Model.Ref_Count.all - 1;
         if Model.Ref_Count.all = 0 then
            Unchecked_Free (Model.Ref_Count);
            Free (Model.Model);
         end if;
      end if;
   end Unref;

   ------------------
   -- Create_Model --
   ------------------

   function Create_Model (Element : Element_Model_Ptr) return Content_Model is
   begin
      if Element = null then
         return Unknown_Model;
      else
         return (Model     => Element,
                 Ref_Count => new Natural'(1));
      end if;
   end Create_Model;

   -----------------------
   -- Get_Element_Model --
   -----------------------

   function Get_Element_Model
     (Model : Content_Model) return Element_Model_Ptr is
   begin
      return Model.Model;
   end Get_Element_Model;

   --------------
   -- Is_Mixed --
   --------------

   function Is_Mixed (M : Element_Model_Ptr) return Boolean is
   begin
      pragma Assert (M /= null);
      return M.Content = Any_Of
        and then M.List (M.List'First).Content = Character_Data;
   end Is_Mixed;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Model : Content_Model) return Unicode.CES.Byte_Sequence is
   begin
      if Model.Model = null then
         return "";
      else
         return To_String (Model.Model.all);
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Model   : Element_Model) return Unicode.CES.Byte_Sequence
   is
      Str : Unbounded_String;
   begin
      case Model.Content is
         when Character_Data =>
            return Pcdata_Sequence;

         when Empty =>
            return Empty_Sequence;

         when Anything =>
            return Any_Sequence;

         when Element_Ref =>
            return Sax.Symbols.Get (Model.Name).all;

         when Any_Of | Sequence =>
            for J in Model.List'Range loop
               if Model.List (J).Content = Character_Data then
                  if Model.Content = Sequence
                    or else J /= Model.List'First
                  then
                     raise Invalid_Content_Model;
                  end if;
               end if;

               if Model.List (J).Content = Anything
                 or else Model.List (J).Content = Empty
               then
                  raise Invalid_Content_Model;
               end if;

               Append (Str, To_String (Model.List (J).all));
               if J /= Model.List'Last then
                  if Model.Content = Any_Of then
                     Append (Str, Vertical_Line_Sequence);
                  else
                     Append (Str, Comma_Sequence);
                  end if;
               end if;
            end loop;
            return Opening_Parenthesis_Sequence
              & To_String (Str) & Closing_Parenthesis_Sequence;

         when Repeat =>
            if Model.Elem.Content = Anything
              or else Model.Elem.Content = Empty
            then
               raise Invalid_Content_Model;
            end if;

            if Model.Min = 0 and then Model.Max = Positive'Last then
               return To_String (Model.Elem.all) & Star_Sequence;
            elsif Model.Min = 0 and then Model.Max = 1 then
               return To_String (Model.Elem.all) & Question_Mark_Sequence;
            elsif Model.Min = 1 and then Model.Max = Positive'Last then
               return To_String (Model.Elem.all) & Plus_Sign_Sequence;
            else
               raise Invalid_Content_Model;
            end if;
      end case;
   end To_String;

   ----------
   -- Free --
   ----------

   procedure Free (Model : in out Element_Model_Ptr) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Element_Model_Array, Element_Model_Array_Ptr);
      procedure Internal is new Ada.Unchecked_Deallocation
        (Element_Model, Element_Model_Ptr);
   begin
      if Model /= null then
         case Model.Content is
            when Character_Data | Anything | Empty | Element_Ref => null;
            when Any_Of | Sequence =>
               for J in Model.List'Range loop
                  Free (Model.List (J));
               end loop;
               Free (Model.List);
            when Repeat =>
               Free (Model.Elem);
         end case;
         Internal (Model);
      end if;
   end Free;

end Sax.Models;
