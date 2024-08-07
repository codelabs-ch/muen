--
--  Copyright (C) 2023 secunet Security Networks AG
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
with Ada.Strings.Unbounded;

with DOM.Core.Elements;

with Mutools.Utils;
with Mutools.Xmldebuglog;

with Muxml.Utils;

with Mulog;

package body Mutools.Vres_Alloc
is
   procedure Allocate_And_Set_Single_Resource
     (Av_Ival       : in out Intervals.Interval_List_Type;
      Node          :        DOM.Core.Node;
      Resource_Kind :        Resource_Kind_Type;
      Size          :        String := "")
   is
      Size_U64    : Interfaces.Unsigned_64;
      New_Address : Interfaces.Unsigned_64;
   begin
      case Resource_Kind is
         when Virtual_Addresses =>
            if Size /= "" then
               Size_U64 := Interfaces.Unsigned_64'Value (Size);
            else
               Size_U64 := Interfaces.Unsigned_64'Value
                 (DOM.Core.Elements.Get_Attribute (Elem => Node,
                                                   Name => "size"));
            end if;
            if not Mutools.Vres_Alloc.Is_Aligned (Size => Size_U64) then
               Mulog.Log (Msg => "Error: Size of node is not "
                        & "a multiple of 16#1000#. XPath: '"
                        & Mutools.Xmldebuglog.Get_Xpath (Node => Node)
                        & "', Size: '"
                        & Mutools.Utils.To_Hex (Number => Size_U64)
                        & "'");
               raise Validation_Error with "Virtual resource not aligned";
            end if;
         when Reader_Vectors | Writer_Events =>
            Size_U64 := 1;
      end case;

      New_Address := Intervals.Reserve_Interval (List => Av_Ival,
                                                 Size => Size_U64);
      Set_Virtual_Resource
        (Node          => Node,
         Resource_Kind => Resource_Kind,
         Value         => New_Address);
   end Allocate_And_Set_Single_Resource;

   -------------------------------------------------------------------------

   function Get_Resource_Size
     (Elem          : DOM.Core.Node;
      Resource_Kind : Resource_Kind_Type)
     return Interfaces.Unsigned_64
   is
      Tag_Name : constant String
        := DOM.Core.Elements.Get_Tag_Name (Elem => Elem);

      --  Check if Size is empty or not and convert to Unsigned_64
      function Check_Size (Size : String) return Interfaces.Unsigned_64;

      ----------------------------------------------------------------------

      function Check_Size (Size : String) return Interfaces.Unsigned_64
      is
      begin
         if Resource_Kind = Virtual_Addresses then
            if Size = "" then
               raise Validation_Error with
                 "Could not find 'size'/'elementSize' attribute in node at '"
                 & Xmldebuglog.Get_Xpath (Node => Elem)
                 & "'";
            else
               return Interfaces.Unsigned_64'Value (Size);
            end if;
         else
            return 1;
         end if;
      end Check_Size;

   begin
      if Tag_Name = "array" then
         return Check_Size (Size => DOM.Core.Elements.Get_Attribute
              (Elem => Elem,
               Name => "elementSize"));
      else
         return Check_Size (Size => DOM.Core.Elements.Get_Attribute
              (Elem => Elem,
               Name => "size"));
      end if;
   end Get_Resource_Size;

   -------------------------------------------------------------------------

   function Get_Resource_Value
     (Elem          : DOM.Core.Node;
      Resource_Kind : Resource_Kind_Type)
     return String
   is
      Tag_Name : constant String
        := DOM.Core.Elements.Get_Tag_Name (Elem => Elem);
   begin
      case Resource_Kind is
         when Virtual_Addresses =>
            if Tag_Name = "memory"
              or Tag_Name = "reader"
              or Tag_Name = "writer"
            then
               return DOM.Core.Elements.Get_Attribute
                 (Elem => Elem,
                  Name => "virtualAddress");
            elsif Tag_Name = "array" then
               return DOM.Core.Elements.Get_Attribute
                 (Elem => Elem,
                  Name => "virtualAddressBase");
            else
               raise Validation_Error with
                 "Found unexpected node tag '"
                 & Tag_Name
                 & "' when reading attribute value for virtual address";
            end if;
         when Reader_Vectors =>
            if Tag_Name = "reader" then
               return DOM.Core.Elements.Get_Attribute
                 (Elem => Elem,
                  Name => "vector");
            elsif Tag_Name = "array" then
               return DOM.Core.Elements.Get_Attribute
                 (Elem => Elem,
                  Name => "vectorBase");
            elsif Tag_Name = "event" then
               declare
                  Child : constant DOM.Core.Node
                    := Muxml.Utils.Get_Unique_Element_Child
                    (Parent     => Elem,
                     Child_Name => "inject_interrupt");
               begin
                  if Child /= null then
                     return DOM.Core.Elements.Get_Attribute
                       (Elem => Child,
                        Name => "vector");
                  else
                     return "";
                  end if;
               end;
            else
               raise Validation_Error with
                 "Found unexpected node tag '"
                 & Tag_Name
                 & "' when reading attribute value for reader event";
            end if;
         when Writer_Events =>
            if Tag_Name = "writer" then
               return DOM.Core.Elements.Get_Attribute
                 (Elem => Elem,
                  Name => "event");
            elsif Tag_Name = "array" then
               return DOM.Core.Elements.Get_Attribute
                 (Elem => Elem,
                  Name => "eventBase");
            elsif Tag_Name = "event" then
               return DOM.Core.Elements.Get_Attribute
                 (Elem => Elem,
                  Name => "id");
            else
               raise Validation_Error with
                 "Found unexpected node tag '"
                 & Tag_Name
                 & "' when reading attribute value for writer event";
            end if;
      end case;
   end Get_Resource_Value;

   -------------------------------------------------------------------------

   function Get_Target_String
     (Target_List : String_Vector.Vector;
      Prefix      : String)
     return String
   is
      use Ada.Strings.Unbounded;

      function U (Input : String) return Unbounded_String
        renames To_Unbounded_String;

      Output : Unbounded_String;
   begin
      for Target of Target_List loop
         if Output /= U ("") then
            Output := Output & U (" | ");
         end if;
         Output := Output
           & U (Prefix)
           & U ("/")
           & U (Target);
      end loop;

      --  If Target_List is empty, the resulting Xpath must not match any
      --  nodes.
      if Output = U ("") then
         Output := U ("*[false()]");
      end if;

      return To_String (Output);
   end Get_Target_String;

   -------------------------------------------------------------------------

   function Is_Aligned
     (Address : Interfaces.Unsigned_64 := 16#1000#;
      Size    : Interfaces.Unsigned_64 := 16#1000#)
     return Boolean
   is
      use type Interfaces.Unsigned_64;
   begin
      return Address mod 16#1000# = 0
        and Size mod 16#1000# = 0
        and Size > 0;
   end Is_Aligned;

   -------------------------------------------------------------------------

   procedure Set_Virtual_Resource
     (Node          : DOM.Core.Node;
      Resource_Kind : Resource_Kind_Type;
      Value         : Interfaces.Unsigned_64)
   is
   begin
      case Resource_Kind is
         when Virtual_Addresses =>
            DOM.Core.Elements.Set_Attribute
              (Elem  => Node,
               Name  => "virtualAddress",
               Value => Mutools.Utils.To_Hex (Number => Value));
         when Reader_Vectors =>
            DOM.Core.Elements.Set_Attribute
              (Elem  => Node,
               Name  => "vector",
               Value => Mutools.Utils.To_Decimal (Value));
         when Writer_Events =>
            DOM.Core.Elements.Set_Attribute
              (Elem  => Node,
               Name  => "event",
               Value => Mutools.Utils.To_Decimal (Value));
      end case;
   end Set_Virtual_Resource;

end Mutools.Vres_Alloc;
