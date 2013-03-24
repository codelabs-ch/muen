-----------------------------------------------------------------------
--  properties.hash -- Hash-based property implementation
--  Copyright (C) 2001, 2002, 2003, 2006, 2008, 2009, 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
package body Util.Properties.Hash is

   use PropertyMap;

   type Manager_Object_Access is access all Manager;

   procedure Free is
     new Ada.Unchecked_Deallocation (Manager, Manager_Object_Access);

   --  -----------------------
   --  Returns TRUE if the property exists.
   --  -----------------------
   function Exists (Self : in Manager; Name : in Value) return Boolean is
      Pos : constant PropertyMap.Cursor
        := PropertyMap.Find (Self.Content, Name);
   begin
      return PropertyMap.Has_Element (Pos);
   end Exists;

   --  -----------------------
   --  Returns the property value.  Raises an exception if not found.
   --  -----------------------
   function Get (Self : in Manager; Name : in Value) return Value is
      Pos : constant PropertyMap.Cursor := PropertyMap.Find (Self.Content, Name);
   begin
      if PropertyMap.Has_Element (Pos) then
         return PropertyMap.Element (Pos);
      else
         raise NO_PROPERTY with "No property: '" & To_String (Name) & "'";
      end if;
   end Get;

   procedure Insert (Self : in out Manager; Name : in Value;
                                            Item : in Value) is
      Pos : PropertyMap.Cursor;
      Inserted : Boolean;
   begin
      PropertyMap.Insert (Self.Content, Name, Item, Pos, Inserted);
   end Insert;

   --  -----------------------
   --  Set the value of the property.  The property is created if it
   --  does not exists.
   --  -----------------------
   procedure Set (Self : in out Manager; Name : in Value;
                                         Item : in Value) is
      Pos : PropertyMap.Cursor;
      Inserted : Boolean;
   begin
      PropertyMap.Insert (Self.Content, Name, Item, Pos, Inserted);
      if not Inserted then
         PropertyMap.Replace_Element (Self.Content, Pos, Item);
      end if;
   end Set;

   --  -----------------------
   --  Remove the property given its name.
   --  -----------------------
   procedure Remove (Self : in out Manager; Name : in Value) is
      Pos : PropertyMap.Cursor := PropertyMap.Find (Self.Content, Name);
   begin
      if PropertyMap.Has_Element (Pos) then
         PropertyMap.Delete (Self.Content, Pos);
      else
         raise NO_PROPERTY with "No property: '" & To_String (Name) & "'";
      end if;
   end Remove;

   --  -----------------------
   --  Iterate over the properties and execute the given procedure passing the
   --  property name and its value.
   --  -----------------------
   procedure Iterate (Self    : in Manager;
                      Process : access procedure (Name, Item : Value)) is
      procedure Do_Process (Name, Item : in Value);

      procedure Do_Process (Name, Item : in Value) is
      begin
         Process.all (Name, Item);
      end Do_Process;

      Iter : PropertyMap.Cursor := Self.Content.First;
   begin
      while PropertyMap.Has_Element (Iter) loop
         PropertyMap.Query_Element (Iter, Do_Process'Access);
         PropertyMap.Next (Iter);
      end loop;
   end Iterate;

   --  -----------------------
   --  Deep copy of properties stored in 'From' to 'To'.
   --  -----------------------
   function Create_Copy (Self : in Manager) return Interface_P.Manager_Access is
      Copy     : constant Manager_Access := new Manager;
      Iter     : PropertyMap.Cursor   := PropertyMap.First (Self.Content);
      New_Item : PropertyMap.Cursor;
      Inserted : Boolean;
   begin
      while PropertyMap.Has_Element (Iter) loop
         PropertyMap.Insert (Copy.Content, PropertyMap.Key (Iter),
                             PropertyMap.Element (Iter), New_Item, Inserted);
         Iter := PropertyMap.Next (Iter);
      end loop;
      return Copy.all'Access;
   end Create_Copy;

   procedure Delete (Self : in Manager;
                     Obj  : in out Interface_P.Manager_Access) is
      pragma Unreferenced (Self);
      Item : Manager_Object_Access := Manager (Obj.all)'Access;
   begin
      Free (Item);
   end Delete;

   function Equivalent_Keys (Left : Value; Right : Value)
                            return Boolean is
   begin
      return Left = Right;
   end Equivalent_Keys;

   function Get_Names (Self   : in Manager;
                       Prefix : in String) return Name_Array is
      It     : PropertyMap.Cursor := Self.Content.First;
      Result : Name_Array (1 .. Integer (Self.Content.Length));
      Pos    : Natural := 1;
   begin
      while PropertyMap.Has_Element (It) loop
         declare
            Key : constant Value := PropertyMap.Key (It);
         begin
            if Prefix'Length = 0 or else Index (Key, Prefix) = 1 then
               Result (Pos) := Key;
               Pos := Pos + 1;
            end if;
         end;
         It := PropertyMap.Next (It);
      end loop;
      return Result (Result'First .. Pos - 1);
   end Get_Names;

end Util.Properties.Hash;
