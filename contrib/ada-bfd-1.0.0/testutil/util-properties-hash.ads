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

with Ada.Strings.Unbounded;
with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;
private package Util.Properties.Hash is

   type Manager is new Util.Properties.Interface_P.Manager with private;
   type Manager_Access is access all Manager'Class;

   --  Returns TRUE if the property exists.
   function Exists (Self : in Manager; Name : in Value)
                    return Boolean;

   --  Returns the property value.  Raises an exception if not found.
   function Get (Self : in Manager; Name : in Value)
                 return Value;

   procedure Insert (Self : in out Manager; Name : in Value;
                     Item : in Value);

   --  Set the value of the property.  The property is created if it
   --  does not exists.
   procedure Set (Self : in out Manager; Name : in Value;
                  Item : in Value);

   --  Remove the property given its name.
   procedure Remove (Self : in out Manager; Name : in Value);

   --  Iterate over the properties and execute the given procedure passing the
   --  property name and its value.
   procedure Iterate (Self    : in Manager;
                      Process : access procedure (Name, Item : Value));

   --  Deep copy of properties stored in 'From' to 'To'.
   function Create_Copy (Self : in Manager) return Interface_P.Manager_Access;

   procedure Delete (Self : in Manager; Obj : in out Interface_P.Manager_Access);

   function Get_Names (Self   : in Manager;
                       Prefix : in String) return Name_Array;

private

   function Equivalent_Keys (Left : Value; Right : Value)
                            return Boolean;

   package PropertyMap is
     new Ada.Containers.Hashed_Maps
       (Element_Type    => Value,
        Key_Type        => Value,
        Hash            => Ada.Strings.Unbounded.Hash,
        Equivalent_Keys => Equivalent_Keys,
        "="             => Ada.Strings.Unbounded."=");

   type Manager is new Util.Properties.Interface_P.Manager with record
      Content : PropertyMap.Map;
   end record;

end Util.Properties.Hash;
