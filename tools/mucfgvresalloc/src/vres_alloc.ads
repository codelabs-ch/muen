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

with Ada.Strings.Hash;
with Ada.Containers.Indefinite_Hashed_Maps;

with Interfaces;

with Mutools;

private with Mutools.Expressions;
private with Mutools.Intervals;
private with Mutools.Vres_Alloc.Config;

package Vres_Alloc
is
   type Address_And_Size_Type is
   record
      First_Address : Interfaces.Unsigned_64;
      Size          : Interfaces.Unsigned_64;
   end record;

   package Logical_To_Interval_Package is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type        => String,
       Element_Type    => Address_And_Size_Type,
       Hash            => Ada.Strings.Hash,
       Equivalent_Keys => "=");

   type Component_Info_Type is
   record
      Profile           : Mutools.String_Holder_Type.Holder;
      Va_Map            : aliased Logical_To_Interval_Package.Map;
      Reader_Events_Map : aliased Logical_To_Interval_Package.Map;
      Writer_Events_Map : aliased Logical_To_Interval_Package.Map;
   end record;

   --  The main procedure
   procedure Run
     (Policy_File_Name : String;
      Output_File_Name : String);

   Validation_Error : exception;

private
   package Component_To_Map_Package is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type        => String,
       Element_Type    => Component_Info_Type,
       Hash            => Ada.Strings.Hash,
       Equivalent_Keys => "=");

   Memory_Sizes   : Mutools.Expressions.Name_To_String_Hashed_Map.Map;
   Channel_Sizes  : Mutools.Expressions.Name_To_String_Hashed_Map.Map;
   Components_Map : Component_To_Map_Package.Map;

   --  The default domains are copied to local variables
   --  in order to be able to change them in unittests.
   Va_Space_Native       : Mutools.Intervals.Interval_Type
     := Mutools.Vres_Alloc.Config.Default_Va_Space_Native;
   Va_Space_Vm           : Mutools.Intervals.Interval_Type
     := Mutools.Vres_Alloc.Config.Default_Va_Space_Vm;
   Vector_Numbers_Domain : Mutools.Intervals.Interval_Type
     := Mutools.Vres_Alloc.Config.Default_Vector_Numbers_Domain;
   Event_Numbers_Domain  : Mutools.Intervals.Interval_Type
     := Mutools.Vres_Alloc.Config.Default_Event_Numbers_Domain;
end Vres_Alloc;
