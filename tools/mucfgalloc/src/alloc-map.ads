--
--  Copyright (C) 2014  Alexander Senier <mail@senier.net>
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

with Interfaces;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;
use type Interfaces.Unsigned_64;

package Alloc.Map
is

   type Map_Type is tagged private;

   type Region_Kind is (Any, Empty, Fixed, Allocated, Device);

   type Region_Type is
   record
      Kind          : Region_Kind;
      First_Address : Interfaces.Unsigned_64;
      Last_Address  : Interfaces.Unsigned_64;
      Name          : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   --  Insert a device region to memory map
   procedure Insert_Device_Region
      (Map           : in out Map_Type;
       Name          :        Ada.Strings.Unbounded.Unbounded_String;
       First_Address :        Interfaces.Unsigned_64;
       Last_Address  :        Interfaces.Unsigned_64) with
      Pre => First_Address < Last_Address;

   --  Insert an empty region to memory map
   procedure Insert_Empty_Region
      (Map           : in out Map_Type;
       Name          :        Ada.Strings.Unbounded.Unbounded_String;
       First_Address :        Interfaces.Unsigned_64;
       Last_Address  :        Interfaces.Unsigned_64) with
      Pre => First_Address < Last_Address;

   --  Allocat a fixed region
   procedure Allocate_Fixed
      (Map           : in out Map_Type;
       Name          :        Ada.Strings.Unbounded.Unbounded_String;
       First_Address :        Interfaces.Unsigned_64;
       Last_Address  :        Interfaces.Unsigned_64) with
      Pre => First_Address < Last_Address;

   --  Allocate region below Upper_Limit using Size and Alignment
   procedure Allocate_Variable
      (Map         : in out Map_Type;
       Name        :        Ada.Strings.Unbounded.Unbounded_String;
       Size        :        Interfaces.Unsigned_64;
       Upper_Limit :        Interfaces.Unsigned_64 :=
                              Interfaces.Unsigned_64'Last;
       Alignment   :        Interfaces.Unsigned_64 := 1) with
      Pre => 0 < Size and 0 < Alignment;

   --  Insert an empty region to memory map
   procedure Iterate
      (Map     : Map_Type;
       Process : not null access procedure (Region : Region_Type);
       Filter  : Region_Kind := Any);

   Overlapping_Empty_Region : exception;
   Invalid_Fixed_Allocation : exception;
   Out_Of_Memory            : exception;
   Limit_Exceeded           : exception;

private

   package Region_List_Package is new
      Ada.Containers.Doubly_Linked_Lists (Element_Type => Region_Type);

   type Map_Type is tagged
   record
      Data : Region_List_Package.List;
   end record;

   procedure Reserve
      (Map           : in out Map_Type;
       Kind          :        Region_Kind;
       Curr          :        Region_List_Package.Cursor;
       Name          :        Ada.Strings.Unbounded.Unbounded_String;
       First_Address :        Interfaces.Unsigned_64;
       Last_Address  :        Interfaces.Unsigned_64);

   procedure Insert_New_Region
      (Map           : in out Map_Type;
       Name          :        Ada.Strings.Unbounded.Unbounded_String;
       Kind          :        Region_Kind;
       First_Address :        Interfaces.Unsigned_64;
       Last_Address  :        Interfaces.Unsigned_64);

end Alloc.Map;
