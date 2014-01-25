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
use type Interfaces.Unsigned_64;

package Alloc.Map
is

   type Map_Type is tagged private;

   type Region_Kind is (Empty, Allocated);

   type Region_Type is
   record
      Kind          : Region_Kind;
      First_Address : Interfaces.Unsigned_64;
      Last_Address  : Interfaces.Unsigned_64;
   end record;

   --  Insert an empty region to memory map
   procedure Insert_Empty_Region
      (Map           : in out Map_Type;
       First_Address :        Interfaces.Unsigned_64;
       Last_Address  :        Interfaces.Unsigned_64) with
      Pre => First_Address < Last_Address;

   --  Allocat a fixed region
   procedure Allocate_Fixed
      (Map           : in out Map_Type;
       First_Address :        Interfaces.Unsigned_64;
       Last_Address  :        Interfaces.Unsigned_64) with
      Pre => First_Address < Last_Address;

   --  Insert an empty region to memory map
   procedure Iterate
      (Map     : Map_Type;
       Process : not null access procedure (Region : Region_Type));

   Overlapping_Empty_Region : exception;

private

   package Region_List_Package is new
      Ada.Containers.Doubly_Linked_Lists (Element_Type => Region_Type);

   type Map_Type is tagged
   record
      Data : Region_List_Package.List;
   end record;

end Alloc.Map;
