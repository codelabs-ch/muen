--
--  Copyright (C) 2014  Alexander Senier <mail@senier.net>
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;
with Interfaces;
use type Interfaces.Unsigned_64;

with DOM.Core;
use type DOM.Core.Node;

with Mutools;

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
      Allocatable   : Boolean;
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
       Allocatable   :        Boolean;
       First_Address :        Interfaces.Unsigned_64;
       Last_Address  :        Interfaces.Unsigned_64) with
      Pre => First_Address < Last_Address;

   --  Allocate a fixed region
   procedure Allocate_Fixed
      (Map           : in out Map_Type;
       Name          :        Ada.Strings.Unbounded.Unbounded_String;
       First_Address :        Interfaces.Unsigned_64;
       Last_Address  :        Interfaces.Unsigned_64) with
      Pre => First_Address < Last_Address;

   --  Allocate region below Upper_Limit using Size and Alignment
   procedure Allocate_Variable
      (Map       : in out Map_Type;
       Name      :        Ada.Strings.Unbounded.Unbounded_String;
       Size      :        Interfaces.Unsigned_64;
       Alignment :        Interfaces.Unsigned_64 := 1) with
      Pre => 0 < Size and 0 < Alignment;

   --  Invoke Process on all regions in Map matching Filter. If Filter is set
   --  to 'Any', all regions are matched. The specified kind is matched
   --  otherwise.
   procedure Iterate
      (Map     : Map_Type;
       Process : not null access procedure (Region : Region_Type);
       Filter  : Region_Kind := Any);

   --  Return region with specified name from given map. An exception is raised
   --  if no region with the given name exists.
   function Get_Region
     (Map  : Map_Type;
      Name : String)
      return Region_Type;

   --  Clear memory map.
   procedure Clear (Map : in out Map_Type);

   Overlapping_Empty_Region : exception;
   Invalid_Fixed_Allocation : exception;
   Out_Of_Memory            : exception;
   No_Region                : exception;
   Invalid_Region           : exception;

   -------------------------------------------------------------------------
   -- Procedures/functions for mucfgcvresalloc and mucfgvresalloc

   type Run_Type_Type is (VIRTUAL_ADDRESSES, WRITER_EVENTS, READER_EVENTS);

   --  Models a list of pairwise disjoint nonadjacent intervals to store
   --  regions within the virtuall address space.
   --  All intervals on the list have positive size.
   type VA_Regions_Type is tagged private;

   --  Models a single interval in the (virtual) address space
   type Memory_Interval_Type is
   record
      -- then intervall does *include* its first and last address
      First_Address : Interfaces.Unsigned_64;
      Last_Address  : Interfaces.Unsigned_64;
   end record;

   package Node_List_Package is new
     Ada.Containers.Doubly_Linked_Lists (Element_Type => DOM.Core.Node);

   --  Include the given interval in the set covered by all intervalls in List.
   --  This may include extensions of intervals, fusing multiple intervals into
   --  one or simply adding this interval to the list.
   procedure Add_Memory_Interval
     (List     : in out VA_Regions_Type;
      Interval :        Memory_Interval_Type);

   --  Exclude all elements of the given interval from the set covered by all
   --  intervals in List. Interval may have any position with respect to the
   --  intervals in the list.
   procedure Subtract_Memory_Interval
     (List     : in out VA_Regions_Type;
      Interval :        Memory_Interval_Type);

   --  Wrapper for Subtract_Memory_Interval
   procedure Subtract_Memory_Interval
     (List          : in out VA_Regions_Type;
      First_Address :        Interfaces.Unsigned_64;
      Size          :        Interfaces.Unsigned_64);

   --  Return an address A such that the intervall [A, A+size-1]
   --  is included in some interval in List. The intervall [A,A+size-1] is then
   --  subtracted from List, "reserving" that space.
   function Reserve_Memory
     (List : in out VA_Regions_Type;
      Size : Interfaces.Unsigned_64)
     return Interfaces.Unsigned_64;

   --  Delete all entries of List.
   procedure Clear (List : in out VA_Regions_Type);

   --  Depending on Run_Type, set the resource attribute of Node.
   --  A region of size Size is reserved in Av_Mem.
   --  If Size is not given, the 'size' attribute of Node is read instead.
   procedure Allocate_And_Set_Single_Resource
     (Av_Mem   : in out Alloc.Map.VA_Regions_Type;
      Node     :        DOM.Core.Node;
      Run_Type :        Run_Type_Type;
      Size     :        String := "");

   --  Prefix the entries of Target_List and join them with "|" into an XPath
   --  If Target_List is empty it returns an xpath that does not match any node.
   function Get_Target_String
     (Target_List : Mutools.String_Vector.Vector;
      Prefix      : String)
     return String;

   --  Return String representation in decimal format.
   function To_String (Number : Interfaces.Unsigned_64) return String;

   --  For Run_Type=VIRTUAL_ADDRESS the size attribute of the memory region
   --  (or of one element of the array) is returned. Otherwise, 1 is returend.
   function Get_Resource_Size
     (Elem     : DOM.Core.Node;
      Run_Type : Alloc.Map.Run_Type_Type)
     return Interfaces.Unsigned_64;

   --  Returns the value of "virtualAddress", "vector", "event",
   --  "virtualAddressBase", "vectorBase", "eventBase" or "id"
   --  attributes, depending on Run_Type and the tag of the node.
   --  If the needed attribute is not found, then "" is returned.
   function Get_Resource_Value
     (Elem     : DOM.Core.Node;
      Run_Type : Alloc.Map.Run_Type_Type)
     return String;

   --  Depending on Run_Type, set the attribute "virtualAddress", "event" or
   --  "vector" of Node to Value.
   procedure Set_Virtual_Resource
     (Node     : DOM.Core.Node;
      Run_Type : Alloc.Map.Run_Type_Type;
      Value    : Interfaces.Unsigned_64);

   Validation_Error : exception;

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
       Allocatable   :        Boolean;
       Kind          :        Region_Kind;
       First_Address :        Interfaces.Unsigned_64;
       Last_Address  :        Interfaces.Unsigned_64);

   -------------------------------------------------------------------------
   -- Procedures/functions for mucfgcvresalloc and mucfgvresalloc

   package Memory_Intervals_Package is new
     Ada.Containers.Doubly_Linked_Lists (Element_Type => Memory_Interval_Type);

   type VA_Regions_Type is tagged
   record
      Data : Memory_Intervals_Package.List;
   end record;

end Alloc.Map;
