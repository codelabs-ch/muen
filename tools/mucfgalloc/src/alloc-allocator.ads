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

with Muxml;
private with Alloc.Map;
private with Interfaces;
private with Ada.Strings.Unbounded;

package Alloc.Allocator
is
   --  Write fully allocated configuration to given output directory.
   procedure Write
     (Output_File : String;
      Policy      : Muxml.XML_Data_Type);

   --  Physical memory regions in the config overlapped each other.
   Overlapping_Physical_Memory : exception;

   --  Internal error (e.g. malformed XML input)
   Internal_Error : exception;

   --  Tried to allocate region twice
   Duplicate_Region : exception;

private

   procedure Add_Empty_Regions
      (Policy      :        Muxml.XML_Data_Type;
       Map         : in out Alloc.Map.Map_Type);

   procedure Add_Fixed_Regions
      (Policy      :        Muxml.XML_Data_Type;
       Map         : in out Alloc.Map.Map_Type);

   procedure Allocate_Variable_Regions
      (Policy      :        Muxml.XML_Data_Type;
       Map         : in out Alloc.Map.Map_Type);

   type Region_Type is
   record
      Alignment   : Interfaces.Unsigned_64;
      Size        : Interfaces.Unsigned_64;
      Upper_Limit : Interfaces.Unsigned_64;
      Name        : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   function "<" (Left, Right : Region_Type) return Boolean;

end Alloc.Allocator;
