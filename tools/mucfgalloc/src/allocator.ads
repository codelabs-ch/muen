--
--  Copyright (C) 2014  Alexander Senier <mail@senier.net>
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package Allocator
is

   --  Allocate memory regions in given input policy and write result to
   --  specified output file.
   procedure Write
     (Input_Policy : Muxml.XML_Data_Type;
      Output_File  : String);

   --  Physical memory regions in the config overlapped each other.
   Overlapping_Physical_Memory : exception;

   --  Internal error (e.g. malformed XML input)
   Internal_Error : exception;

   --  Tried to allocate region twice
   Duplicate_Region : exception;

private

   procedure Add_Device_Regions
      (Policy :        Muxml.XML_Data_Type;
       Map    : in out Alloc.Map.Map_Type);

   procedure Add_Empty_Regions
      (Policy :        Muxml.XML_Data_Type;
       Map    : in out Alloc.Map.Map_Type);

   procedure Add_Fixed_Regions
      (Policy :        Muxml.XML_Data_Type;
       Map    : in out Alloc.Map.Map_Type);

   procedure Allocate_Variable_Regions
      (Policy :        Muxml.XML_Data_Type;
       Path   :        String;
       Map    : in out Alloc.Map.Map_Type);

   procedure Allocate_Variable_Empty_Regions
      (Policy :        Muxml.XML_Data_Type;
       Map    : in out Alloc.Map.Map_Type);

   procedure Allocate_Variable_File_Regions
      (Policy :        Muxml.XML_Data_Type;
       Map    : in out Alloc.Map.Map_Type);

   procedure Allocate_Variable_Fill_Regions
     (Policy :        Muxml.XML_Data_Type;
      Map    : in out Alloc.Map.Map_Type);

   type Region_Type is
   record
      Alignment : Interfaces.Unsigned_64;
      Size      : Interfaces.Unsigned_64;
      Name      : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   function "<" (Left, Right : Region_Type) return Boolean;

end Allocator;
