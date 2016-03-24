--
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

with Ada.Strings.Unbounded;

with Interfaces;

package Pack.Manifest
is

   use type Interfaces.Unsigned_64;

   --  System image manifest type.
   type Manifest_Type is private;

   --  Add manifest entry with given attributes.
   procedure Add_Entry
     (Manifest     : in out Manifest_Type;
      Mem_Name     :        String;
      Mem_Type     :        String;
      Content      :        String;
      Address      :        Interfaces.Unsigned_64;
      Memory_Size  :        Interfaces.Unsigned_64;
      Content_Size :        Interfaces.Unsigned_64;
      Offset       :        Interfaces.Unsigned_64)
     with
       Pre => Memory_Size >= Content_Size;

   --  Write manifest to file given by filename.
   procedure Write
     (Manifest : Manifest_Type;
      Filename : String);

private

   type Manifest_Type is record
      Data : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end Pack.Manifest;
