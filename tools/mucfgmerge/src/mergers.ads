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

with Muxml;

package Mergers
is

   --  Merge given config document with specified system policy.
   procedure Merge_Config
     (Policy : in out Muxml.XML_Data_Type;
      Config :        Muxml.XML_Data_Type);

   --  Load hardware section from specified file and merge with given policy.
   procedure Merge_Hardware
     (Policy        : in out Muxml.XML_Data_Type;
      Hardware_File :        String;
      Add_Location  :        Boolean);

   --  Load platform section from specified file and merge with given policy.
   procedure Merge_Platform
     (Policy        : in out Muxml.XML_Data_Type;
      Platform_File :        String;
      Add_Location  :        Boolean);

   --  Merge platform config into global config section of given policy. The
   --  platform config section is removed after the merge operation.
   procedure Merge_Platform_Config (Policy : in out Muxml.XML_Data_Type);

end Mergers;
