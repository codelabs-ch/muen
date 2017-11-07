--
--  Copyright (C) 2017  secunet Security Networks AG
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

with Muxml;

package Bin_Split.Spec
is

   --  Add an entry to a component spec, corresponding to a section to
   --  be filled with a bit pattern.
   --
   --  `Spec'            : Component specification
   --  `Logical'         : Section name
   --  `Writable'        : Whether the section is writable
   --  `Executable'      : Whether the section is executable
   --  `Fill_Pattern'    : (Optional) Bit pattern the section is to be filled
   --                      with
   --  `Size'            : Size of the section
   --  `Virtual_Address' : Address of the section
   procedure Add_Fill_Entry
     (Spec            : in out Muxml.XML_Data_Type;
      Logical         :        String;
      Writable        :        Boolean;
      Executable      :        Boolean;
      Fill_Pattern    :        Interfaces.Unsigned_64 := 0;
      Size            :        Interfaces.Unsigned_64;
      Virtual_Address :        Interfaces.Unsigned_64);

   --  Add an entry to a component spec, corresponding to a section to
   --  be read from a binary file.
   --
   --  `Spec'            : Component specification
   --  `Logical'         : Section name
   --  `Writable'        : Whether the section is writable
   --  `Executable'      : Whether the section is executable
   --  `File_Name'       : The filename of the binary containing the section
   --  `Hash'            : (Optional) hash value of the binary's contents
   --  `Size'            : Size of the section
   --  `Virtual_Address' : Address of the section
   procedure Add_File_Entry
     (Spec            : in out Muxml.XML_Data_Type;
      Logical         :        String;
      Writable        :        Boolean;
      Executable      :        Boolean;
      File_Name       :        String;
      Hash            :        String := "";
      Size            :        Interfaces.Unsigned_64;
      Virtual_Address :        Interfaces.Unsigned_64);

end Bin_Split.Spec;
