--
--  Copyright (C) 2017  secunet Security Networks AG
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

with Bfd;

package Bin_Split.Types
is

   --  Contains information on a binary section.
   type Section_Info is record
      --  Name of section
      Name          : Ada.Strings.Unbounded.Unbounded_String;
      --  True if the section is to be extracted and written to disk
      Write_To_File : Boolean;
      --  The BFD section flags we expect from this section
      Flags         : Bfd.Section_Flags;
      --  Properties of the section information to be written to the output
      --  component specification
      Fill          : Boolean;
      Writable      : Boolean;
      Executable    : Boolean;
   end record;

   type SI_Array is array (Positive range <>) of Section_Info;

end Bin_Split.Types;
