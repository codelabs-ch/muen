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

with Binary;

package Types is

   --  Contains information on a binary section.
   type Section_Info is record
      Name          : Ada.Strings.Unbounded.Unbounded_String;
      Write_To_File : Boolean;
      Flags         : Binary.Section_Flags;
   end record;

   type SI_Array is array (Positive range <>) of Section_Info;

   --  Several sections may be combined into a compound section.
   type Compound_Section_Info is record
      Infos             : access SI_Array;
      Fill              : Boolean;
      Writable          : Boolean;
      Executable        : Boolean;
   end record;

   type CSI_Array is array (Positive range <>) of Compound_Section_Info;

   Bin_Split_Error : exception;

end Types;
