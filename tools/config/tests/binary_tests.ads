--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ahven.Framework;

package Binary_Tests
is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   --  Initialize testcase.
   procedure Initialize (T : in out Testcase);

   --  Read information from a binary file.
   procedure Read_Binary;

   --  Try to read a nonexistent file.
   procedure Read_Nonexistent_File;

   --  Try to read non-binary file.
   procedure Read_Invalid_File;

   --  Try to read a binary file which contains no symbols.
   procedure Read_No_Symbols;

   --  Try to read a binary file which contains no global symbols.
   procedure Read_No_Global_Symbols;

   --  Try to read a binary file which contains undefined symbols.
   procedure Read_Undefined_Symbols;

   --  Write binary XML specification.
   procedure Write_Binary_Spec;

   --  Write memory layout XML specification.
   procedure Write_Memlayout_Spec;

end Binary_Tests;
