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

package Zp.Generator
is

   --  Write Linux bootparams structure and specified command line to file
   --  given by filename. The size of the generated file is 4k + length (cmdl).
   --  The physical address argument designates the physical address of the
   --  zero-page in guest memory.
   procedure Write
     (Filename         : String;
      Cmdline          : String;
      Physical_Address : Natural);

   IO_Error : exception;

end Zp.Generator;
