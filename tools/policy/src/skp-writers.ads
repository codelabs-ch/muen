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

package Skp.Writers
is

   --  Write kernel-related policy files to directory specified by name.
   procedure Write_Kernel
     (Dir_Name : String;
      Policy   : Policy_Type);

   --  Write subjects-related policy files to directory specified by name.
   procedure Write_Subjects
     (Dir_Name : String;
      Policy   : Policy_Type);

   --  Write binary information files to directory specified by name.
   procedure Write_Binaries
     (Dir_Name : String;
      Policy   : Policy_Type);

   --  Write system-related policy files to directory specified by name.
   procedure Write_System
     (Dir_Name : String;
      Policy   : Policy_Type);

   --  Write hardware-related policy files to directory specified by name.
   procedure Write_Hardware
     (Dir_Name : String;
      Policy   : Policy_Type);

   --  Write scheduling-related policy files to directory specified by name.
   procedure Write_Scheduling
     (Dir_Name : String;
      Policy   : Policy_Type);

   --  Write interrupt policy to directory specified by name.
   procedure Write_Interrupts
     (Dir_Name : String;
      Policy   : Policy_Type);

end Skp.Writers;
