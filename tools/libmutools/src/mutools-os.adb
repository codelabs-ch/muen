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

with GNAT.OS_Lib;

package body Mutools.OS
is

   Shell : constant String := "/bin/bash";

   -------------------------------------------------------------------------

   procedure Execute (Command : String)
   is
      Status : Boolean;
      Args   : GNAT.OS_Lib.Argument_List (1 .. 5);
   begin
      Args (1) := new String'(Shell);
      Args (2) := new String'("-o");
      Args (3) := new String'("pipefail");
      Args (4) := new String'("-c");
      Args (5) := new String'(Command);

      GNAT.OS_Lib.Spawn (Program_Name => Args (Args'First).all,
                         Args         => Args (Args'First + 1 .. Args'Last),
                         Success      => Status);
      for A in Args'Range loop
         GNAT.OS_Lib.Free (X => Args (A));
      end loop;

      if not Status then
         raise Command_Failed with "Command '" & Command & "' failed";
      end if;
   end Execute;

end Mutools.OS;
