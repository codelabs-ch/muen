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

package body Pack.Command_Line.Test
is

   use Ada.Strings.Unbounded;

   -------------------------------------------------------------------------

   procedure Set_Input_Dir (Path : String)
   is
   begin
      Input_Dir := To_Unbounded_String (Path);
   end Set_Input_Dir;

   -------------------------------------------------------------------------

   procedure Set_Output_Dir (Path : String)
   is
   begin
      Output_Dir := To_Unbounded_String (Path);
   end Set_Output_Dir;

   -------------------------------------------------------------------------

   procedure Set_Policy (Path : String)
   is
   begin
      Policy := To_Unbounded_String (Path);
   end Set_Policy;

end Pack.Command_Line.Test;
