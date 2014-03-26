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

with Mutools.Immutable_Processors;

pragma Elaborate_All (Mutools.Immutable_Processors);

package body Pack.Post_Checks
is

   package Check_Procs is new
     Mutools.Immutable_Processors (Param_Type => Content_Providers.Param_Type);

   -------------------------------------------------------------------------

   function Get_Count return Natural renames Check_Procs.Get_Count;

   -------------------------------------------------------------------------

   procedure Register_All
   is
   begin
      null;
   end Register_All;

   -------------------------------------------------------------------------

   procedure Run (Data : Content_Providers.Param_Type) renames Check_Procs.Run;

end Pack.Post_Checks;
